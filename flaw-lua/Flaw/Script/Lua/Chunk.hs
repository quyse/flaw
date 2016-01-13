{-|
Module: Flaw.Script.Lua.Chunk
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Script.Lua.Chunk
	( compileLuaChunk
	) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.HashTable.IO as HT
import Data.IORef
import Data.Maybe
import qualified Data.Serialize as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Unique
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Script.Lua
import Flaw.Script.Lua.FFI
import Flaw.Script.Lua.Operations

-- | Chunk header, encapsulating machine-specific properties
-- such as endianness or size of int.
luaChunkHeader :: B.ByteString
luaChunkHeader = S.runPut $ do
	-- signature
	S.putByteString LUA_SIGNATURE
	-- version, calculated as major * 16 + minor
	S.putWord8 $ 5 * 16 + 3
	-- format, always 0
	S.putWord8 0
	-- data
	S.putByteString LUAC_DATA
	-- size of int
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: CInt)
	-- size of size_t
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: CIntPtr)
	-- size of instruction
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: Word32)
	-- size of lua_Integer
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: Int)
	-- size of lua_Number
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: Double)
	-- test lua_Integer
	S.putWordhost LUAC_INT
	-- test lua_Number
	S.putFloat64le LUAC_NUM

data LuaProto = LuaProto
	{ luaProtoSource :: !B.ByteString
	, luaProtoLineDefined :: {-# UNPACK #-} !Int
	, luaProtoLastLineDefined :: {-# UNPACK #-} !Int
	, luaProtoNumParams :: {-# UNPACK #-} !Int
	, luaProtoIsVararg :: !Bool
	, luaProtoMaxStackSize :: {-# UNPACK #-} !Int
	, luaProtoOpcodes :: !(VU.Vector Word32)
	, luaProtoConstants :: !(V.Vector ExpQ)
	, luaProtoFunctions :: !(V.Vector LuaProto)
	, luaProtoUpvalues :: !(V.Vector (Bool, Int))
	}

-- | Compile Lua chunk.
compileLuaChunk :: B.ByteString -> ExpQ
compileLuaChunk bytes = do

	-- parse
	runIO $ B.writeFile "header.luab" luaChunkHeader
	let eitherProto = flip S.runGet bytes $ do
		chunkHeader <- S.getByteString (B.length luaChunkHeader)
		when (chunkHeader /= luaChunkHeader) $ fail "wrong Lua chunk header"

		let getInt = liftM fromIntegral S.getWord32le :: S.Get Int

		let getString = do
			b <- S.getWord8
			size <- if b == 0xff then liftM fromIntegral S.getWordhost else return $ fromIntegral b
			if size == 0 then return B.empty
			else S.getByteString $ size - 1

		_chunkUpvaluesCount <- S.getWord8

		let loadFunction = do
			source <- getString
			lineDefined <- getInt
			lastLineDefined <- getInt
			numParams <- liftM fromIntegral S.getWord8
			isVararg <- liftM ( > 0) S.getWord8
			maxStackSize <- liftM fromIntegral S.getWord8

			-- opcodes
			opcodesCount <- getInt
			opcodes <- VU.replicateM opcodesCount S.getWord32le

			-- constants
			constantsCount <- getInt
			constants <- V.replicateM constantsCount $ do
				t <- S.getWord8
				let getStringConstant = do
					s <- getString
					return [| LuaString $ fromString $(litE $ stringL $ T.unpack $ T.decodeUtf8 s) |]
				case t of
					LUA_TNIL -> return [| LuaNil |]
					LUA_TBOOLEAN -> do
						b <- liftM ( > 0) S.getWord8
						return [| LuaBoolean b |]
					LUA_TNUMFLT -> do
						n <- S.getFloat64le
						return [| LuaReal n |]
					LUA_TNUMINT -> do
						n <- liftM fromIntegral S.getWordhost :: S.Get Int
						return [| LuaInteger n |]
					LUA_TSHRSTR -> getStringConstant
					LUA_TLNGSTR -> getStringConstant
					_ -> fail "wrong Lua constant"

			-- upvalues
			upvaluesCount <- getInt
			upvalues <- V.replicateM upvaluesCount $ do
				instack <- S.getWord8
				idx <- S.getWord8
				return (instack > 0, fromIntegral idx)

			-- subfunctions
			functionsCount <- getInt
			functions <- V.replicateM functionsCount loadFunction

			-- debug info
			debugLineInfoCount <- getInt
			_debugLineInfo <- V.replicateM debugLineInfoCount getInt
			debugLocVarsCount <- getInt
			_debugLocVars <- V.replicateM debugLocVarsCount $ do
				_varName <- getString
				_startPc <- getInt
				_endPc <- getInt
				return ()
			debugUpvalueNamesCount <- getInt
			_debugUpvalueNames <- V.replicateM debugUpvalueNamesCount getString

			return LuaProto
				{ luaProtoSource = source
				, luaProtoLineDefined = lineDefined
				, luaProtoLastLineDefined = lastLineDefined
				, luaProtoNumParams = numParams
				, luaProtoIsVararg = isVararg
				, luaProtoMaxStackSize = maxStackSize
				, luaProtoOpcodes = opcodes
				, luaProtoConstants = constants
				, luaProtoUpvalues = upvalues
				, luaProtoFunctions = functions
				}

		loadFunction

	mainProto <- case eitherProto of
		Left err -> fail err
		Right proto -> return proto

	env <- newName "e"
	lamE [varP env] $ compileLuaFunction mainProto (V.singleton $ varE env) V.empty

-- | Internal representation of instruction.
data LuaInst = LuaInst
	-- | Indices of instructions this instruction refers to.
	[Int]
	-- ^ Function accepting list of instructions (corresponding to indices), and returning this instruction.
	([[StmtQ]] -> Q [StmtQ])

-- | Compile Lua function.
compileLuaFunction :: LuaProto -> V.Vector ExpQ -> V.Vector ExpQ -> ExpQ
compileLuaFunction LuaProto
	{ luaProtoNumParams = numParams
	, luaProtoIsVararg = isVararg
	, luaProtoMaxStackSize = maxStackSize
	, luaProtoOpcodes = opcodes
	, luaProtoConstants = constants
	, luaProtoUpvalues = protoUpvalues
	, luaProtoFunctions = functions
	} parentStack parentUpvalues = do

	-- upvalues
	upvaluesNames <- V.generateM (V.length protoUpvalues) $ \i -> newName $ "u" ++ show i
	let upvaluesStmt = letS $ V.toList $ V.generate (V.length protoUpvalues) $ \i -> let
		(instack, idx) = protoUpvalues V.! i
		upvalueValue = if instack then parentStack V.! idx else parentUpvalues V.! idx
		in valD (varP (upvaluesNames V.! i)) (normalB upvalueValue) []
	let upvalues = V.map varE upvaluesNames

	-- stack
	stackNames <- V.generateM maxStackSize $ \i -> newName $ "s" ++ show i
	let stackStmts = V.toList $ V.generate maxStackSize $ \i ->
		bindS (varP (stackNames V.! i)) [| newIORef LuaNil |]
	let stack = V.generate maxStackSize $ \i -> varE $ stackNames V.! i

	-- arguments & vararg
	varargName <- newName "va"
	let varargStmts = if isVararg then [bindS (varP varargName) [| newIORef [] |] ] else []
	let argsSetStmts i =
		if i < numParams then do
			a <- newName "a"
			x <- newName "x"
			(restStmts, xs) <- argsSetStmts $ i + 1
			let e = doE $ (noBindS [| writeIORef $(stack V.! i) $(varE x) |]) : restStmts
			return
				( [ noBindS $ caseE (varE a)
						[ match [p| $(varP x) : $xs |] (normalB e) []
						, match [p| [] |] (normalB [| return () |]) []
						]
					]
				, varP a
				)
		else if isVararg then do
			a <- newName "a"
			return
				( [ noBindS [| writeIORef $(varE varargName) $(varE a) |]
					]
				, varP a
				)
		else return ([], wildP)
	(argsStmts, argsPat) <- argsSetStmts 0

	-- subfunctions
	functionsNames <- V.generateM (V.length functions) $ \i -> newName $ "f" ++ show i
	let functionsStmt = letS $ V.toList $ V.generate (V.length functions) $ \i ->
		valD (varP (functionsNames V.! i)) (normalB $ compileLuaFunction (functions V.! i) stack upvalues) []

	-- instructions
	let instructions = V.generate (VU.length opcodes) $ \i -> let
		-- instruction word
		x = opcodes VU.! i
		-- all possible parameters
		a = fromIntegral $ (x `shiftR` 6) .&. (bit 8 - 1)
		c = fromIntegral $ (x `shiftR` 14) .&. (bit 9 - 1)
		b = fromIntegral $ (x `shiftR` 23) .&. (bit 9 - 1)
		bx = fromIntegral $ (x `shiftR` 14) .&. (bit 18 - 1)
		sbx = bx - (bit 17 - 1)
		kbx = constants V.! bx

		-- helper functions
		kst j = constants V.! j -- :: LuaValue
		r j = stack V.! j -- :: IORef LuaValue
		rk j = -- :: IO LuaValue
			if j `testBit` 8 then [| return $(kst (j `clearBit` 8)) |]
			else [| readIORef $(r j) |]
		u j = upvalues V.! j -- :: IORef LuaValue
		binop op = normalFlow [| do
			p <- $(rk b)
			q <- $(rk c)
			writeIORef $(r a) =<< $op p q
			|]
		unop op = normalFlow [| writeIORef $(r a) =<< $op =<< readIORef $(r b) |]

		-- next and next-after-next instruction ids
		nextInstId = i + 1
		nextNextInstId = i + 2
		-- append next instruction
		normalFlow e = LuaInst [nextInstId] $ \[nextInstStmts] -> return $ (noBindS e) : nextInstStmts
		-- conditional operation
		condbinop op = LuaInst [nextInstId, nextNextInstId] $ \[nextInstStmts, nextNextInstStmts] -> return [noBindS [| do
			p <- $(rk b)
			q <- $(rk c)
			z <- $op p q
			$(if a > 0 then
				[| if luaCoerceToBool z then $(doE nextInstStmts) else $(doE nextNextInstStmts) |]
				else
				[| if luaCoerceToBool z then $(doE nextNextInstStmts) else $(doE nextInstStmts) |])
			|] ]
		-- call operation
		callop args rets = do
			(callArgsStmts, callArgsNames) <- liftM unzip $ forM args $ \j -> do
				n <- newName $ "a" ++ show j
				return (bindS (varP n) [| readIORef $(r j) |], n)
			f <- newName "f"
			let
				adjustRets (j:js) = do
					q <- newName $ "q" ++ show j
					z <- newName $ "z" ++ show j
					(restStmts, restP) <- adjustRets js
					return
						( [ noBindS $ caseE (varE q)
								[ match [p| $(varP z) : $restP |] (normalB $ doE $
									(noBindS [| writeIORef $(r j) $(varE z) |]) : restStmts) []
								, match [p| [] |] (normalB [| return () |]) []
								]
							]
						, varP q
						)
				adjustRets [] = return ([], wildP)
			let callE = [| luaValueCall $(varE f) $(listE $ map varE callArgsNames) |]
			retsAndCallStmts <- if null rets then return [noBindS [| void $callE |] ]
				else do
					(retsStmts, retPat) <- adjustRets rets
					return $ (bindS retPat callE) : retsStmts
			doE $ (bindS (varP f) [| readIORef $(r a) |]) : callArgsStmts ++ retsAndCallStmts
		-- get ax from extra arg
		extraArg = do
			let nx = opcodes VU.! (i + 1)
			when ((nx .&. (bit 6 - 1)) /= OP_EXTRAARG) $ fail "flaw-lua: opcode must be followed by OP_EXTRAARG"
			return $ fromIntegral $ (nx `shiftR` 6) .&. (bit 26 - 1)

		-- choose by instruction
		in case x .&. (bit 6 - 1) of
			OP_MOVE -> normalFlow [| writeIORef $(r a) =<< readIORef $(r b) |]
			OP_LOADK -> normalFlow [| writeIORef $(r a) $kbx |]
			OP_LOADKX -> LuaInst [nextNextInstId] $ \[nextNextInstStmts] -> do
				nax <- extraArg
				return $ (noBindS [| writeIORef $(r a) $(kst nax) |]) : nextNextInstStmts
			OP_LOADBOOL -> LuaInst [if c > 0 then nextNextInstId else nextInstId] $ \[followingInstStmts] -> return $
				(noBindS [| writeIORef $(r a) $ LuaBoolean $(conE $ if b > 0 then 'True else 'False) |]) : followingInstStmts
			OP_LOADNIL -> normalFlow $ doE $ flip map [a .. (a + b)] $ \j -> noBindS [| writeIORef $(r j) LuaNil |]
			OP_GETUPVAL -> normalFlow [| writeIORef $(r a) =<< readIORef $(u b) |]
			OP_GETTABUP -> normalFlow [| do
				LuaTable { luaTable = t } <- readIORef $(u b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				|]
			OP_GETTABLE -> normalFlow [| do
				LuaTable { luaTable = t } <- readIORef $(r b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				|]
			OP_SETTABUP -> normalFlow [| do
				LuaTable { luaTable = t } <- readIORef $(u a)
				q <- $(rk b)
				HT.insert t q =<< $(rk c)
				|]
			OP_SETUPVAL -> normalFlow [| writeIORef $(u b) =<< readIORef $(r a) |]
			OP_SETTABLE -> normalFlow [| do
				LuaTable { luaTable = t } <- readIORef $(r a)
				q <- $(rk b)
				HT.insert t q =<< $(rk c)
				|]
			OP_NEWTABLE -> normalFlow [| do
				q <- newUnique
				t <- HT.newSized $(litE $ integerL $ fromIntegral $ max b c)
				z <- newIORef LuaNil
				writeIORef $(r a) $ LuaTable
					{ luaTableUnique = q
					, luaTable = t
					, luaTableMetaTable = z
					}
				|]
			OP_SELF -> normalFlow [| do
				writeIORef $(r $ a + 1) =<< readIORef $(r b)
				LuaTable { luaTable = t } <- readIORef $(r b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				|]
			OP_ADD -> binop [| luaValueAdd |]
			OP_SUB -> binop [| luaValueSub |]
			OP_MUL -> binop [| luaValueMul |]
			OP_MOD -> binop [| luaValueMod |]
			OP_POW -> binop [| luaValuePow |]
			OP_DIV -> binop [| luaValueDiv |]
			OP_IDIV -> binop [| luaValueIDiv |]
			OP_BAND -> binop [| luaValueBAnd |]
			OP_BOR -> binop [| luaValueBOr |]
			OP_BXOR -> binop [| luaValueBXor |]
			OP_SHL -> binop [| luaValueShl |]
			OP_SHR -> binop [| luaValueShr |]
			OP_UNM -> unop [| luaValueUnm |]
			OP_BNOT -> unop [| luaValueBNot |]
			OP_NOT -> unop [| luaValueNot |]
			OP_LEN -> unop [| luaValueLen |]
			OP_CONCAT -> normalFlow $ do
				let
					f [] = return ([], [| LuaString T.empty |])
					f [j] = do
						p <- newName $ "p" ++ show j
						return ([bindS (varP p) [| readIORef $(r j) |] ], varE p)
					f (j:js) = do
						p <- newName $ "p" ++ show j
						q <- newName $ "q" ++ show j
						(restStmts, restE) <- f js
						return
							( (bindS (varP p) [| readIORef $(r j) |]) : restStmts ++
								[bindS (varP q) [| luaValueConcat $(varE p) $restE |] ]
							, varE q
							)
				(stmts, e) <- f [b..c]
				doE $ stmts ++ [noBindS [| writeIORef $(r a) $e |]]
			OP_JMP -> LuaInst [i + sbx + 1] $ \[jmpInstStmts] -> return jmpInstStmts
			OP_EQ -> condbinop [| luaValueEq |]
			OP_LT -> condbinop [| luaValueLt |]
			OP_LE -> condbinop [| luaValueLe |]
			OP_TEST -> LuaInst [nextInstId, nextNextInstId] $ \[nextInstStmts, nextNextInstStmts] -> return [noBindS [| do
				p <- readIORef $(r a)
				$(if c > 0 then
					[| if luaCoerceToBool p then $(doE nextInstStmts) else $(doE nextNextInstStmts) |]
					else
					[| if luaCoerceToBool p then $(doE nextNextInstStmts) else $(doE nextInstStmts) |])
				|] ]
			OP_TESTSET -> LuaInst [nextInstId, nextNextInstId] $ \[nextInstStmts, nextNextInstStmts] -> return [noBindS [| do
				p <- readIORef $(r b)
				$(if c > 0 then
					[| if luaCoerceToBool p then do
						writeIORef $(r a) p
						$(doE nextInstStmts)
						else $(doE nextNextInstStmts)
					|]
					else
					[| if luaCoerceToBool p then $(doE nextNextInstStmts) else do
						writeIORef $(r a) p
						$(doE nextInstStmts)
					|])
				|] ]
			OP_CALL -> normalFlow $ do
				when (b == 0) $ reportError "flaw-lua OP_CALL: calling with variable number of arguments is not implemented"
				when (c == 0) $ reportError "flaw-lua OP_CALL: returning variable number of arguments is not implemented"
				callop [(a + 1) .. (a + b - 1)] [a .. (a + c - 2)]
			OP_TAILCALL -> LuaInst [] $ \[] -> do
				when (b == 0) $ reportError "flaw-lua OP_TAILCALL: calling with variable number of arguments is not implemented"
				let args = [(a + 1) .. (a + b - 1)]
				(callArgsStmts, callArgsNames) <- liftM unzip $ forM args $ \j -> do
					n <- newName $ "a" ++ show j
					return (bindS (varP n) [| readIORef $(r j) |], n)
				f <- newName "f"
				return [noBindS $ doE $ (bindS (varP f) [| readIORef $(r a) |]) : callArgsStmts ++
					[ noBindS [| luaValueCall $(varE f) $(listE $ map varE callArgsNames) |]
					] ]
			OP_RETURN -> LuaInst [] $ \[] -> do
				when (b == 0) $ reportError "flaw-lua OP_RETURN: returning variable number of arguments is not implemented"
				let args = [a .. (a + b - 2)]
				(retArgsStmts, retArgsNames) <- liftM unzip $ forM args $ \j -> do
					n <- newName $ "a" ++ show j
					return (bindS (varP n) [| readIORef $(r j) |], n)
				return [noBindS $ doE $ retArgsStmts ++ [noBindS [| return $(listE $ map varE retArgsNames) |] ] ]
			OP_FORLOOP -> LuaInst [nextInstId, i + sbx + 1] $ \[nextInstStmts, jmpInstStmts] -> return [noBindS [| do
				step <- readIORef $(r $ a + 2)
				idx <- readIORef $(r a)
				newIdx <- luaValueAdd idx step
				writeIORef $(r a) newIdx
				limit <- readIORef $(r $ a + 1)
				positiveStep <- luaValueLt (LuaInteger 0) step
				loop <- if luaCoerceToBool positiveStep then luaValueLe newIdx limit else luaValueLe limit newIdx
				if luaCoerceToBool loop then $(doE $ (noBindS [| writeIORef $(r $ a + 3) newIdx |]) : jmpInstStmts)
				else $(doE nextInstStmts)
				|] ]
			OP_FORPREP -> LuaInst [i + sbx + 1] $ \[followingInstStmts] -> return $ (noBindS [| do
				step <- readIORef $(r $ a + 2)
				idx <- readIORef $(r a)
				writeIORef $(r a) =<< luaValueSub idx step
				|]) : followingInstStmts
			OP_TFORCALL -> normalFlow $ callop [a + 1, a + 2] [(a + 3) .. (a + 2 + c)]
			OP_TFORLOOP -> LuaInst [nextInstId, i + sbx + 1] $ \[nextInstStmts, followingInstStmts] -> return [noBindS [| do
				cond <- readIORef $(r $ a + 1)
				case cond of
					LuaNil -> $(doE nextInstStmts)
					_ -> do
						writeIORef $(r a) cond
						$(doE followingInstStmts)
				|] ]
			OP_SETLIST -> normalFlow $ do
				when (b == 0) $ reportError "flaw-lua OP_SETLIST: calling with variable number of arguments is not implemented"
				offset <- if c == 0 then extraArg else return c
				let fpf = 50 -- LFIELDS_PER_FLUSH from lopcodes.h
				t <- newName "t"
				let stmts = flip map [1..b] $ \j -> noBindS
					[| HT.insert $(varE t) (LuaInteger $(litE $ integerL $ fromIntegral $ (offset - 1) * fpf + j)) =<< readIORef $(r $ a + j) |]
				doE $ (bindS [p| LuaTable { luaTable = $(varP t) } |] [| readIORef $(r a) |]) : stmts
			OP_CLOSURE -> normalFlow [| do
				q <- newUnique
				writeIORef $(r a) $ LuaClosure
					{ luaClosureUnique = q
					, luaClosure = $(varE $ functionsNames V.! bx)
					}
				|]
			OP_VARARG -> normalFlow $ do
				when (b == 0) $ reportError "flaw-lua OP_VARARG: getting variable number of arguments is not implemented"
				let
					adjustVararg (j:js) = do
						q <- newName $ "q" ++ show j
						z <- newName $ "z" ++ show j
						(restStmts, restP) <- adjustVararg js
						return
							( [ noBindS $ caseE (varE q)
									[ match [p| $(varP z) : $restP |] (normalB $ doE $
										(noBindS [| writeIORef $(r j) $(varE z) |]) : restStmts) []
									, match [p| [] |] (normalB [| return () |]) []
									]
								]
							, varP q
							)
					adjustVararg [] = return ([], wildP)
				(readVarargStmts, varargPat) <- adjustVararg [a .. (a + b - 2)]
				doE $ (bindS varargPat [| readIORef $(varE varargName) |]) : readVarargStmts
			--OP_EXTRAARG -- should not be processed here
			_ -> LuaInst [] $ \[] -> fail "unknown Lua opcode"

	-- number of instructions referring to this instruction
	let instructionsRefCounts = VU.create $ do
		rc <- VUM.replicate (VU.length opcodes) (0 :: Int)
		-- force first instruction to have a name
		VUM.write rc 0 2
		-- calculate how many instructions refer to every instruction
		forM_ instructions $ \(LuaInst edges _) -> forM_ edges $ VUM.modify rc (+ 1)
		return rc

	-- generate names for instructions
	instructionsNames <- V.generateM (VU.length opcodes) $ \i -> newName $ "i" ++ show i
	-- emit stmts for instruction
	let getInstructionStmts (LuaInst followingInstIds f) = do
		followingInstsStmts <- forM followingInstIds $ \followingInstId -> do
			-- for instruction with one referrer (this one), emit stmts inline
			if instructionsRefCounts VU.! followingInstId == 1 then getInstructionStmts $ instructions V.! followingInstId
			-- otherwise use name
			else return [noBindS $ varE $ instructionsNames V.! followingInstId]
		f followingInstsStmts
	-- emit stmts for all instructions with >1 ref
	-- instructions with 1 ref will be emitted automatically
	sharedInstructionsDecs <- liftM (concat . V.toList) $ V.generateM (VU.length opcodes) $ \i ->
		if instructionsRefCounts VU.! i > 1 then do
			stmts <- getInstructionStmts $ instructions V.! i
			return [valD (varP $ instructionsNames V.! i) (normalB $ doE stmts) []]
		else return []

	lamE [argsPat] $ doE $ upvaluesStmt : stackStmts ++ varargStmts ++ argsStmts
		++ [functionsStmt, letS sharedInstructionsDecs, noBindS $ varE $ instructionsNames V.! 0]
