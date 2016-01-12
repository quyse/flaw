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
	, luaProtoInstructions :: !(VU.Vector Word32)
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

			-- instructions
			instructionsCount <- getInt
			instructions <- VU.replicateM instructionsCount S.getWord32le

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
				, luaProtoInstructions = instructions
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

-- | Compile Lua function.
compileLuaFunction :: LuaProto -> V.Vector ExpQ -> V.Vector ExpQ -> ExpQ
compileLuaFunction LuaProto
	{ luaProtoNumParams = numParams
	, luaProtoIsVararg = isVararg
	, luaProtoMaxStackSize = maxStackSize
	, luaProtoInstructions = instructions
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

	-- state
	stateName <- newName "g"

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
	instructionsNames <- V.generateM (VU.length instructions) $ \i -> newName $ "i" ++ show i
	let instructionsStmt = letS $ V.toList $ V.generate (VU.length instructions) $ \i -> let
		-- instruction word
		x = instructions VU.! i
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
		binop op = [| do
			p <- $(rk b)
			q <- $(rk c)
			writeIORef $(r a) =<< $op p q
			$nextInstruction
			|]
		unop op = normalFlow [| writeIORef $(r a) =<< $op =<< readIORef $(r b) |]

		-- next instruction
		nextInstruction = varE $ instructionsNames V.! (i + 1)
		-- next after next instruction
		nextNextInstruction = varE $ instructionsNames V.! (i + 2)
		-- append next instruction
		normalFlow e = doE
			[ noBindS e
			, noBindS nextInstruction
			]
		-- conditional operation
		condbinop op = [| do
			p <- $(rk b)
			q <- $(rk c)
			z <- $op p q
			$(if a > 0 then
				[| if luaCoerceToBool z then $nextInstruction else $nextNextInstruction |]
				else
				[| if luaCoerceToBool z then $nextNextInstruction else $nextInstruction |])
			|]
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
			(retsStmts, retPat) <- adjustRets rets
			doE $ (bindS (varP f) [| readIORef $(r a) |]) : callArgsStmts ++
				[ bindS retPat [| luaValueCall $(varE f) $(varE stateName) $(listE $ map varE callArgsNames) |]
				] ++ retsStmts ++ [noBindS nextInstruction]

		-- choose by instruction
		instructionE = case x .&. (bit 6 - 1) of
			OP_MOVE -> normalFlow [| writeIORef $(r a) =<< readIORef $(r b) |]
			OP_LOADK -> normalFlow [| writeIORef $(r a) $kbx |]
			OP_LOADKX -> do
				let nx = instructions VU.! (i + 1)
				when ((nx .&. (bit 6 - 1)) /= OP_EXTRAARG) $ fail "OP_LOADKX must be followed by OP_EXTRAARG"
				let nax = fromIntegral $ (nx `shiftR` 6) .&. (bit 26 - 1)
				normalFlow [| writeIORef $(r a) $(kst nax) |]
			OP_LOADBOOL -> [| do
				writeIORef $(r a) $(conE $ if b > 0 then 'True else 'False)
				$(if c > 0 then nextNextInstruction else nextInstruction)
				|]
			OP_LOADNIL -> doE $ (flip map [a .. (a + b)] $ \j -> noBindS [| writeIORef $(r j) LuaNil |]) ++ [noBindS nextInstruction]
			OP_GETUPVAL -> normalFlow [| writeIORef $(r a) =<< readIORef $(u b) |]
			OP_GETTABUP -> [| do -- swapped b & c???
				LuaTable { luaTable = t } <- readIORef $(u b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				$nextInstruction
				|]
			OP_GETTABLE -> [| do
				LuaTable { luaTable = t } <- readIORef $(r b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				$nextInstruction
				|]
			OP_SETTABUP -> [| do
				LuaTable { luaTable = t } <- readIORef $(u a)
				HT.insert t $(rk b) =<< readIORef =<< $(rk c)
				$nextInstruction
				|]
			OP_SETUPVAL -> normalFlow [| writeIORef $(u b) =<< readIORef $(r a) |]
			OP_SETTABLE -> [| do
				LuaTable { luaTable = t } <- readIORef $(r a)
				q <- $(rk b)
				HT.insert t q =<< $(rk c)
				$nextInstruction
				|]
			OP_NEWTABLE -> [| do
				q <- newUnique
				t <- HT.newSized $(litE $ integerL $ fromIntegral $ max b c)
				z <- newIORef LuaNil
				writeIORef $(r a) $ LuaTable
					{ luaTableUnique = q
					, luaTable = t
					, luaTableMetaTable = z
					}
				$nextInstruction
				|]
			OP_SELF -> [| do
				writeIORef $(r $ a + 1) =<< readIORef $(r b)
				LuaTable { luaTable = t } <- readIORef $(r b)
				writeIORef $(r a) =<< liftM (fromMaybe LuaNil) (HT.lookup t =<< $(rk c))
				$nextInstruction
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
			OP_CONCAT -> do
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
				doE $ stmts ++ [noBindS [| writeIORef $(r a) $e |], noBindS nextInstruction]
			OP_JMP -> varE $ instructionsNames V.! (i + sbx + 1)
			OP_EQ -> condbinop [| luaValueEq |]
			OP_LT -> condbinop [| luaValueLt |]
			OP_LE -> condbinop [| luaValueLe |]
			OP_TEST -> [| do
				p <- readIORef $(r a)
				$(if c > 0 then
					[| if luaCoerceToBool p then $nextInstruction else $nextNextInstruction |]
					else
					[| if luaCoerceToBool p then $nextNextInstruction else $nextInstruction |])
				|]
			OP_TESTSET -> [| do
				p <- readIORef $(r b)
				$(if c > 0 then
					[| if luaCoerceToBool p then do
						writeIORef $(r a) p
						$nextInstruction
						else $nextNextInstruction
					|]
					else
					[| if luaCoerceToBool p then $nextNextInstruction else do
						writeIORef $(r a) p
						$nextInstruction
					|])
				|]
			OP_CALL -> callop [(a + 1) .. (a + b - 1)] [a .. (a + c - 2)]
			OP_TAILCALL -> do
				let args = [(a + 1) .. (a + b - 1)]
				(callArgsStmts, callArgsNames) <- liftM unzip $ forM args $ \j -> do
					n <- newName $ "a" ++ show j
					return (bindS (varP n) [| readIORef $(r j) |], n)
				f <- newName "f"
				doE $ (bindS (varP f) [| readIORef $(r a) |]) : callArgsStmts ++
					[ noBindS [| luaValueCall $(varE f) $(listE $ map varE callArgsNames) |]
					]
			OP_RETURN -> do
				let args = [a .. (a + b - 2)]
				(retArgsStmts, retArgsNames) <- liftM unzip $ forM args $ \j -> do
					n <- newName $ "a" ++ show j
					return (bindS (varP n) [| readIORef $(r j) |], n)
				doE $ retArgsStmts ++ [noBindS [| return $(listE $ map varE retArgsNames) :: IO [LuaValue] |] ]
			OP_FORLOOP -> [| do
				step <- readIORef $(r $ a + 2)
				idx <- readIORef $(r a)
				newIdx <- luaValueAdd idx step
				writeIORef $(r a) newIdx
				limit <- readIORef $(r $ a + 1)
				positiveStep <- luaValueLt (LuaInteger 0) step
				loop <- if luaCoerceToBool positiveStep then luaValueLe newIdx limit else luaValueLe limit newIdx
				if luaCoerceToBool loop then do
					writeIORef $(r $ a + 3) newIdx
					$(varE $ instructionsNames V.! (i + sbx + 1))
				else $nextInstruction
				|]
			OP_FORPREP -> [| do
				step <- readIORef $(r $ a + 2)
				idx <- readIORef $(r a)
				writeIORef $(r a) =<< luaValueSub idx step
				$(varE $ instructionsNames V.! (i + sbx + 1))
				|]
			OP_TFORCALL -> callop [a + 1, a + 2] [(a + 3) .. (a + 2 + c)]
			OP_TFORLOOP -> [| do
				cond <- readIORef $(r $ a + 1)
				case cond of
					LuaNil -> $nextInstruction
					_ -> do
						writeIORef $(r a) cond
						$(varE $ instructionsNames V.! (i + sbx + 1))
				|]
			--OP_SETLIST
			OP_CLOSURE -> [| do
				q <- newUnique
				writeIORef $(r a) $ LuaClosure
					{ luaClosureUnique = q
					, luaClosure = $(varE $ functionsNames V.! bx)
					}
				$nextInstruction
				|]
			OP_VARARG -> do
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
				doE $ (bindS varargPat [| readIORef $(varE varargName) |]) : readVarargStmts ++ [noBindS nextInstruction]
			--OP_EXTRAARG -- should not be processed here
			_ -> fail "unknown Lua opcode"

		in valD (varP $ instructionsNames V.! i) (normalB instructionE) []

	lamE [varP stateName, argsPat] $ doE $ upvaluesStmt : stackStmts ++ varargStmts ++ argsStmts
		++ [functionsStmt] ++ [instructionsStmt] ++ [noBindS $ varE $ instructionsNames V.! 0]
