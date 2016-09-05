{-|
Module: Flaw.Script.Lua.Chunk
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Script.Lua.Chunk
	( luaCompileChunk
	) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.Primitive.MutVar
import qualified Data.Serialize as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: Word64)
	-- size of lua_Number
	S.putWord8 $ fromIntegral $ sizeOf (undefined :: Double)
	-- test lua_Integer
	S.putWord64host LUAC_INT
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
	-- | Flags for variables. True means volatile variable.
	, luaProtoVariables :: !(V.Vector Bool)
	, luaProtoUpvalues :: !(V.Vector (Bool, Int))
	-- | Bitmask of in-stack upvalues (for parent scope they are volatile variables).
	, luaProtoVolatileUpvaluesMask :: !Integer
	, luaProtoFunctions :: !(V.Vector LuaProto)
	}

-- | Compile Lua chunk.
luaCompileChunk :: B.ByteString -> ExpQ
luaCompileChunk bytes = do

	-- parse
	let eitherProto = flip S.runGet bytes $ do
		chunkHeader <- S.getByteString (B.length luaChunkHeader)
		when (chunkHeader /= luaChunkHeader) $ fail "wrong Lua chunk header"

		let getInt = fmap fromIntegral S.getWord32le :: S.Get Int

		let getString = do
			b <- S.getWord8
			size <- if b == 0xff then fmap fromIntegral S.getWordhost else return $ fromIntegral b
			if size == 0 then return B.empty
			else S.getByteString $ size - 1

		_chunkUpvaluesCount <- S.getWord8

		let loadFunction = do
			source <- getString
			lineDefined <- getInt
			lastLineDefined <- getInt
			numParams <- fmap fromIntegral S.getWord8
			isVararg <- fmap ( > 0) S.getWord8
			maxStackSize <- fmap fromIntegral S.getWord8

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
						b <- fmap ( > 0) S.getWord8
						return [| LuaBoolean b |]
					LUA_TNUMFLT -> do
						n <- S.getFloat64le
						return [| LuaReal n |]
					LUA_TNUMINT -> do
						n <- fmap fromIntegral S.getWord64host :: S.Get Int
						return [| LuaInteger n |]
					LUA_TSHRSTR -> getStringConstant
					LUA_TLNGSTR -> getStringConstant
					_ -> fail "wrong Lua constant"

			-- upvalues
			upvaluesCount <- getInt
			upvalues <- V.replicateM upvaluesCount $ do
				instack <- ( > 0) <$> S.getWord8
				idx <- fromIntegral <$> S.getWord8
				return (instack, idx)

			-- bitmask of in-stack upvalues
			let volatileUpvaluesMask = V.foldr (\(instack, idx) mask -> if instack then mask .|. (1 `shiftL` idx) else mask) 0 upvalues

			-- subfunctions
			functionsCount <- getInt
			functions <- V.replicateM functionsCount loadFunction

			-- calculate volatile variables
			let
				volatileVariablesMask = V.foldr (\LuaProto
					{ luaProtoVolatileUpvaluesMask = mask
					} restMask -> mask .|. restMask) 0 functions
				variables = V.generate maxStackSize $ testBit volatileVariablesMask

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
				, luaProtoVariables = variables
				, luaProtoUpvalues = upvalues
				, luaProtoVolatileUpvaluesMask = volatileUpvaluesMask
				, luaProtoFunctions = functions
				}

		loadFunction

	mainProto <- case eitherProto of
		Left err -> fail err
		Right proto -> return proto

	env <- newName "e"
	lamE [varP env] $ compileLuaFunction mainProto (V.singleton $ varE env) V.empty

-- | Internal representation of instruction.
-- Contains indices of instructions this instruction refers to, and function
-- accepting list of codes (corresponding to indices), and returning code of the instruction.
data LuaInst = LuaInst [Int] ([LuaCode] -> LuaCode)

-- | Internal representation of instruction's code.
type LuaCode = LuaCodeState -> Q [StmtQ]

data LuaCodeState = LuaCodeState
	{
	  -- | Start register of dynamic arguments.
	  -- -1 if not set.
	  luaCodeStateTop :: !Int
	  -- | Expression representing dynamic arguments (of type [LuaValue m]).
	, luaCodeStateTopValuesE :: ExpQ
	}

nullCodeState :: LuaCodeState
nullCodeState = LuaCodeState
	{ luaCodeStateTop = -1
	, luaCodeStateTopValuesE = undefined
	}

-- | Compile Lua function.
compileLuaFunction :: LuaProto -> V.Vector ExpQ -> V.Vector ExpQ -> ExpQ
compileLuaFunction LuaProto
	{ luaProtoNumParams = numParams
	, luaProtoIsVararg = isVararg
	, luaProtoMaxStackSize = maxStackSize
	, luaProtoOpcodes = opcodes
	, luaProtoConstants = protoConstants
	, luaProtoUpvalues = protoUpvalues
	, luaProtoFunctions = functions
	} parentStack parentUpvalues = do

	-- constants and upvalues
	constantsNames <- V.generateM (V.length protoConstants) $ \i -> newName $ "k" ++ show i
	upvaluesNames <- V.generateM (V.length protoUpvalues) $ \i -> newName $ "u" ++ show i
	let constantsUpvaluesStmt = let
		constantsLets = V.generate (V.length protoConstants) $ \i -> valD (varP (constantsNames V.! i)) (normalB (protoConstants V.! i)) []
		upvaluesLets = V.generate (V.length protoUpvalues) $ \i -> let
			(instack, idx) = protoUpvalues V.! i
			upvalueValue = if instack then parentStack V.! idx else parentUpvalues V.! idx
			in valD (varP (upvaluesNames V.! i)) (normalB upvalueValue) []
		in letS $ V.toList $ constantsLets V.++ upvaluesLets
	let constants = V.map varE constantsNames
	let upvalues = V.map varE upvaluesNames

	-- stack
	stackNames <- V.generateM maxStackSize $ \i -> newName $ "s" ++ show i
	let stackStmts = V.toList $ V.generate maxStackSize $ \i ->
		bindS (varP (stackNames V.! i)) [| newMutVar LuaNil |]
	let stack = V.generate maxStackSize $ \i -> varE $ stackNames V.! i

	-- arguments & vararg
	varargName <- newName "va"
	let varargStmts = if isVararg then [bindS (varP varargName) [| newMutVar [] |] ] else []
	let argsSetStmts i =
		if i < numParams then do
			a <- newName "a"
			x <- newName "x"
			(restStmts, xs) <- argsSetStmts $ i + 1
			let e = doE $ (noBindS [| writeMutVar $(stack V.! i) $(varE x) |]) : restStmts
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
				( [ noBindS [| writeMutVar $(varE varargName) $(varE a) |]
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
		kst j = constants V.! j -- :: LuaValue m
		r j = stack V.! j -- :: MutVar (PrimState m) (LuaValue m)
		rk e j = -- :: m (LuaValue m)
			if j `testBit` 8 then [| $e $(kst (j `clearBit` 8)) |]
			else [| $e =<< readMutVar $(r j) |]
		rk2 e j1 j2 = -- :: m (LuaValue m)
			if j1 `testBit` 8 then rk [| $e $(kst (j1 `clearBit` 8)) |] j2
			else [| do
				p <- readMutVar $(r j1)
				$(rk [| $e p |] j2)
				|]
		u j = upvalues V.! j -- :: MutVar (PrimState m) (LuaValue m)
		binop op = normalFlow [| writeMutVar $(r a) =<< $(rk2 op b c) |]
		unop op = normalFlow [| writeMutVar $(r a) =<< $op =<< readMutVar $(r b) |]

		-- next and next-after-next instruction ids
		nextInstId = i + 1
		nextNextInstId = i + 2
		-- append next instruction
		normalFlow e = LuaInst [nextInstId] $ \[nextInstCode] codeState -> fmap ((noBindS e) :) $ nextInstCode codeState
		-- conditional operation
		condbinop op = LuaInst [nextInstId, nextNextInstId] $ \[nextInstCode, nextNextInstCode] codeState -> do
			nextInstStmts <- nextInstCode codeState
			nextNextInstStmts <- nextNextInstCode codeState
			return [noBindS [| do
				z <- $(rk2 op b c)
				$(if a > 0 then
					[| if luaCoerceToBool z then $(doE nextInstStmts) else $(doE nextNextInstStmts) |]
					else
					[| if luaCoerceToBool z then $(doE nextNextInstStmts) else $(doE nextInstStmts) |])
				|] ]
		-- static arg for call-like operations
		staticArgStmtAndExp j = do
			n <- newName $ "a" ++ show j
			return (bindS (varP n) [| readMutVar $(r j) |], varE n)
		-- get args (possibly dynamic)
		getArgs :: Either Int [Int] -> LuaCodeState -> Q ([StmtQ], ExpQ)
		getArgs eargs LuaCodeState
			{ luaCodeStateTop = top
			, luaCodeStateTopValuesE = topValuesE
			} = case eargs of
			Right args -> do
				when (top >= 0) $ reportError "flaw-lua: dynamic values are lost"
				(stmts, argsEs) <- fmap unzip $ mapM staticArgStmtAndExp args
				return (stmts, listE argsEs)
			Left firstArg -> do
				when (top < 0) $ reportError "flaw-lua: expected dynamic values for opcode"
				(stmts, argsEs) <- fmap unzip $ mapM staticArgStmtAndExp [firstArg .. (top - 1)]
				return (stmts, foldr (\p q -> [| $p : $q |]) topValuesE argsEs)
		putRets :: Either Int [Int] -> ExpQ -> LuaCode -> LuaCodeState -> Q [StmtQ]
		putRets erets mainE nextInstCode codeState = case erets of
			Right rets -> do
				(retsStmts, retsPats) <- fmap unzip $ forM rets $ \j -> do
					n <- newName $ "r" ++ show j
					return (noBindS [| writeMutVar $(r j) $(varE n) |], varP n)
				let retPat = foldr (\p q -> [p| $p : $q |]) wildP retsPats
				let mainStmt = if null rets then noBindS [| void $mainE |]
					else bindS retPat [| fmap (++ repeat LuaNil) $mainE |]
				nextInstStmts <- nextInstCode codeState
					{ luaCodeStateTop = -1
					}
				return $ mainStmt : retsStmts ++ nextInstStmts
			Left firstRet -> do
				retsName <- newName "r"
				let mainStmt = bindS (varP retsName) mainE
				nextInstStmts <- nextInstCode codeState
					{ luaCodeStateTop = firstRet
					, luaCodeStateTopValuesE = varE retsName
					}
				return $ mainStmt : nextInstStmts
		-- call operation
		callop :: Either Int [Int] -> Either Int [Int] -> LuaInst
		callop eargs erets = LuaInst [nextInstId] $ \[nextInstCode] codeState -> do
			(getArgsStmts, argsE) <- getArgs eargs codeState
			f <- newName "f"
			putRetsStmts <- putRets erets [| luaValueCall $(varE f) $argsE |] nextInstCode codeState
			return $ (bindS (varP f) [| readMutVar $(r a) |]) : getArgsStmts ++ putRetsStmts
		-- get ax from extra arg
		extraArg = do
			let nx = opcodes VU.! (i + 1)
			when ((nx .&. (bit 6 - 1)) /= OP_EXTRAARG) $ fail "flaw-lua: opcode must be followed by OP_EXTRAARG"
			return $ fromIntegral $ (nx `shiftR` 6) .&. (bit 26 - 1)

		-- choose by instruction
		in case x .&. (bit 6 - 1) of
			OP_MOVE -> normalFlow [| writeMutVar $(r a) =<< readMutVar $(r b) |]
			OP_LOADK -> normalFlow [| writeMutVar $(r a) $kbx |]
			OP_LOADKX -> LuaInst [nextNextInstId] $ \[nextNextInstCode] codeState -> do
				nax <- extraArg
				nextNextInstStmts <- nextNextInstCode codeState
				return $ (noBindS [| writeMutVar $(r a) $(kst nax) |]) : nextNextInstStmts
			OP_LOADBOOL -> LuaInst [if c > 0 then nextNextInstId else nextInstId] $ \[followingInstCode] codeState -> do
				followingInstStmts <- followingInstCode codeState
				return $ (noBindS [| writeMutVar $(r a) $ LuaBoolean $(conE $ if b > 0 then 'True else 'False) |]) : followingInstStmts
			OP_LOADNIL -> normalFlow $ doE $ flip map [a .. (a + b)] $ \j -> noBindS [| writeMutVar $(r j) LuaNil |]
			OP_GETUPVAL -> normalFlow [| writeMutVar $(r a) =<< readMutVar $(u b) |]
			OP_GETTABUP -> normalFlow [| do
				t <- readMutVar $(u b)
				writeMutVar $(r a) =<< $(rk [| luaValueGet t |] c)
				|]
			OP_GETTABLE -> normalFlow [| do
				t <- readMutVar $(r b)
				writeMutVar $(r a) =<< $(rk [| luaValueGet t |] c)
				|]
			OP_SETTABUP -> normalFlow [| do
				t <- readMutVar $(u a)
				$(rk2 [| luaValueSet t |] b c)
				|]
			OP_SETUPVAL -> normalFlow [| writeMutVar $(u b) =<< readMutVar $(r a) |]
			OP_SETTABLE -> normalFlow [| do
				t <- readMutVar $(r a)
				$(rk2 [| luaValueSet t |] b c)
				|]
			OP_NEWTABLE -> normalFlow [| writeMutVar $(r a) =<< luaNewTableSized $(litE $ integerL $ fromIntegral $ max b c) |]
			OP_SELF -> normalFlow [| do
				writeMutVar $(r $ a + 1) =<< readMutVar $(r b)
				t <- readMutVar $(r b)
				writeMutVar $(r a) =<< $(rk [| luaValueGet t |] c)
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
						return ([bindS (varP p) [| readMutVar $(r j) |] ], varE p)
					f (j:js) = do
						p <- newName $ "p" ++ show j
						q <- newName $ "q" ++ show j
						(restStmts, restE) <- f js
						return
							( (bindS (varP p) [| readMutVar $(r j) |]) : restStmts ++
								[bindS (varP q) [| luaValueConcat $(varE p) $restE |] ]
							, varE q
							)
				(stmts, e) <- f [b..c]
				doE $ stmts ++ [noBindS [| writeMutVar $(r a) $e |]]
			OP_JMP -> LuaInst [i + sbx + 1] $ \[jmpInstCode] codeState -> do
				jmpInstStmts <- jmpInstCode codeState
				return jmpInstStmts
			OP_EQ -> condbinop [| luaValueEq |]
			OP_LT -> condbinop [| luaValueLt |]
			OP_LE -> condbinop [| luaValueLe |]
			OP_TEST -> LuaInst [nextInstId, nextNextInstId] $ \[nextInstCode, nextNextInstCode] codeState -> do
				nextInstStmts <- nextInstCode codeState
				nextNextInstStmts <- nextNextInstCode codeState
				return [noBindS [| do
					p <- readMutVar $(r a)
					$(if c > 0 then
						[| if luaCoerceToBool p then $(doE nextInstStmts) else $(doE nextNextInstStmts) |]
						else
						[| if luaCoerceToBool p then $(doE nextNextInstStmts) else $(doE nextInstStmts) |])
					|] ]
			OP_TESTSET -> LuaInst [nextInstId, nextNextInstId] $ \[nextInstCode, nextNextInstCode] codeState -> do
				nextInstStmts <- nextInstCode codeState
				nextNextInstStmts <- nextNextInstCode codeState
				return [noBindS [| do
					p <- readMutVar $(r b)
					$(if c > 0 then
						[| if luaCoerceToBool p then $(doE $ (noBindS [| writeMutVar $(r a) p |]) : nextInstStmts)
							else $(doE nextNextInstStmts)
						|]
						else
						[| if luaCoerceToBool p then $(doE nextNextInstStmts)
							else $(doE $ (noBindS [| writeMutVar $(r a) p |]) : nextInstStmts)
						|])
					|] ]
			OP_CALL -> callop
				(if b == 0 then Left (a + 1) else Right [(a + 1) .. (a + b - 1)])
				(if c == 0 then Left a else Right [a .. (a + c - 2)])
			OP_TAILCALL -> LuaInst [] $ \[] codeState -> do
				(getArgsStmts, argsE) <- getArgs (if b == 0 then Left (a + 1) else Right [(a + 1) .. (a + b - 1)]) codeState
				f <- newName "f"
				let callE = [| luaValueCall $(varE f) $argsE |]
				return $ (bindS (varP f) [| readMutVar $(r a) |]) : getArgsStmts ++ [noBindS callE]
			OP_RETURN -> LuaInst [] $ \[] codeState -> do
				(getArgsStmts, argsE) <- getArgs (if b == 0 then Left a else Right [a .. (a + b - 2)]) codeState
				return $ getArgsStmts ++ [noBindS [| return $argsE |] ]
			OP_FORLOOP -> LuaInst [nextInstId, i + sbx + 1] $ \[nextInstCode, jmpInstCode] _codeState -> do
				nextInstStmts <- nextInstCode nullCodeState
				jmpInstStmts <- jmpInstCode nullCodeState
				return [noBindS [| do
					step <- readMutVar $(r $ a + 2)
					idx <- readMutVar $(r a)
					newIdx <- luaValueAdd idx step
					writeMutVar $(r a) newIdx
					limit <- readMutVar $(r $ a + 1)
					positiveStep <- luaValueLt (LuaInteger 0) step
					loop <- if luaCoerceToBool positiveStep then luaValueLe newIdx limit else luaValueLe limit newIdx
					if luaCoerceToBool loop then $(doE $ (noBindS [| writeMutVar $(r $ a + 3) newIdx |]) : jmpInstStmts)
					else $(doE nextInstStmts)
					|] ]
			OP_FORPREP -> LuaInst [i + sbx + 1] $ \[followingInstCode] codeState -> do
				followingInstStmts <- followingInstCode codeState
				return $ (noBindS [| do
					step <- readMutVar $(r $ a + 2)
					idx <- readMutVar $(r a)
					writeMutVar $(r a) =<< luaValueSub idx step
					|]) : followingInstStmts
			OP_TFORCALL -> callop
				(Right [a + 1, a + 2])
				(Right [(a + 3) .. (a + 2 + c)])
			OP_TFORLOOP -> LuaInst [nextInstId, i + sbx + 1] $ \[nextInstCode, followingInstCode] _codeState -> do
				nextInstStmts <- nextInstCode nullCodeState
				followingInstStmts <- followingInstCode nullCodeState
				return [noBindS [| do
					cond <- readMutVar $(r $ a + 1)
					case cond of
						LuaNil -> $(doE nextInstStmts)
						_ -> do
							writeMutVar $(r a) cond
							$(doE followingInstStmts)
					|] ]
			OP_SETLIST -> LuaInst [nextInstId] $ \[nextInstCode] codeState -> do
				offset <- if c == 0 then extraArg else return c
				let fpf = 50 -- LFIELDS_PER_FLUSH from lopcodes.h
				t <- newName "t"
				(getArgsStmts, argsE) <- getArgs (if b == 0 then Left (a + 1) else Right [(a + 1) .. (a + b)]) codeState
				nextInstStmts <- nextInstCode codeState
					{ luaCodeStateTop = -1
					}
				return $ getArgsStmts ++
					[ bindS (varP t) [| readMutVar $(r a) |]
					, noBindS
						[| forM_ (zip $argsE [1..]) $ \(p, j) -> luaValueSet $(varE t) (LuaInteger $ j + $(litE $ integerL $ fromIntegral $ (offset - 1) * fpf)) p |]
					] ++ nextInstStmts
			OP_CLOSURE -> normalFlow [| writeMutVar $(r a) =<< luaNewClosure $(varE $ functionsNames V.! bx) |]
			OP_VARARG -> LuaInst [nextInstId] $ \[nextInstCode] codeState -> do
				putRets (if b == 0 then Left a else Right [a .. (a + b - 2)]) [| readMutVar $(varE varargName) |] nextInstCode codeState
			--OP_EXTRAARG -- should not be processed here
			_ -> LuaInst [] $ \[] _ -> fail "unknown Lua opcode"

	-- number of instructions referring to this instruction
	let instructionsRefCounts = VU.create $ do
		-- mark all instructions reachable from first instruction
		reachable <- VUM.replicate (VU.length opcodes) False
		let markReachable i = do
			alreadyReachable <- VUM.read reachable i
			unless alreadyReachable $ do
				VUM.write reachable i True
				let LuaInst edges _ = instructions V.! i
				mapM_ markReachable edges
		markReachable 0

		rc <- VUM.replicate (VU.length opcodes) (0 :: Int)
		-- force first instruction to have a name
		VUM.write rc 0 2
		-- calculate how many instructions refer to every instruction
		flip V.imapM_ instructions $ \i (LuaInst edges _) -> do
			isRechable <- VUM.read reachable i
			when isRechable $ forM_ edges $ VUM.modify rc (+ 1)
		return rc

	-- generate names for instructions
	instructionsNames <- V.generateM (VU.length opcodes) $ \i -> newName $ "i" ++ show i
	-- emit stmts for instruction
	let getInstructionCode (LuaInst followingInstsIds f) = do
		followingInstsCodes <- forM followingInstsIds $ \followingInstId -> do
			-- for instruction with one referrer (this one), emit stmts inline
			if instructionsRefCounts VU.! followingInstId == 1 then getInstructionCode $ instructions V.! followingInstId
			-- otherwise use name
			else return $ \LuaCodeState
				{ luaCodeStateTop = top
				} -> do
				when (top >= 0) $ reportError "flaw-lua: instruction reference cannot send dynamic values"
				return [noBindS $ varE $ instructionsNames V.! followingInstId]
		return $ f followingInstsCodes
	-- emit stmts for all instructions with >1 ref
	-- instructions with 1 ref will be emitted automatically
	sharedInstructionsDecs <- fmap (concat . V.toList) $ V.generateM (VU.length opcodes) $ \i ->
		if instructionsRefCounts VU.! i > 1 then do
			code <- getInstructionCode $ instructions V.! i
			stmts <- code nullCodeState
			return [valD (varP $ instructionsNames V.! i) (normalB $ doE stmts) []]
		else return []

	lamE [argsPat] $ doE $ constantsUpvaluesStmt : stackStmts ++ varargStmts ++ argsStmts
		++ [functionsStmt, letS sharedInstructionsDecs, noBindS $ varE $ instructionsNames V.! 0]
