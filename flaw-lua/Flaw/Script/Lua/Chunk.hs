{-|
Module: Flaw.Script.Lua.Chunk
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Script.Lua.Chunk
	( compileLuaChunk
	) where

import Debug.Trace

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Serialize as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Script.Lua
import Flaw.Script.Lua.FFI

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

trc :: (Monad m, Show a) => String -> m a -> m a
trc msg m = do
	r <- m
	trace (msg ++ " " ++ show r) $ return r

-- | Compile Lua chunk.
compileLuaChunk :: B.ByteString -> ExpQ
compileLuaChunk bytes = do

	-- parse
	runIO $ B.writeFile "header.luab" luaChunkHeader
	let eitherProto = flip S.runGet bytes $ do
		chunkHeader <- S.getByteString (B.length luaChunkHeader)
		when (chunkHeader /= luaChunkHeader) $ fail "wrong Lua chunk header"

		let getInt = trc "getInt" $ liftM fromIntegral S.getWord32le :: S.Get Int

		let getString = do
			b <- trc "string b" S.getWord8
			size <- trc "string size" $ if b == 0xff then liftM fromIntegral S.getWordhost else return $ fromIntegral b
			if size == 0 then return B.empty
			else do
				trc "string" $ S.getByteString $ size - 1

		_chunkUpvaluesCount <- trc "chunkUpvaluesCount" S.getWord8

		let loadFunction = do
			source <- getString
			lineDefined <- getInt
			lastLineDefined <- getInt
			numParams <- trc "numParams" $ liftM fromIntegral S.getWord8
			isVararg <- trc "isVararg" $ liftM ( > 0) S.getWord8
			maxStackSize <- trc "maxStackSize" $ liftM fromIntegral S.getWord8

			-- instructions
			instructionsCount <- getInt
			instructions <- trc "instructions" $ VU.replicateM instructionsCount S.getWord32le

			-- constants
			constantsCount <- getInt
			constants <- V.replicateM constantsCount $ do
				t <- trc "constant type" S.getWord8
				let getStringConstant = do
					s <- getString
					return [| LuaString $ fromString $(litE $ stringL $ T.unpack $ T.decodeUtf8 s) |]
				case t of
					LUA_TNIL -> return [| LuaNil |]
					LUA_TBOOLEAN -> do
						b <- trc "TBOOLEAN" $ liftM ( > 0) S.getWord8
						return [| LuaBoolean b |]
					LUA_TNUMFLT -> do
						n <- trc "TNUMFLT" S.getFloat64le
						return [| LuaReal n |]
					LUA_TNUMINT -> do
						n <- trc "TNUMINT" S.getWordhost
						return [| LuaInteger n |]
					LUA_TSHRSTR -> getStringConstant
					LUA_TLNGSTR -> getStringConstant
					_ -> fail "wrong Lua constant"

			-- upvalues
			upvaluesCount <- getInt
			upvalues <- V.replicateM upvaluesCount $ do
				instack <- trc "upvalue instack" S.getWord8
				idx <- trc "upvalue idx" S.getWord8
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

	-- stack
	stackNames <- V.generateM maxStackSize $ \i -> newName $ "s" ++ show i
	let stackStmts = V.toList $ V.generate maxStackSize $ \i ->
		bindS (varP (stackNames V.! i)) [| newIORef LuaNil |]
	let stack = V.generate maxStackSize $ \i -> varE $ stackNames V.! i

	-- arguments
	let argsSetStmts i = if i >= numParams then return ([], wildP) else do
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
		a = fromIntegral $ (x `shiftR` 6) .&. ((1 `shiftL` 8) - 1)
		b = fromIntegral $ (x `shiftR` 14) .&. ((1 `shiftL` 9) - 1)
		c = (x `shiftR` 23) .&. ((1 `shiftL` 9) - 1)
		ax = (x `shiftR` 6) .&. ((1 `shiftL` 26) - 1)
		bx = fromIntegral $ (x `shiftR` 14) .&. ((1 `shiftL` 18) - 1)
		kbx = constants V.! bx

		-- helper functions
		number = litE . integerL . fromIntegral :: Word32 -> ExpQ

		-- next instruction
		nextInstruction = varE $ instructionsNames V.! (i + 1)
		-- next after next instruction
		nextNextInstruction = varE $ instructionsNames V.! (i + 2)
		-- append next instruction
		normalFlow e = doE
			[ noBindS e
			, noBindS nextInstruction
			]

		-- choose by instruction
		r = case x .&. ((1 `shiftL` 6) - 1) of
			OP_MOVE -> normalFlow [| writeIORef $(stack V.! a) =<< readIORef $(stack V.! b) |]
			OP_LOADK -> normalFlow [| writeIORef $(stack V.! a) $kbx |]
			OP_LOADKX -> do
				let nx = instructions VU.! (i + 1)
				when ((nx .&. ((1 `shiftL` 6) - 1)) /= OP_EXTRAARG) $ fail "OP_LOADKX must be followed by OP_EXTRAARG"
				let nax = fromIntegral $ (nx `shiftR` 6) .&. ((1 `shiftL` 26) - 1)
				let knax = constants V.! nax
				normalFlow [| writeIORef $(stack V.! a) $knax |]
			OP_LOADBOOL -> [| do
				writeIORef $(stack V.! a) $(conE $ if b > 0 then 'True else 'False)
				$(if c > 0 then nextNextInstruction else nextInstruction)
				|]
			OP_LOADNIL -> normalFlow $ doE $ flip map [a .. (a + b)] $ \j -> noBindS [| writeIORef $(stack V.! j) LuaNil |]
			OP_GETUPVAL -> normalFlow [| writeIORef $(stack V.! a) =<< readIORef $(upvalues V.! b) |]
			_ -> [| return () |]

		in valD (varP $ instructionsNames V.! i) (normalB r) []

	lamE [argsPat] $ doE $ upvaluesStmt : stackStmts ++ argsStmts
		++ [functionsStmt] ++ [instructionsStmt] ++ [noBindS $ varE $ instructionsNames V.! 0]
