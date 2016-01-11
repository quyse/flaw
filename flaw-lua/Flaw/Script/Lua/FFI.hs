{-|
Module: Flaw.Script.Lua.FFI
Description: Lua FFI definitions.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module Flaw.Script.Lua.FFI
	( pattern LUA_TNIL
	, pattern LUA_TBOOLEAN
	, pattern LUA_TLIGHTUSERDATA
	, pattern LUA_TNUMBER
	, pattern LUA_TSTRING
	, pattern LUA_TTABLE
	, pattern LUA_TFUNCTION
	, pattern LUA_TUSERDATA
	, pattern LUA_TTHREAD

	, pattern LUA_TSHRSTR
	, pattern LUA_TLNGSTR
	, pattern LUA_TNUMFLT
	, pattern LUA_TNUMINT

	, pattern LUA_SIGNATURE
	, pattern LUAC_DATA
	, pattern LUAC_INT
	, pattern LUAC_NUM
	, pattern OP_MOVE
	, pattern OP_LOADK
	, pattern OP_LOADKX
	, pattern OP_LOADBOOL
	, pattern OP_LOADNIL
	, pattern OP_GETUPVAL
	, pattern OP_GETTABUP
	, pattern OP_GETTABLE
	, pattern OP_SETTABUP
	, pattern OP_SETUPVAL
	, pattern OP_SETTABLE
	, pattern OP_NEWTABLE
	, pattern OP_SELF
	, pattern OP_ADD
	, pattern OP_SUB
	, pattern OP_MUL
	, pattern OP_MOD
	, pattern OP_POW
	, pattern OP_DIV
	, pattern OP_IDIV
	, pattern OP_BAND
	, pattern OP_BOR
	, pattern OP_BXOR
	, pattern OP_SHL
	, pattern OP_SHR
	, pattern OP_UNM
	, pattern OP_BNOT
	, pattern OP_NOT
	, pattern OP_LEN
	, pattern OP_CONCAT
	, pattern OP_JMP
	, pattern OP_EQ
	, pattern OP_LT
	, pattern OP_LE
	, pattern OP_TEST
	, pattern OP_TESTSET
	, pattern OP_CALL
	, pattern OP_TAILCALL
	, pattern OP_RETURN
	, pattern OP_FORLOOP
	, pattern OP_FORPREP
	, pattern OP_TFORCALL
	, pattern OP_TFORLOOP
	, pattern OP_SETLIST
	, pattern OP_CLOSURE
	, pattern OP_VARARG
	, pattern OP_EXTRAARG
	) where

{-
data C_lua_State

newLuaState :: IO (Ptr C_lua_State, IO ())
newLuaState = do
	let alloc = wrap_C_lua_Alloc $ \ud ptr osize nsize -> do
		if nsize > 0 then reallocBytes ptr nsize
		else do
			free ptr
			return nullPtr
	statePtr <- lua_newstate alloc nullPtr
	return (statePtr, lua_close statePtr)

-- | Load Lua code or bytecode, and return bytecode.
loadLuaByteCode :: Ptr C_lua_State -> T.Text -> B.ByteString -> IO B.ByteString
loadLuaByteCode luaStatePtr chunkName bytes = do
	B.unsafeUseAsCStringLen bytes $ (bytesPtr, bytesLen) -> do
		finishedReadingRef <- newIORef False
		let reader _ _ sizePtr = do
			finishedReading <- readIORef finishedReadingRef
			if finishedReading then do
				poke sizePtr 0
				return nullPtr
			else do
				writeIORef finishedReadingRef True
				poke sizePtr bytesLen
				return bytesPtr
		B.useAsCString (T.encodeUtf8 chunkName) $ \chunkNamePtr -> do
			lua_load luaStatePtr (wrap_C_lua_Reader reader) nullPtr chunkNamePtr nullPtr

type C_lua_Alloc = Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ())
type C_lua_Reader = Ptr C_lua_State -> Ptr () -> CSize -> IO (Ptr CChar)

foreign import ccall safe lua_newstate :: FunPtr C_lua_Alloc -> Ptr () -> IO (Ptr C_lua_State)
foreign import ccall safe lua_close :: Ptr C_lua_State -> IO ()
foreign import ccall safe lua_load :: Ptr C_lua_State -> FunPtr C_lua_Reader -> Ptr () -> Ptr CChar -> Ptr CChar -> IO CInt
foreign import ccall safe lua_dump :: Ptr C_lua_State -> FunPtr C_lua_Writer -> Ptr () -> CInt -> IO CInt

foreign import ccall "wrapper" wrap_C_lua_Alloc :: C_lua_Alloc -> IO (FunPtr C_lua_Alloc)
foreign import ccall "wrapper" wrap_C_lua_Reader :: C_lua_Reader -> IO (FunPtr C_lua_Reader)
-}

pattern LUA_TNIL = 0
pattern LUA_TBOOLEAN = 1
pattern LUA_TLIGHTUSERDATA = 2
pattern LUA_TNUMBER = 3
pattern LUA_TSTRING = 4
pattern LUA_TTABLE = 5
pattern LUA_TFUNCTION = 6
pattern LUA_TUSERDATA = 7
pattern LUA_TTHREAD = 8

pattern LUA_TSHRSTR = 4 -- short strings
pattern LUA_TLNGSTR = 20 -- long strings
pattern LUA_TNUMFLT = 3
pattern LUA_TNUMINT = 19

pattern LUA_SIGNATURE = "\x1bLua"
pattern LUAC_DATA = "\x19\x93\r\n\x1a\n"
pattern LUAC_INT = 0x5678
pattern LUAC_NUM = 370.5

pattern OP_MOVE = 0
pattern OP_LOADK = 1
pattern OP_LOADKX = 2
pattern OP_LOADBOOL = 3
pattern OP_LOADNIL = 4
pattern OP_GETUPVAL = 5
pattern OP_GETTABUP = 6
pattern OP_GETTABLE = 7
pattern OP_SETTABUP = 8
pattern OP_SETUPVAL = 9
pattern OP_SETTABLE = 10
pattern OP_NEWTABLE = 11
pattern OP_SELF = 12
pattern OP_ADD = 13
pattern OP_SUB = 14
pattern OP_MUL = 15
pattern OP_MOD = 16
pattern OP_POW = 17
pattern OP_DIV = 18
pattern OP_IDIV = 19
pattern OP_BAND = 20
pattern OP_BOR = 21
pattern OP_BXOR = 22
pattern OP_SHL = 23
pattern OP_SHR = 24
pattern OP_UNM = 25
pattern OP_BNOT = 26
pattern OP_NOT = 27
pattern OP_LEN = 28
pattern OP_CONCAT = 29
pattern OP_JMP = 30
pattern OP_EQ = 31
pattern OP_LT = 32
pattern OP_LE = 33
pattern OP_TEST = 34
pattern OP_TESTSET = 35
pattern OP_CALL = 36
pattern OP_TAILCALL = 37
pattern OP_RETURN = 38
pattern OP_FORLOOP = 39
pattern OP_FORPREP = 40
pattern OP_TFORCALL = 41
pattern OP_TFORLOOP = 42
pattern OP_SETLIST = 43
pattern OP_CLOSURE = 44
pattern OP_VARARG = 45
pattern OP_EXTRAARG = 46
