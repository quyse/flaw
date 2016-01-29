{-|
Module: Flaw.Script.Lua.StdLib
Description: Lua standard library.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Flaw.Script.Lua.StdLib
	( registerLuaBasicLib
	) where

import Control.Monad
import Control.Monad.Primitive
import Data.Maybe
import Data.Monoid
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Primitive.MutVar
import qualified Data.Text as T
import Debug.Trace

import Flaw.Script.Lua
import Flaw.Script.Lua.Build
import Flaw.Script.Lua.Operations

registerLuaBasicLib :: LuaMonad m => LuaValue m -> m ()
registerLuaBasicLib env@LuaTable
	{ luaTable = envt
	} = do

	envVar <- newMutVar env

	let func n f = do
		c <- luaNewClosure f
		liftPrim $ HT.insert envt (LuaString n) c

	let notImplementedFunc n = func n $ \_ -> throwLuaError $ LuaError $ LuaString $ "flaw-lua: stdlib " <> n <> " is not implemented"

	let
		adjustArgs :: Int -> [LuaValue m] -> [LuaValue m]
		adjustArgs 0 _ = []
		adjustArgs n (x:xs) = x : adjustArgs (n - 1) xs
		adjustArgs n [] = LuaNil : adjustArgs (n - 1) []

	-- basic functions

	func "assert" $ \as@(x:xs) -> if luaCoerceToBool x then return as
		else throwLuaError $ LuaError $ case xs of
			msg:_ -> msg
			_ -> LuaString "assertion failed!"

	func "collectgarbage" $ \as -> let
		[opt, _arg] = adjustArgs 2 as
		in case luaCoerceToString opt of
			Just s -> case s of
				"collect" -> return []
				"stop" -> return []
				"restart" -> return []
				"count" -> return [LuaReal 0]
				"step" -> return [LuaBoolean False]
				"setpause" -> return [LuaInteger 100]
				"setstepmul" -> return [LuaInteger 200]
				"isrunning" -> return [LuaBoolean True]
				_ -> throwLuaError $ LuaError $ LuaString "collectgarbage: wrong opt"
			Nothing -> throwLuaError $ LuaError $ LuaString "collectgarbage: wrong opt"

	func "dofile" $ [lua|
		local fileName = ...
		return loadfile(fileName)()
		|] envVar

	func "error" $ \(msg:_) -> throwLuaError $ LuaError msg

	liftPrim $ HT.insert envt (LuaString "_G") env

	func "getmetatable" $ \(obj:_) -> case obj of
		LuaTable
			{ luaTableMetaTable = mtVar
			} -> do
			mt <- readMutVar mtVar
			case mt of
				LuaTable
					{ luaTable = mtt
					} -> do
					maybeMttMt <- liftPrim $ HT.lookup mtt (LuaString "__metatable")
					case maybeMttMt of
						Just mttmt -> return [mttmt]
						Nothing -> return [mt]
				_ -> return [LuaNil]
		_ -> return [LuaNil]

	func "ipairs" $ \(x:_) -> do
		f <- luaNewClosure $ case x of
			LuaTable
				{ luaTable = t
				} -> \[_t, LuaInteger i] -> do
				let k = LuaInteger $ i + 1
				mv <- liftPrim $ HT.lookup t k
				case mv of
					Just v -> return [k, v]
					Nothing -> return []
			_ -> \_ -> return []
		return [f, x, LuaInteger 0]

	notImplementedFunc "load"

	func "loadfile" $ [lua|
		local fileName = ...
		return _chunks[fileName]
		|] envVar

	notImplementedFunc "next"

	notImplementedFunc "pairs"

	func "pcall" $ \(f:as) -> catchLuaError (liftM (LuaBoolean True : ) $ luaValueCall f as) $ \e ->
		return [LuaBoolean False, LuaString $ T.pack $ show e]

	func "print" $ \as -> do
		forM_ as $ \a -> case luaCoerceToString a of
			Just s -> traceM $ T.unpack s
			Nothing -> traceM "<<???>>"
		return []

	func "rawequal" $ \(a:b:_) -> return [LuaBoolean $ a == b]

	func "rawget" $ \(t:i:_) -> case t of
		LuaTable
			{ luaTable = tt
			} -> do
			r <- liftPrim $ HT.lookup tt i
			return [fromMaybe LuaNil r]
		_ -> return [LuaNil]

	func "rawlen" $ \(t:_) -> case t of
		LuaString s -> return [LuaInteger $ T.length s]
		LuaTable
			{ luaTableLength = lenVar
			} -> do
			r <- readMutVar lenVar
			return [LuaInteger r]
		_ -> throwLuaError $ LuaBadOperation "rawlen: table or string expected"

	func "rawset" $ \(t:i:v:_) -> case t of
		LuaTable
			{ luaTable = tt
			, luaTableLength = lenVar
			} -> do
			mv <- liftPrim $ HT.lookup tt i
			case mv of
				Just _ -> case v of
					LuaNil -> do
						liftPrim $ HT.delete tt i
						modifyMutVar' lenVar (+ (-1))
					_ -> liftPrim $ HT.insert tt i v
				Nothing -> case v of
					LuaNil -> return ()
					_ -> do
						liftPrim $ HT.insert tt i v
						modifyMutVar' lenVar (+ 1)
			return [t]
		_ -> throwLuaError $ LuaBadOperation "rawset: table expected"

	func "select" $ \(n:as) -> case n of
		LuaInteger i -> if i == 0 then throwLuaError $ LuaBadOperation "select: zero index"
			else return $ if i > 0 then drop (i - 1) as else drop (length as + i) as
		LuaString "#" -> return [LuaInteger $ length as]
		_ -> throwLuaError $ LuaBadOperation "select: non-zero index or string '#' expected"

	func "setmetatable" $ \(t:mt:_) -> case t of
		LuaTable
			{ luaTableMetaTable = mtVar
			} -> do
			omt <- readMutVar mtVar
			case omt of
				LuaTable
					{ luaTable = omtt
					} -> do
					momtmt <- liftPrim $ HT.lookup omtt $ LuaString "__metatable"
					case momtmt of
						Just _ -> throwLuaError $ LuaError $ LuaString "setmetatable: table has __metatable metafield"
						Nothing -> return ()
				_ -> return ()
			writeMutVar mtVar mt
			return [t]
		_ -> throwLuaError $ LuaError $ LuaString "setmetatable: not a table"

	func "tonumber" $ \(x:_) -> let
		r = case luaCoerceToNumber x of
			Just n -> LuaReal n
			Nothing -> LuaNil
		in return [r]

	func "tostring" $ \(x:_) -> let
		r = case luaCoerceToString x of
			Just s -> LuaString s
			Nothing -> LuaNil
		in return [r]

	func "type" $ \(x:_) -> let
		t = case x of
			LuaNil -> "nil"
			LuaBoolean _ -> "boolean"
			LuaInteger _ -> "number"
			LuaReal _ -> "number"
			LuaString _ -> "string"
			LuaClosure {} -> "function"
			LuaUserData {} -> "userdata"
			LuaTable {} -> "table"
		in return [LuaString t]

	liftPrim $ HT.insert envt (LuaString "_VERSION") $ LuaString "Lua 5.3"

	notImplementedFunc "xpcall"

registerLuaBasicLib _ = undefined
