{-|
Module: Flaw.Script.Lua.StdLib
Description: Lua standard library.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.Script.Lua.StdLib
	( registerLuaStdLib
	) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.HashTable.IO as HT
import qualified Data.Text as T
import Data.IORef

import Flaw.Script.Lua
import Flaw.Script.Lua.Operations

registerLuaStdLib :: LuaValue -> IO ()
registerLuaStdLib env@LuaTable
	{ luaTable = envt
	} = do

	let func n f = do
		c <- luaNewClosure f
		HT.insert envt (LuaString n) c

	let notImplementedFunc n = func n $ \_ -> throwIO $ LuaError $ LuaString $ "flaw-lua: stdlib " <> n <> " is not implemented"

	let
		adjustArgs :: Int -> [LuaValue] -> [LuaValue]
		adjustArgs 0 _ = []
		adjustArgs n (x:xs) = x : adjustArgs (n - 1) xs
		adjustArgs n [] = LuaNil : adjustArgs (n - 1) []

	-- basic functions

	func "assert" $ \as@(x:xs) -> if luaCoerceToBool x then return as
		else throwIO $ LuaError $ case xs of
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
				_ -> throwIO $ LuaError $ LuaString "collectgarbage: wrong opt"
			Nothing -> throwIO $ LuaError $ LuaString "collectgarbage: wrong opt"

	notImplementedFunc "dofile"

	func "error" $ \(msg:_) -> throwIO $ LuaError msg

	HT.insert envt (LuaString "_G") env

	func "getmetatable" $ \(obj:_) -> case obj of
		LuaTable
			{ luaTableMetaTable = mtRef
			} -> do
			mt <- readIORef mtRef
			case mt of
				LuaTable
					{ luaTable = mtt
					} -> do
					maybeMttMt <- HT.lookup mtt (LuaString "__metatable")
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
				mv <- HT.lookup t k
				case mv of
					Just v -> return [k, v]
					Nothing -> return []
			_ -> \_ -> return []
		return [f, x, LuaInteger 0]

	notImplementedFunc "load"

	notImplementedFunc "loadfile"

	notImplementedFunc "next"

	notImplementedFunc "pairs"

	func "pcall" $ \(f:as) -> catch (liftM (LuaBoolean True : ) $ luaValueCall f as) $ \(SomeException e) ->
		return [LuaBoolean False, LuaString $ T.pack $ show e]

	func "print" $ \as -> do
		forM_ as $ \a -> case luaCoerceToString a of
			Just s -> putStr $ T.unpack s
			Nothing -> putStr "<<???>>"
		return []

	func "rawequal" $ \(a:b:_) -> return [LuaBoolean $ a == b]

	func "rawget" $ \(t:i:_) -> case t of
		LuaTable
			{ luaTable = tt
			} -> do
			r <- HT.lookup tt i
			return [fromMaybe LuaNil r]
		_ -> return [LuaNil]

	func "rawlen" $ \(t:_) -> case t of
		LuaString s -> return [LuaInteger $ T.length s]
		LuaTable
			{ luaTable = tt
			} -> do
			r <- HT.toList tt -- FIXME: slow
			return [LuaInteger $ length r]
		_ -> throwIO $ LuaBadOperation "rawlen: table or string expected"

	func "rawset" $ \(t:i:v:_) -> case t of
		LuaTable
			{ luaTable = tt
			} -> do
			HT.insert tt i v
			return [t]
		_ -> throwIO $ LuaBadOperation "rawset: table expected"

	func "select" $ \(n:as) -> case n of
		LuaInteger i -> if i == 0 then throwIO $ LuaBadOperation "select: zero index"
			else return $ if i > 0 then drop (i - 1) as else drop (length as + i) as
		LuaString "#" -> return [LuaInteger $ length as]
		_ -> throwIO $ LuaBadOperation "select: non-zero index or string '#' expected"

	func "setmetatable" $ \(t:mt:_) -> case t of
		LuaTable
			{ luaTableMetaTable = mtRef
			} -> do
			omt <- readIORef mtRef
			case omt of
				LuaTable
					{ luaTable = omtt
					} -> do
					momtmt <- HT.lookup omtt $ LuaString "__metatable"
					case momtmt of
						Just _ -> throwIO $ LuaError $ LuaString "setmetatable: table has __metatable metafield"
						Nothing -> return ()
				_ -> return ()
			writeIORef mtRef mt
			return [t]
		_ -> throwIO $ LuaError $ LuaString "setmetatable: not a table"

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

	HT.insert envt (LuaString "_VERSION") $ LuaString "Lua 5.3"

	notImplementedFunc "xpcall"

registerLuaStdLib _ = undefined
