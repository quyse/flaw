{-|
Module: Main
Description: Tests for Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Main
	( main
	) where

import Control.Monad
import Data.IORef
import Data.Time.Clock

import Flaw.Script.Lua
import Flaw.Script.Lua.Build
import Flaw.Script.Lua.Operations
import Flaw.Script.Lua.StdLib

main :: IO ()
main = do
	let chunk = $(luaCompileFile "src/test.lua")

	env <- luaNewTable
	envRef <- newIORef env

	registerLuaStdLib env

	let registerFunction n f = luaValueSet env (LuaString n) =<< luaNewClosure f

	registerFunction "measure_time" $ \((LuaClosure { luaClosure = f }) : as) -> do
		t1 <- getCurrentTime
		rs <- f as
		t2 <- getCurrentTime
		return $ (LuaReal $ fromRational $ toRational $ diffUTCTime t2 t1) : rs

	void $ chunk envRef [LuaInteger 123, LuaString "hello"]
