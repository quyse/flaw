{-|
Module: Flaw.Script.Lua
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.Script.Lua
	( LuaValue(..)
	) where

import Data.Hashable
import qualified Data.HashTable.IO as HT
import Data.IORef
import qualified Data.Text as T
import Data.Unique

-- | Lua value.
data LuaValue where
	-- | Standard 'nil' value.
	LuaNil :: LuaValue
	-- | Standard boolean value.
	LuaBoolean :: !Bool -> LuaValue
	-- | Integer 'number' value.
	LuaInteger :: {-# UNPACK #-} !Int -> LuaValue
	-- | Real 'number' value.
	LuaReal :: {-# UNPACK #-} !Float -> LuaValue
	-- | String value.
	LuaString :: !T.Text -> LuaValue
	-- | Lua function
	LuaClosure ::
		{ luaClosureUnique :: !Unique
		, luaClosure :: !([LuaValue] -> IO [LuaValue])
		} -> LuaValue
	-- | Full user data.
	LuaFullUserData ::
		{ luaFullUserDataUnique :: !Unique
		, luaFullUserData :: !a
		, luaFullUserDataMetaTable :: !(IORef LuaValue)
		} -> LuaValue
	-- | Light user data.
	LuaLightUserData :: Hashable a => !a -> LuaValue
	LuaThread :: LuaValue
	LuaTable ::
		{ luaTableUnique :: !Unique
		, luaTable :: !(IORef (HT.CuckooHashTable LuaValue LuaValue))
		, luaTableMetaTable :: !(IORef LuaValue)
		} -> LuaValue

instance Hashable LuaValue where
	{-# INLINE hashWithSalt #-}
	hashWithSalt s v = case v of
		LuaNil -> s `hashWithSalt` (0 :: Int)
		LuaBoolean b -> s `hashWithSalt` (1 :: Int) `hashWithSalt` b
		LuaInteger i -> s `hashWithSalt` (2 :: Int) `hashWithSalt` i
		LuaReal r -> s `hashWithSalt` (3 :: Int) `hashWithSalt` r
		LuaString t -> s `hashWithSalt` (4 :: Int) `hashWithSalt` t
		LuaClosure
			{ luaClosureUnique = u
			} -> s `hashWithSalt` (5 :: Int) `hashWithSalt` hashUnique u
		LuaFullUserData
			{ luaFullUserDataUnique = u
			} -> s `hashWithSalt` (6 :: Int) `hashWithSalt` hashUnique u
		LuaLightUserData a -> s `hashWithSalt` (7 :: Int) `hashWithSalt` a
		LuaThread -> s `hashWithSalt` (8 :: Int)
		LuaTable
			{ luaTableUnique = u
			} -> s `hashWithSalt` (9 :: Int) `hashWithSalt` hashUnique u
