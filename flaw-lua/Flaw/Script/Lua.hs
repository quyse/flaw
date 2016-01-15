{-|
Module: Flaw.Script.Lua
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.Script.Lua
	( LuaValue(..)
	, LuaError(..)
	) where

import Control.Exception
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
	LuaReal :: {-# UNPACK #-} !Double -> LuaValue
	-- | String value.
	LuaString :: !T.Text -> LuaValue
	-- | Lua function
	LuaClosure ::
		{ luaClosureUnique :: !Unique
		, luaClosure :: !([LuaValue] -> IO [LuaValue])
		} -> LuaValue
	-- | User data.
	LuaUserData ::
		{ luaUserDataUnique :: !Unique
		, luaUserData :: !a
		} -> LuaValue
	LuaTable ::
		{ luaTableUnique :: !Unique
		, luaTable :: !(HT.CuckooHashTable LuaValue LuaValue)
		, luaTableLength :: !(IORef Int)
		, luaTableMetaTable :: !(IORef LuaValue)
		} -> LuaValue

instance Eq LuaValue where
	LuaNil == LuaNil = True
	LuaBoolean a == LuaBoolean b = a == b
	LuaInteger a == LuaInteger b = a == b
	LuaReal a == LuaReal b = a == b
	LuaString a == LuaString b = a == b
	LuaClosure { luaClosureUnique = a } == LuaClosure { luaClosureUnique = b } = a == b
	LuaUserData { luaUserDataUnique = a } == LuaUserData { luaUserDataUnique = b } = a == b
	LuaTable { luaTableUnique = a } == LuaTable { luaTableUnique = b } = a == b
	_ == _ = False

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
		LuaUserData
			{ luaUserDataUnique = u
			} -> s `hashWithSalt` (6 :: Int) `hashWithSalt` hashUnique u
		LuaTable
			{ luaTableUnique = u
			} -> s `hashWithSalt` (7 :: Int) `hashWithSalt` hashUnique u

instance Show LuaValue where
	showsPrec p v q = case v of
		LuaNil -> "LuaNil" ++ q
		LuaBoolean b -> enclose $ \qq -> "LuaBoolean " ++ showsPrec 10 b qq
		LuaInteger i -> enclose $ \qq -> "LuaInteger " ++ showsPrec 10 i qq
		LuaReal r -> enclose $ \qq -> "LuaReal " ++ showsPrec 10 r qq
		LuaString t -> enclose $ \qq -> "LuaString " ++ showsPrec 10 t qq
		LuaClosure
			{ luaClosureUnique = u
			} -> enclose $ \qq -> "LuaClosure { luaClosureUnique = " ++ showsPrec 0 (hashUnique u) qq
		LuaUserData
			{ luaUserDataUnique = u
			} -> enclose $ \qq -> "LuaUserData { luaUserDataUnique = " ++ showsPrec 0 (hashUnique u) qq
		LuaTable
			{ luaTableUnique = u
			} -> enclose $ \qq -> "LuaTable { luaTableUnique = " ++ showsPrec 0 (hashUnique u) qq
		where enclose f = if p >= 10 then '(' : f (')' : q) else f q

data LuaError
	-- | Standard Lua error (e.g. thrown by 'error' stdlib function).
	= LuaError !LuaValue
	-- | Operation is called on unsupported value, and value
	-- doesn't have metatable, or doesn't have specific metamethod.
	| LuaBadOperation !T.Text
	deriving Show

instance Exception LuaError
