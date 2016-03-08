{-|
Module: Flaw.Script.Lua
Description: Lua implementation in Haskell.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.Script.Lua
	( LuaMonad(..)
	, LuaValue(..)
	, LuaError(..)
	, LuaLoadError(..)
	) where

import Control.Exception
import Control.Monad.Primitive
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Primitive.MutVar
import qualified Data.Text as T
import Data.Typeable
import Data.Unique

class PrimMonad m => LuaMonad m where
	newLuaUnique :: m Unique
	throwLuaError :: LuaError m -> m a
	catchLuaError :: m a -> (LuaError m -> m a) -> m a

instance LuaMonad IO where
	newLuaUnique = newUnique
	throwLuaError = throwIO
	catchLuaError = catch

-- | Lua value.
data LuaValue m where
	-- Standard 'nil' value.
	LuaNil :: LuaValue m
	-- Standard boolean value.
	LuaBoolean :: !Bool -> LuaValue m
	-- Integer 'number' value.
	LuaInteger :: {-# UNPACK #-} !Int -> LuaValue m
	-- Real 'number' value.
	LuaReal :: {-# UNPACK #-} !Double -> LuaValue m
	-- String value.
	LuaString :: !T.Text -> LuaValue m
	-- Lua function
	LuaClosure ::
		{ luaClosureUnique :: !Unique
		, luaClosure :: !([LuaValue m] -> m [LuaValue m])
		} -> LuaValue m
	-- User data.
	LuaUserData ::
		{ luaUserDataUnique :: !Unique
		, luaUserData :: !a
		} -> LuaValue m
	LuaTable ::
		{ luaTableUnique :: !Unique
		, luaTable :: !(HT.HashTable (PrimState m) (LuaValue m) (LuaValue m))
		, luaTableLength :: !(MutVar (PrimState m) Int)
		, luaTableMetaTable :: !(MutVar (PrimState m) (LuaValue m))
		} -> LuaValue m

instance Eq (LuaValue m) where
	{-# INLINABLE (==) #-}
	LuaNil == LuaNil = True
	LuaBoolean a == LuaBoolean b = a == b
	LuaInteger a == LuaInteger b = a == b
	LuaReal a == LuaReal b = a == b
	LuaString a == LuaString b = a == b
	LuaClosure { luaClosureUnique = a } == LuaClosure { luaClosureUnique = b } = a == b
	LuaUserData { luaUserDataUnique = a } == LuaUserData { luaUserDataUnique = b } = a == b
	LuaTable { luaTableUnique = a } == LuaTable { luaTableUnique = b } = a == b
	_ == _ = False

instance Hashable (LuaValue m) where
	{-# INLINABLE hashWithSalt #-}
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

instance Show (LuaValue m) where
	showsPrec p v q = case v of
		LuaNil -> "LuaNil" ++ q
		LuaBoolean b -> enclose $ \qq -> "LuaBoolean " ++ showsPrec 10 b qq
		LuaInteger i -> enclose $ \qq -> "LuaInteger " ++ showsPrec 10 i qq
		LuaReal r -> enclose $ \qq -> "LuaReal " ++ showsPrec 10 r qq
		LuaString t -> enclose $ \qq -> "LuaString " ++ showsPrec 10 t qq
		LuaClosure
			{ luaClosureUnique = u
			} -> enclose $ \qq -> "LuaClosure { luaClosureUnique = " ++ shows (hashUnique u) qq
		LuaUserData
			{ luaUserDataUnique = u
			} -> enclose $ \qq -> "LuaUserData { luaUserDataUnique = " ++ shows (hashUnique u) qq
		LuaTable
			{ luaTableUnique = u
			} -> enclose $ \qq -> "LuaTable { luaTableUnique = " ++ shows (hashUnique u) qq
		where enclose f = if p >= 10 then '(' : f (')' : q) else f q

data LuaError m
	-- | Standard Lua error (e.g. thrown by 'error' stdlib function).
	= LuaError !(LuaValue m)
	-- | Operation is called on unsupported value, and value
	-- doesn't have metatable, or doesn't have specific metamethod.
	| LuaBadOperation !T.Text
	deriving Show

instance Typeable m => Exception (LuaError m)

-- | Error while loading Lua chunk.
data LuaLoadError
	= LuaLoadError !T.Text
	deriving Show

instance Exception LuaLoadError
