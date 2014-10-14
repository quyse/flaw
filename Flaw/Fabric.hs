{-|
Module: Flaw.Fabric
Description: Provides methods for serializing objects into graph.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, GADTs, TypeFamilies #-}

module Flaw.Fabric
	( Hash(..)
	, hash
	, hashBinary
	, HashedFabric(..)
	, Fabricable(..)
	, genPrimFabricable
	, genFabricable
	) where

import Flaw.Fabric.Internal
import Control.Monad
import Data.Binary
import Data.Int
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap

-- | Missing Binary instance for Data.Text.Text.
instance Binary T.Text where
	put = put . T.unpack
	get = liftM T.pack get

-- Instance for list.
instance Fabricable a => Fabricable [a] where
	data Fabric [a] = List_Fabric [HashedFabric a]
	fabric xs = HashedFabric h $ List_Fabric bs where
		bs = map fabric xs
		hs = [h | HashedFabric h _ <- bs]
		h = hashBinary ("[]", hs)

-- Instances for tuples.
instance (Fabricable a, Fabricable b) => Fabricable (a, b) where
	data Fabric (a, b) = Tuple2_Fabric (HashedFabric a) (HashedFabric b)
	fabric (a, b) = HashedFabric h $ Tuple2_Fabric ba bb where
		ba @ (HashedFabric ha _) = fabric a
		bb @ (HashedFabric hb _) = fabric b
		h = hashBinary ("(,)", ha, hb)

-- Instance for Map.
-- Keys are not fabric.
instance (Binary k, Fabricable v) => Fabricable (Map.Map k v) where
	data Fabric (Map.Map k v) = Map_Fabric (Map.Map k (HashedFabric v))
	fabric m = HashedFabric h (Map_Fabric fm) where
		fm = Map.map fabric m
		h = hashBinary ("Map", [(k, hv) | (k, HashedFabric hv v) <- Map.toList fm])

-- Instance for HashMap.
-- Keys are not fabric.
instance (Binary k, Fabricable v) => Fabricable (HashMap.HashMap k v) where
	data Fabric (HashMap.HashMap k v) = HashMap_Fabric (HashMap.HashMap k (HashedFabric v))
	fabric m = HashedFabric h (HashMap_Fabric fm) where
		fm = HashMap.map fabric m
		h = hashBinary ("HashMap", [(k, hv) | (k, HashedFabric hv v) <- HashMap.toList fm])

-- Generate Fabricable instance for some 'primitive' types.
liftM concat $ mapM genPrimFabricable
	[ ''Bool
	, ''Char
	, ''Double
	, ''Float
	, ''Int
	, ''Int8
	, ''Int16
	, ''Int32
	, ''Int64
	, ''Integer
	, ''Word
	, ''Word8
	, ''Word16
	, ''Word32
	, ''Word64
	, ''BS.ByteString
	, ''T.Text
	]
