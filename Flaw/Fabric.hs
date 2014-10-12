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

-- | Missing Binary instance for Data.Text.Text.
instance Binary T.Text where
	put = put . T.unpack
	get = liftM T.pack get

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
