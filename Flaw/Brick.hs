{-|
Module: Flaw.Brick
Description: Provides methods for serializing objects into graph.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, GADTs, TypeFamilies #-}

module Flaw.Brick
	( Hash(..)
	, hash
	, hashBinary
	, HashedBrick(..)
	, Brickable(..)
	, genPrimBrickable
	, genBrickable
	) where

import Flaw.BrickInternal
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

-- Generate Brickable instance for some 'primitive' types.
liftM concat $ mapM genPrimBrickable
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
