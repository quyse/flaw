{-|
Module: Flaw.Oil.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Oil.Entity.Basic
	( basicEntityDeserializators
	) where

import Data.Int
import qualified Data.ByteString as B
import Data.Default
import Data.Serialize.Text()
import qualified Data.Text as T
import Data.Word

import Flaw.Oil.Entity

instance Entity EntityId where
	getEntityTypeId _ = $(hashTextToEntityTypeId "EntityId")
instance BasicEntity EntityId

instance Entity Int32 where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int32")
instance BasicEntity Int32

instance Entity Int64 where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int64")
instance BasicEntity Int64

instance Entity Word32 where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word32")
instance BasicEntity Word32

instance Entity Word64 where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word64")
instance BasicEntity Word64

instance Entity Integer where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Integer")
instance BasicEntity Integer

instance Entity B.ByteString where
	getEntityTypeId _ = $(hashTextToEntityTypeId "ByteString")
instance BasicEntity B.ByteString
instance Default B.ByteString where
	def = B.empty

instance Entity T.Text where
	getEntityTypeId _ = $(hashTextToEntityTypeId "Text")
instance BasicEntity T.Text
instance Default T.Text where
	def = T.empty

basicEntityDeserializators :: [(EntityTypeId, Deserializator)]
basicEntityDeserializators =
	[ f (undefined :: EntityId)
	, f (undefined :: Int32)
	, f (undefined :: Int64)
	, f (undefined :: Word32)
	, f (undefined :: Word64)
	, f (undefined :: Integer)
	, f (undefined :: B.ByteString)
	, f (undefined :: T.Text)
	]
	where
		f :: Entity a => a -> (EntityTypeId, Deserializator)
		f a = (getEntityTypeId a, (SomeEntity <$>) . q a)
		q :: Entity a => a -> (B.ByteString -> IO B.ByteString) -> IO a
		q _ = deserializeEntity
