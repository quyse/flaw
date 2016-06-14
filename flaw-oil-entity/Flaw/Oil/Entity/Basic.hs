{-|
Module: Flaw.Oil.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Oil.Entity.Basic
	( registerBasicEntityDeserializators
	) where

import qualified Data.ByteString as B
import Data.Default
import Data.Int
import Data.Monoid
import qualified Data.Serialize as S
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

entityPtrFirstEntityTypeId :: EntityTypeId
entityPtrFirstEntityTypeId = $(hashTextToEntityTypeId "EntityPtr")

instance Entity a => Entity (EntityPtr a) where
	getEntityTypeId = f undefined where
		f :: Entity a => a -> EntityPtr a -> EntityTypeId
		f u _ = entityPtrFirstEntityTypeId <> getEntityTypeId u
instance Entity a => BasicEntity (EntityPtr a) where
	serializeBasicEntity (EntityPtr underlyingEntityId) = serializeBasicEntity underlyingEntityId
	deserializeBasicEntity = EntityPtr . deserializeBasicEntity

registerBasicEntityDeserializators :: EntityManager -> IO ()
registerBasicEntityDeserializators entityManager = do
	-- register EntityPtr's deserializator
	registerEntityType entityManager entityPtrFirstEntityTypeId $ do
		SomeBaseEntityGetter underlyingBaseEntityGetter <- getBaseEntityGetter
		return $ SomeBaseEntityGetter $ do
			bytes <- S.getBytes =<< S.remaining
			let
				setType :: Entity a => S.Get a -> EntityPtr a
				setType _ = deserializeBasicEntity bytes
			return $ setType underlyingBaseEntityGetter

	-- register basic entities
	f (undefined :: EntityId)
	f (undefined :: Int32)
	f (undefined :: Int64)
	f (undefined :: Word32)
	f (undefined :: Word64)
	f (undefined :: Integer)
	f (undefined :: B.ByteString)
	f (undefined :: T.Text)
	where
		f :: BasicEntity a => a -> IO ()
		f a = registerEntityType entityManager (getEntityTypeId a) $ return $ SomeBaseEntityGetter $ q a <$> (S.getBytes =<< S.remaining)
		q :: BasicEntity a => a -> B.ByteString -> a
		q _ = deserializeBasicEntity
