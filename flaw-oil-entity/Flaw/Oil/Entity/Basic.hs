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
import qualified Data.Map.Strict as M
import Data.Default
import Data.Int
import Data.Monoid
import Data.Serialize.Text()
import qualified Data.Text as T
import Data.Word

import Flaw.Oil.Entity

instance Entity a => Entity (EntityPtr a) where
	getEntityTypeId _ = $(hashTextToEntityTypeId "EntityPtr")
instance Entity a => BasicEntity (EntityPtr a) where
	serializeBasicEntity = let
		f :: Entity a => a -> EntityPtr a -> B.ByteString
		f u (EntityPtr (EntityId underlyingEntityIdBytes)) = let
			EntityTypeId underlyingEntityTypeIdBytes = getEntityTypeId u
			in underlyingEntityTypeIdBytes <> underlyingEntityIdBytes
		in f undefined
	deserializeBasicEntity _ = error "EntityPtr cannot deserialize through deserializeBasicEntity"

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

registerBasicEntityDeserializators :: EntityManager -> IO ()
registerBasicEntityDeserializators entityManager = do
	-- register EntityPtr's deserializator
	registerEntityType entityManager (getEntityTypeId (undefined :: EntityPtr NullEntity)) $ Deserializator $ \deserializators value -> let
		(underlyingEntityTypeIdBytes, moreBytes) = B.splitAt ENTITY_TYPE_ID_SIZE value
		underlyingEntityId = if B.length moreBytes >= ENTITY_ID_SIZE then EntityId (B.take ENTITY_ID_SIZE moreBytes) else nullEntityId
		setPtrType :: a -> EntityPtr a
		setPtrType _ = EntityPtr underlyingEntityId
		in
		if B.length value >= ENTITY_TYPE_ID_SIZE then
			case M.lookup (EntityTypeId underlyingEntityTypeIdBytes) deserializators of
				Just (Deserializator deserializator) -> case deserializator deserializators B.empty of
					SomeEntity underlyingEntity -> SomeEntity $ setPtrType underlyingEntity
				Nothing -> SomeEntity (EntityPtr underlyingEntityId :: EntityPtr NullEntity)
		else SomeEntity (EntityPtr nullEntityId :: EntityPtr NullEntity)

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
		f a = registerEntityType entityManager (getEntityTypeId a) $ Deserializator $ \_deserializators -> SomeEntity . q a
		q :: BasicEntity a => a -> B.ByteString -> a
		q _ = deserializeBasicEntity
