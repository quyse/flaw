{-|
Module: Flaw.Editor.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Editor.Entity.Basic
	( registerBasicEntityDeserializators
	) where

import qualified Data.ByteString as B
import Data.Default
import Data.Int
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Serialize.Text()
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word

import Flaw.Editor.Entity

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

-- EntityPtr

entityPtrFirstEntityTypeId :: EntityTypeId
entityPtrFirstEntityTypeId = $(hashTextToEntityTypeId "EntityPtr")

instance Entity a => Entity (EntityPtr a) where
	getEntityTypeId = f undefined where
		f :: Entity a => a -> EntityPtr a -> EntityTypeId
		f u _ = entityPtrFirstEntityTypeId <> getEntityTypeId u
instance Entity a => BasicEntity (EntityPtr a) where
	serializeBasicEntity (EntityPtr underlyingEntityId) = serializeBasicEntity underlyingEntityId
	deserializeBasicEntity = EntityPtr . deserializeBasicEntity

-- Set

setFirstEntityTypeId :: EntityTypeId
setFirstEntityTypeId = $(hashTextToEntityTypeId "Set")

instance (Ord a, BasicEntity a) => Entity (S.Set a) where
	type EntityChange (S.Set a) = (a, Bool)
	getEntityTypeId = f undefined where
		f :: (Entity a) => a -> S.Set a -> EntityTypeId
		f u _ = setFirstEntityTypeId <> getEntityTypeId u
	processEntityChange oldEntity keyBytes valueBytes = result where
		result = if B.null keyBytes || B.head keyBytes /= 0 then Nothing else Just (newEntity, change)
		newEntity = operation oldEntity
		(operation, change) =
			if B.null valueBytes then (S.delete key, (key, False))
			else (S.insert key, (key, True))
		key = deserializeBasicEntity $ B.drop 1 keyBytes
	applyEntityChange var (value, f) = writeEntityVarRecord var (B.singleton 0 <> serializeBasicEntity value) (if f then B.singleton 1 else B.empty)

-- Map

mapFirstEntityTypeId :: EntityTypeId
mapFirstEntityTypeId = $(hashTextToEntityTypeId "Map")

instance (Ord k, BasicEntity k, BasicEntity v) => Entity (M.Map k v) where
	type EntityChange (M.Map k v) = (k, Maybe v)
	getEntityTypeId = f undefined undefined where
		f :: (Entity k, Entity v) => k -> v -> M.Map k v -> EntityTypeId
		f uk uv _ = mapFirstEntityTypeId <> getEntityTypeId uk <> getEntityTypeId uv
	processEntityChange oldEntity keyBytes valueBytes = result where
		result = if B.null keyBytes || B.head keyBytes /= 0 then Nothing else Just (newEntity, change)
		newEntity = operation oldEntity
		(operation, change) =
			if B.null valueBytes then (M.delete key, (key, Nothing))
			else (M.insert key value, (key, Just value))
		key = deserializeBasicEntity $ B.drop 1 keyBytes
		value = deserializeBasicEntity $ B.drop 1 valueBytes
	applyEntityChange var (key, maybeValue) = writeEntityVarRecord var (B.singleton 0 <> serializeBasicEntity key) $ case maybeValue of
		Just value -> B.singleton 0 <> serializeBasicEntity value
		Nothing -> B.empty

registerBasicEntityDeserializators :: EntityManager -> IO ()
registerBasicEntityDeserializators entityManager = do

	-- register EntityPtr's deserializator
	registerBasicOrdEntityType entityManager entityPtrFirstEntityTypeId $ do
		SomeEntity underlyingBaseEntity <- getRootBaseEntity
		let
			setType :: a -> EntityPtr a
			setType _ = EntityPtr nullEntityId
		return $ SomeBasicOrdEntity $ setType underlyingBaseEntity

	-- register Set's deserializator
	registerEntityType entityManager setFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingBaseEntity <- getRootBaseBasicOrdEntity
		let
			setType :: a -> S.Set a
			setType _ = S.empty
		return $ SomeEntity $ setType underlyingBaseEntity

	-- register Map's deserializator
	registerEntityType entityManager mapFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingKeyBaseEntity <- getRootBaseBasicOrdEntity
		SomeBasicEntity underlyingValueBaseEntity <- getRootBaseBasicEntity
		let
			setType :: k -> v -> M.Map k v
			setType _ _ = M.empty
		return $ SomeEntity $ setType underlyingKeyBaseEntity underlyingValueBaseEntity

	-- register basic entities
	f (def :: EntityId)
	f (def :: Int32)
	f (def :: Int64)
	f (def :: Word32)
	f (def :: Word64)
	f (def :: Integer)
	f (def :: B.ByteString)
	f (def :: T.Text)
	where
		f :: (BasicEntity a, Ord a) => a -> IO ()
		f a = registerBasicOrdEntityType entityManager (getEntityTypeId a) $ return $ SomeBasicOrdEntity a
