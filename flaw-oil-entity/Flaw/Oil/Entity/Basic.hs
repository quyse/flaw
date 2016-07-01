{-|
Module: Flaw.Oil.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Oil.Entity.Basic
	( writeInsertMapEntityVar
	, writeDeleteMapEntityVar
	, registerBasicEntityDeserializators
	) where

import Control.Concurrent.STM
import qualified Data.ByteString as B
import Data.Default
import Data.Int
import qualified Data.Map.Strict as M
import Data.Monoid
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

-- Map

mapFirstEntityTypeId :: EntityTypeId
mapFirstEntityTypeId = $(hashTextToEntityTypeId "Map")

instance (Ord k, BasicEntity k, BasicEntity v) => Entity (M.Map k v) where
	type EntityChange (M.Map k v) = Maybe (k, Maybe v)
	getEntityTypeId = f undefined undefined where
		f :: (Entity k, Entity v) => k -> v -> M.Map k v -> EntityTypeId
		f uk uv _ = mapFirstEntityTypeId <> getEntityTypeId uk <> getEntityTypeId uv
	processEntityChange oldEntity keyBytes valueBytes = result where
		result = if B.null keyBytes || B.head keyBytes /= 0 then (oldEntity, Nothing) else (newEntity, change)
		newEntity = operation oldEntity
		(operation, change) =
			if B.null valueBytes then (M.delete key, Just (key, Nothing))
			else (M.insert key value, Just (key, Just value))
		key = deserializeBasicEntity $ B.drop 1 keyBytes
		value = deserializeBasicEntity $ B.drop 1 valueBytes
	applyEntityChange mc m = case mc of
		Just (k, mv) -> case mv of
			Just v -> M.insert k v m
			Nothing -> M.delete k m
		Nothing -> m

writeInsertMapEntityVar :: (Ord k, BasicEntity k, BasicEntity v) => EntityVar (M.Map k v) -> k -> v -> STM ()
writeInsertMapEntityVar var key value = writeEntityVarRecord var (B.singleton 0 <> serializeBasicEntity key) (B.singleton 0 <> serializeBasicEntity value)

writeDeleteMapEntityVar :: (Ord k, BasicEntity k, BasicEntity v) => EntityVar (M.Map k v) -> k -> STM ()
writeDeleteMapEntityVar var key = writeEntityVarRecord var (B.singleton 0 <> serializeBasicEntity key) B.empty

registerBasicEntityDeserializators :: EntityManager -> IO ()
registerBasicEntityDeserializators entityManager = do

	-- register EntityPtr's deserializator
	registerBasicOrdEntityType entityManager entityPtrFirstEntityTypeId $ do
		SomeEntity underlyingBaseEntity <- getRootBaseEntity
		let
			setType :: a -> EntityPtr a
			setType _ = EntityPtr nullEntityId
		return $ SomeBasicOrdEntity $ setType underlyingBaseEntity

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
