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

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
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
import Flaw.UI
import Flaw.UI.EditBox
import Flaw.UI.Editor.EditableEntity
import Flaw.UI.Layout
import Flaw.UI.Metrics
import Flaw.UI.Panel

instance Entity EntityId where
	type EntityChange EntityId = EntityId
	getEntityTypeId _ = $(hashTextToEntityTypeId "EntityId")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity EntityId

instance Entity Int32 where
	type EntityChange Int32 = Int32
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int32")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Int32

instance Entity Int64 where
	type EntityChange Int64 = Int64
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int64")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Int64

instance Entity Word32 where
	type EntityChange Word32 = Word32
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word32")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Word32

instance Entity Word64 where
	type EntityChange Word64 = Word64
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word64")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Word64

instance Entity Integer where
	type EntityChange Integer = Integer
	getEntityTypeId _ = $(hashTextToEntityTypeId "Integer")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Integer

instance Entity B.ByteString where
	type EntityChange B.ByteString = B.ByteString
	getEntityTypeId _ = $(hashTextToEntityTypeId "ByteString")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity B.ByteString
instance Default B.ByteString where
	def = B.empty

instance Entity T.Text where
	type EntityChange T.Text = T.Text
	getEntityTypeId _ = $(hashTextToEntityTypeId "Text")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
instance BasicEntity T.Text
instance Default T.Text where
	def = T.empty
instance EditableEntity T.Text where
	editableEntityTypeName _ = "Text"
	editableEntityConstructorName _ = "Text"
	editableEntityLayout initialEntity setter = ReaderT $ \EditableLayoutState {} -> do
		currentValueVar <- lift $ newTVar initialEntity
		panel <- lift $ newPanel False
		editBox <- lift newEditBox
		lift $ setText editBox initialEntity
		_editBoxChild <- lift $ addFreeChild panel editBox
		lift $ setLayoutHandler panel $ layoutElement editBox
		FlowLayoutState
			{ flsMetrics = metrics
			} <- get
		elementWithSizeInFlowLayout panel (preferredSize metrics editBox)
		lift $ setCommitHandler panel $ \commitReason -> do
			if commitReason == CommitAccept || commitReason == CommitLostFocus then do
				value <- getText editBox
				currentValue <- readTVar currentValueVar
				when (value /= currentValue) $ do
					writeTVar currentValueVar value
					setter value
			else setText editBox =<< readTVar currentValueVar
			return True
		return $ \newValue _change -> do
			-- check that it's not equal to current value
			currentValue <- readTVar currentValueVar
			when (newValue /= currentValue) $ do
				-- in any case remember new current value
				writeTVar currentValueVar newValue
				-- change text in edit box only if it's not changed
				value <- getText editBox
				when (value == currentValue) $ setText editBox newValue

-- EntityPtr

entityPtrFirstEntityTypeId :: EntityTypeId
entityPtrFirstEntityTypeId = $(hashTextToEntityTypeId "EntityPtr")

instance Entity a => Entity (EntityPtr a) where
	type EntityChange (EntityPtr a) = EntityPtr a
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	getEntityTypeId = f undefined where
		f :: Entity a => a -> EntityPtr a -> EntityTypeId
		f u _ = entityPtrFirstEntityTypeId <> getEntityTypeId u
instance Entity a => BasicEntity (EntityPtr a) where
	serializeBasicEntity (EntityPtr underlyingEntityId) = serializeBasicEntity underlyingEntityId
	deserializeBasicEntity = EntityPtr . deserializeBasicEntity

-- InterfacedEntityPtr

interfacedEntityPtrFirstEntityTypeId :: EntityTypeId
interfacedEntityPtrFirstEntityTypeId = $(hashTextToEntityTypeId "InterfacedEntityPtr")

instance EntityInterface i => Entity (InterfacedEntityPtr i) where
	type EntityChange (InterfacedEntityPtr i) = InterfacedEntityPtr i
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	getEntityTypeId = f Proxy where
		f :: EntityInterface i => Proxy i -> InterfacedEntityPtr i -> EntityTypeId
		f proxy _ = let
			EntityInterfaceId entityInterfaceIdBytes = getEntityInterfaceId proxy
			in interfacedEntityPtrFirstEntityTypeId <> EntityTypeId entityInterfaceIdBytes
instance EntityInterface i => BasicEntity (InterfacedEntityPtr i) where
	serializeBasicEntity (InterfacedEntityPtr underlyingEntityId) = serializeBasicEntity underlyingEntityId
	deserializeBasicEntity = InterfacedEntityPtr . deserializeBasicEntity

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

	-- register InterfacedEntityPtr's deserializator
	registerBasicOrdEntityType entityManager interfacedEntityPtrFirstEntityTypeId $ do
		SomeEntityInterface proxy <- deserializeEntityInterface
		let
			setType :: Proxy i -> InterfacedEntityPtr i
			setType Proxy = InterfacedEntityPtr nullEntityId
		return $ SomeBasicOrdEntity $ setType proxy

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
