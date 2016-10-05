{-|
Module: Flaw.Editor.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Editor.Entity.Basic
	(
	) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Default
import Data.Int
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Serialize.Text()
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Numeric
import Text.Read(readMaybe)

import Flaw.Editor.Entity
import Flaw.UI
import Flaw.UI.CheckBox
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
instance EditableEntity EntityId where
	editableEntityTypeName _ = "EntityId"
	editableEntityConstructorName _ = "EntityId"
	editableEntityLayout = editBoxEditableLayout
		(\(EntityId bytes) -> T.decodeUtf8 $ BA.convertToBase BA.Base64URLUnpadded $ BS.fromShort bytes)
		(either (const Nothing) (Just . EntityId . BS.toShort) . BA.convertFromBase BA.Base64URLUnpadded . T.encodeUtf8)

instance Entity Int32 where
	type EntityChange Int32 = Int32
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int32")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Int32
instance EditableEntity Int32 where
	editableEntityTypeName _ = "Int32"
	editableEntityConstructorName _ = "Int32"
	editableEntityLayout = editBoxShowReadEditableLayout

instance Entity Int64 where
	type EntityChange Int64 = Int64
	getEntityTypeId _ = $(hashTextToEntityTypeId "Int64")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Int64
instance EditableEntity Int64 where
	editableEntityTypeName _ = "Int64"
	editableEntityConstructorName _ = "Int64"
	editableEntityLayout = editBoxShowReadEditableLayout

instance Entity Word32 where
	type EntityChange Word32 = Word32
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word32")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Word32
instance EditableEntity Word32 where
	editableEntityTypeName _ = "Word32"
	editableEntityConstructorName _ = "Word32"
	editableEntityLayout = editBoxShowReadEditableLayout

instance Entity Word64 where
	type EntityChange Word64 = Word64
	getEntityTypeId _ = $(hashTextToEntityTypeId "Word64")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Word64
instance EditableEntity Word64 where
	editableEntityTypeName _ = "Word64"
	editableEntityConstructorName _ = "Word64"
	editableEntityLayout = editBoxShowReadEditableLayout

instance Entity Integer where
	type EntityChange Integer = Integer
	getEntityTypeId _ = $(hashTextToEntityTypeId "Integer")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Integer
instance EditableEntity Integer where
	editableEntityTypeName _ = "Integer"
	editableEntityConstructorName _ = "Integer"
	editableEntityLayout = editBoxShowReadEditableLayout

instance Entity Float where
	type EntityChange Float = Float
	getEntityTypeId _ = $(hashTextToEntityTypeId "Float")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Float
instance EditableEntity Float where
	editableEntityTypeName _ = "Float"
	editableEntityConstructorName _ = "Float"
	editableEntityLayout = editBoxEditableLayout (\n -> T.pack $ showFFloat Nothing n "") (readMaybe . T.unpack)

instance Entity Bool where
	type EntityChange Bool = Bool
	getEntityTypeId _ = $(hashTextToEntityTypeId "Bool")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity Bool
instance Default Bool where
	def = False
instance EditableEntity Bool where
	editableEntityTypeName _ = "Bool"
	editableEntityConstructorName _ = "Bool"
	editableEntityLayout initialEntity setter = ReaderT $ \EditableLayoutState {} -> do
		currentValueVar <- lift $ newTVar initialEntity
		checkBox <- lift $ newLabeledCheckBox "enabled"
		lift $ setChecked checkBox initialEntity
		FlowLayoutState
			{ flsMetrics = metrics
			} <- get
		elementWithSizeInFlowLayout checkBox (preferredSize metrics checkBox)
		lift $ setChangeHandler checkBox $ do
			value <- getChecked checkBox
			currentValue <- readTVar currentValueVar
			when (value /= currentValue) $ do
				writeTVar currentValueVar value
				setter value
		return $ \newValue _change -> do
			writeTVar currentValueVar newValue
			setChecked checkBox newValue

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
	editableEntityLayout = editBoxEditableLayout id Just

-- | Layout with edit box.
editBoxEditableLayout :: (BasicEntity a, Eq a) => (a -> T.Text) -> (T.Text -> Maybe a) -> a -> (EntityChange a -> STM ()) -> EditableLayoutM (a -> EntityChange a -> STM ())
editBoxEditableLayout toText fromText initialEntity setter = ReaderT $ \EditableLayoutState {} -> do
	currentValueVar <- lift $ newTVar initialEntity
	panel <- lift $ newPanel False
	editBox <- lift newEditBox
	lift $ setText editBox $ toText initialEntity
	_editBoxChild <- lift $ addFreeChild panel editBox
	lift $ setLayoutHandler panel $ layoutElement editBox
	FlowLayoutState
		{ flsMetrics = metrics
		} <- get
	elementWithSizeInFlowLayout panel (preferredSize metrics editBox)
	lift $ setCommitHandler panel $ \commitReason -> do
		setText editBox =<<
			if commitReason == CommitAccept || commitReason == CommitLostFocus then do
				maybeValue <- fromText <$> getText editBox
				case maybeValue of
					Just value -> do
						currentValue <- readTVar currentValueVar
						when (value /= currentValue) $ do
							writeTVar currentValueVar value
							setter value
						return $ toText value
					Nothing -> toText <$> readTVar currentValueVar
			else toText <$> readTVar currentValueVar
		return True
	return $ \newValue _change -> do
		-- check that it's not equal to current value
		currentValue <- readTVar currentValueVar
		when (newValue /= currentValue) $ do
			-- in any case remember new current value
			writeTVar currentValueVar newValue
			-- change text in edit box only if it's not changed
			maybeValue <- fromText <$> getText editBox
			case maybeValue of
				Just value -> when (value == currentValue) $ setText editBox $ toText newValue
				Nothing -> return ()

-- | Layout with edit box using 'Show' and 'Read' for conversion.
editBoxShowReadEditableLayout :: (BasicEntity a, Eq a, Show a, Read a) => a -> (EntityChange a -> STM ()) -> EditableLayoutM (a -> EntityChange a -> STM ())
editBoxShowReadEditableLayout = editBoxEditableLayout (T.pack . show) (readMaybe . T.unpack)

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

instance EntityRegistration EntityPtr where
	performEntityRegistration entityManager _ = registerBasicOrdEntityType entityManager entityPtrFirstEntityTypeId $ do
		SomeEntity underlyingBaseEntity <- getRootBaseEntity
		let
			setType :: a -> EntityPtr a
			setType _ = EntityPtr nullEntityId
		return $ SomeBasicOrdEntity $ setType underlyingBaseEntity

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

instance EntityRegistration InterfacedEntityPtr where
	performEntityRegistration entityManager _ = registerBasicOrdEntityType entityManager interfacedEntityPtrFirstEntityTypeId $ do
		SomeEntityInterface proxy <- deserializeEntityInterface
		let
			setType :: Proxy i -> InterfacedEntityPtr i
			setType Proxy = InterfacedEntityPtr nullEntityId
		return $ SomeBasicOrdEntity $ setType proxy

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

instance EntityRegistration S.Set where
	performEntityRegistration entityManager _ = registerEntityType entityManager setFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingBaseEntity <- getRootBaseBasicOrdEntity
		let
			setType :: a -> S.Set a
			setType _ = S.empty
		return $ SomeEntity $ setType underlyingBaseEntity

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

instance EntityRegistration M.Map where
	performEntityRegistration entityManager _ = registerEntityType entityManager mapFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingKeyBaseEntity <- getRootBaseBasicOrdEntity
		SomeBasicEntity underlyingValueBaseEntity <- getRootBaseBasicEntity
		let
			setType :: k -> v -> M.Map k v
			setType _ _ = M.empty
		return $ SomeEntity $ setType underlyingKeyBaseEntity underlyingValueBaseEntity

