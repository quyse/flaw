{-|
Module: Flaw.Editor.Entity.Basic
Description: Basic instances of 'Entity' typeclass.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, OverloadedStrings, TemplateHaskell, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Editor.Entity.Basic
	( Folder
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

import Flaw.Editor.EditableEntity
import Flaw.Editor.Entity
import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.CheckBox
import Flaw.UI.EditBox
import Flaw.UI.Layout
import Flaw.UI.ListBox
import Flaw.UI.Metrics
import Flaw.UI.Panel

instance Entity EntityId where
	type EntityChange EntityId = EntityId
	getEntityTypeId _ = $(hashTextToEntityTypeId "EntityId")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText (EntityId entityIdBytes) = "entityid:" <> T.decodeUtf8 (BA.convertToBase BA.Base64 $ BS.fromShort entityIdBytes)
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText n = T.pack $ showFFloat Nothing n ""
instance BasicEntity Float
instance EditableEntity Float where
	editableEntityTypeName _ = "Float"
	editableEntityConstructorName _ = "Float"
	editableEntityLayout = editBoxEditableLayout entityToText (readMaybe . T.unpack)

instance Entity (Vec2 Float) where
	type EntityChange (Vec2 Float) = (Vec2 Float)
	getEntityTypeId _ = $(hashTextToEntityTypeId "Float2")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText = T.unwords . map entityToText . vecToList
instance BasicEntity (Vec2 Float)
instance EditableEntity (Vec2 Float) where
	editableEntityTypeName _ = "Float2"
	editableEntityConstructorName _ = "Float2"
	editableEntityLayout = editBoxVecEditableLayout maybeVecFromList where
		maybeVecFromList [x, y] = Just $ Float2 x y
		maybeVecFromList _ = Nothing

instance Entity (Vec3 Float) where
	type EntityChange (Vec3 Float) = (Vec3 Float)
	getEntityTypeId _ = $(hashTextToEntityTypeId "Float3")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText = T.unwords . map entityToText . vecToList
instance BasicEntity (Vec3 Float)
instance EditableEntity (Vec3 Float) where
	editableEntityTypeName _ = "Float3"
	editableEntityConstructorName _ = "Float3"
	editableEntityLayout = editBoxVecEditableLayout maybeVecFromList where
		maybeVecFromList [x, y, z] = Just $ Float3 x y z
		maybeVecFromList _ = Nothing

instance Entity (Vec4 Float) where
	type EntityChange (Vec4 Float) = (Vec4 Float)
	getEntityTypeId _ = $(hashTextToEntityTypeId "Float4")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText = T.unwords . map entityToText . vecToList
instance BasicEntity (Vec4 Float)
instance EditableEntity (Vec4 Float) where
	editableEntityTypeName _ = "Float4"
	editableEntityConstructorName _ = "Float4"
	editableEntityLayout = editBoxVecEditableLayout maybeVecFromList where
		maybeVecFromList [x, y, z, w] = Just $ Float4 x y z w
		maybeVecFromList _ = Nothing

editBoxVecEditableLayout :: (BasicEntity v, Eq v, Read a) => ([a] -> Maybe v) -> v -> (EntityChange v -> STM ()) -> EditableLayoutM (v -> EntityChange v -> STM ())
editBoxVecEditableLayout maybeVecFromList = editBoxEditableLayout entityToText ((maybeVecFromList =<<) . mapM readMaybe . words . T.unpack)

instance Entity Bool where
	type EntityChange Bool = Bool
	getEntityTypeId _ = $(hashTextToEntityTypeId "Bool")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
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
	entityToText = id
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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText s = editableEntityTypeName s <> T.pack (" {" ++ shows (S.size s) "}")

instance EntityRegistration S.Set where
	performEntityRegistration entityManager _ = registerEntityType entityManager setFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingBaseEntity <- getRootBaseBasicOrdEntity
		let
			setType :: a -> S.Set a
			setType _ = S.empty
		return $ SomeEntity $ setType underlyingBaseEntity

instance (Ord a, BasicEntity a) => EditableEntity (S.Set a) where
	editableEntityTypeName = f undefined where
		f :: Entity a => a -> S.Set a -> T.Text
		f u@(interfaceEntity (Proxy :: Proxy EditableEntity) -> EntityInterfaced) _ = "Set<" <> editableEntityTypeName u <> ">"
		f _ _ = "Set"
	editableEntityConstructorName _ = "Set"
	editableEntityLayout initialEntity setter = ReaderT $ \EditableLayoutState {} -> do
		currentValueVar <- lift $ newTVar initialEntity
		itemHandlesVar <- lift $ newTVar M.empty
		FlowLayoutState
			{ flsMetrics = metrics@Metrics
				{ metricsMainWidth = metricMainWidth
				}
			} <- get
		let keyEntityTypeName = let
			f :: Entity a => S.Set a -> a -> T.Text
			f _ u = case interfaceEntity (Proxy :: Proxy EditableEntity) u of
				EntityInterfaced -> editableEntityTypeName u
				EntityNotInterfaced -> "value"
			in f initialEntity undefined
		listBoxColumn <- lift $ newListBoxTextColumnDesc keyEntityTypeName metricMainWidth id entityToText
		listBox <- lift $ newListBox metrics [listBoxColumn]

		let onAdd = return ()
		let onRemove = do
			selectedItems <- getListBoxSelectedValues listBox
			forM_ selectedItems $ \selectedItem -> setter (selectedItem, False)
		addRemoveButtonsLayout listBox onAdd onRemove

		let insertItem item = do
			itemHandles <- readTVar itemHandlesVar
			unless (M.member item itemHandles) $ do
				itemHandle <- addListBoxItem listBox item
				writeTVar itemHandlesVar $! M.insert item itemHandle itemHandles

		let removeItem item = do
			itemHandles <- readTVar itemHandlesVar
			case M.lookup item itemHandles of
				Just itemHandle -> do
					removeListBoxItem listBox itemHandle
					writeTVar itemHandlesVar $! M.delete item itemHandles
				Nothing -> return ()

		lift $ mapM_ insertItem $ S.toList initialEntity

		return $ \newValue (changedKey, changedFlag) -> do
			writeTVar currentValueVar newValue
			(if changedFlag then insertItem else removeItem) changedKey

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
	interfaceEntity = $(interfaceEntityExp [''EditableEntity])
	entityToText m = T.pack ("map{" ++ shows (M.size m) "}")

instance EntityRegistration M.Map where
	performEntityRegistration entityManager _ = registerEntityType entityManager mapFirstEntityTypeId $ do
		SomeBasicOrdEntity underlyingKeyBaseEntity <- getRootBaseBasicOrdEntity
		SomeBasicEntity underlyingValueBaseEntity <- getRootBaseBasicEntity
		let
			setType :: k -> v -> M.Map k v
			setType _ _ = M.empty
		return $ SomeEntity $ setType underlyingKeyBaseEntity underlyingValueBaseEntity

instance (Ord k, BasicEntity k, BasicEntity v) => EditableEntity (M.Map k v) where
	editableEntityTypeName = f undefined undefined where
		f :: (Entity k, Entity v) => k -> v -> M.Map k v -> T.Text
		f
			uk@(interfaceEntity (Proxy :: Proxy EditableEntity) -> EntityInterfaced)
			uv@(interfaceEntity (Proxy :: Proxy EditableEntity) -> EntityInterfaced)
			_ = "Map<" <> editableEntityTypeName uk <> "," <> editableEntityTypeName uv <> ">"
		f _ _ _ = "Map"
	editableEntityConstructorName _ = "Map"
	editableEntityLayout initialEntity setter = ReaderT $ \EditableLayoutState {} -> do
		currentValueVar <- lift $ newTVar initialEntity
		itemHandlesVar <- lift $ newTVar M.empty
		FlowLayoutState
			{ flsMetrics = metrics@Metrics
				{ metricsMainWidth = metricMainWidth
				}
			} <- get

		let (keyEntityTypeName, valueEntityTypeName) = let
			f :: (Entity k, Entity v) => M.Map k v -> k -> v -> (T.Text, T.Text)
			f _ uk uv =
				( case interfaceEntity (Proxy :: Proxy EditableEntity) uk of
					EntityInterfaced -> editableEntityTypeName uk
					EntityNotInterfaced -> "key"
				, case interfaceEntity (Proxy :: Proxy EditableEntity) uv of
					EntityInterfaced -> editableEntityTypeName uv
					EntityNotInterfaced -> "value"
				)
			in f initialEntity undefined undefined

		listBoxKeyColumn <- lift $ newListBoxTextColumnDesc keyEntityTypeName (metricMainWidth `quot` 2) fst (entityToText . fst)
		listBoxValueColumn <- lift $ let f = entityToText . snd in newListBoxTextColumnDesc valueEntityTypeName (metricMainWidth `quot` 2) f f
		listBox <- lift $ newListBox metrics [listBoxKeyColumn, listBoxValueColumn]

		let onAdd = return ()

		let onRemove = do
			selectedItems <- getListBoxSelectedValues listBox
			forM_ selectedItems $ \(selectedKey, _selectedValue) -> setter (selectedKey, Nothing)

		addRemoveButtonsLayout listBox onAdd onRemove

		let insertItem item@(itemKey, _itemValue) = do
			itemHandles <- readTVar itemHandlesVar
			unless (M.member itemKey itemHandles) $ do
				itemHandle <- addListBoxItem listBox item
				writeTVar itemHandlesVar $! M.insert itemKey itemHandle itemHandles

		let removeItem itemKey = do
			itemHandles <- readTVar itemHandlesVar
			case M.lookup itemKey itemHandles of
				Just itemHandle -> do
					removeListBoxItem listBox itemHandle
					writeTVar itemHandlesVar $! M.delete itemKey itemHandles
				Nothing -> return ()

		lift $ mapM_ insertItem $ M.toList initialEntity

		return $ \newValue (changedKey, changedMaybeInsertedValue) -> do
			writeTVar currentValueVar newValue
			case changedMaybeInsertedValue of
				Just insertedValue -> insertItem (changedKey, insertedValue)
				Nothing -> removeItem changedKey

-- | Helper function: layout add and remove buttons
addRemoveButtonsLayout :: ListBox a -> STM () -> STM () -> FlowLayoutM ()
addRemoveButtonsLayout listBox onAdd onRemove = StateT $ \s@FlowLayoutState
	{ flsMetrics = Metrics
		{ metricsGap = metricGap
		, metricsMainWidth = metricMainWidth
		, metricsButtonSize = Vec2 _ metricButtonHeight
		, metricsListBoxItemHeight = metricListBoxItemHeight
		}
	, flsParentElement = parentElement
	, flsLayoutHandler = lh
	, flsPreSize = Vec2 psx psy
	} -> do
	listBoxChild <- addFreeChild parentElement listBox

	addButton <- newLabeledButton "+"
	setActionHandler addButton onAdd
	addButtonChild <- addFreeChild parentElement addButton
	layoutElement addButton $ Vec2 metricButtonHeight metricButtonHeight

	removeButton <- newLabeledButton "-"
	setActionHandler removeButton onRemove
	removeButtonChild <- addFreeChild parentElement removeButton
	layoutElement removeButton $ Vec2 metricButtonHeight metricButtonHeight

	return ((), s
		{ flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
			placeFreeChild parentElement listBoxChild $ Vec2 px py
			layoutElement listBox $ Vec2 (qx - px) (qy - py - metricGap - metricButtonHeight)
			placeFreeChild parentElement addButtonChild $ Vec2 (qx - metricButtonHeight * 2 - metricGap) (qy - metricButtonHeight)
			placeFreeChild parentElement removeButtonChild $ Vec2 (qx - metricButtonHeight) (qy - metricButtonHeight)
			return $ Vec4 px qy qx qy
		, flsPreSize = Vec2 (max psx metricMainWidth) (psy + metricListBoxItemHeight * 7 + metricGap)
		})


-- some synonyms

type Folder = M.Map T.Text EntityId
