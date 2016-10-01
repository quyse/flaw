{-|
Module: Flaw.UI.Editor.EditableEntity
Description: Class for defining layout to edit entities.
License: MIT
-}

{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, TemplateHaskell, TypeFamilies, TypeOperators #-}

module Flaw.UI.Editor.EditableEntity
	( EditableLayoutState(..)
	, EditableLayoutM
	, EditableEntity(..)
	, editableLayoutForEntityId
	) where

import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Text as T
import qualified GHC.Generics as G

import Flaw.Book
import Flaw.Editor.Entity
import Flaw.Flow
import Flaw.UI.Layout
import Flaw.UI.Popup

data EditableLayoutState = EditableLayoutState
	{ elsFlow :: !Flow
	, elsBook :: !Book
	, elsEntityManager :: !EntityManager
	, elsPopupService :: !PopupService
	}

type EditableLayoutM = ReaderT EditableLayoutState FlowLayoutM

-- | Value able to be edited in editor.
class Entity a => EditableEntity a where
	-- | Get editable entity's type name.
	editableEntityTypeName :: a -> T.Text
	default editableEntityTypeName :: (G.Generic a, GenericEditableDatatype (G.Rep a)) => a -> T.Text
	editableEntityTypeName = genericEditableDatatypeName . G.from

	-- | Get editable entity's constructor name.
	editableEntityConstructorName :: a -> T.Text
	default editableEntityConstructorName :: (G.Generic a, G.Rep a ~ G.M1 G.D c f, GenericEditableConstructor f) => a -> T.Text
	editableEntityConstructorName = genericEditableConstructorName . G.unM1 . G.from

	-- | Get editable entity's layout.
	editableEntityLayout :: a -> (EntityChange a -> STM ()) -> EditableLayoutM (a -> EntityChange a -> STM ())
	default editableEntityLayout :: (G.Generic a, GenericEditableDatatype (G.Rep a), EntityChange a ~ GenericEntityChange a) => a -> (EntityChange a -> STM ()) -> EditableLayoutM (a -> EntityChange a -> STM ())
	editableEntityLayout initialEntity setter = (. G.from) <$> genericEditableDatatypeLayout (G.from initialEntity) setter

instance EntityInterface EditableEntity where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "EditableEntity")

class GenericEntityDatatype f => GenericEditableDatatype f where
	genericEditableDatatypeName :: f p -> T.Text
	genericEditableDatatypeLayout :: f p -> (GenericEntityDatatypeChange f -> STM ()) -> EditableLayoutM (f p -> GenericEntityDatatypeChange f -> STM ())

class GenericEntityConstructor f => GenericEditableConstructor f where
	genericEditableConstructorName :: f p -> T.Text
	genericEditableConstructorLayout :: f p -> (GenericEntityConstructorChange f -> STM ()) -> EditableLayoutM (f p -> GenericEntityConstructorChange f -> STM ())

class GenericEntitySelector f => GenericEditableSelector f where
	genericEditableSelectorLayout :: f p -> (GenericEntitySelectorChange f -> STM ()) -> EditableLayoutM (f p -> GenericEntitySelectorChange f -> STM ())

class GenericEntityValue f => GenericEditableValue f where
	genericEditableValueLayout :: f p -> (GenericEntityValueChange f -> STM ()) -> EditableLayoutM (f p -> GenericEntityValueChange f -> STM ())

-- datatype metadata
instance (G.Datatype c, GenericEditableConstructor f) => GenericEditableDatatype (G.M1 G.D c f) where
	genericEditableDatatypeName = T.pack . G.datatypeName
	genericEditableDatatypeLayout initialEntity setter = do
		lift $ titleInFlowLayout $ genericEditableDatatypeName initialEntity
		(. G.unM1) <$> genericEditableConstructorLayout (G.unM1 initialEntity) setter
	{-# INLINEABLE genericEditableDatatypeLayout #-}

-- constructor metadata
instance (G.Constructor c, GenericEditableSelector f) => GenericEditableConstructor (G.M1 G.C c f) where
	genericEditableConstructorName = T.pack . G.conName
	genericEditableConstructorLayout initialEntity setter = (. G.unM1) <$> genericEditableSelectorLayout (G.unM1 initialEntity) setter
	{-# INLINEABLE genericEditableConstructorLayout #-}

-- selector metadata
instance (G.Selector c, GenericEditableValue f) => GenericEditableSelector (G.M1 G.S c f) where
	genericEditableSelectorLayout initialEntity setter = ReaderT $ \s -> let
		sublayout = runReaderT (genericEditableValueLayout (G.unM1 initialEntity) setter) s
		in (. G.unM1) <$> labeledFlowLayout (T.pack $ G.selName initialEntity) sublayout
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- constructor sum metadata
-- instance GenericEditableConstructor (a G.:+: b)
-- We are yet to support multiple constructors, because a way to switch
-- multiple UIs for different constructors (tabs? combobox?) is not implemented yet.

-- selector sum metadata
instance (GenericEditableSelector a, GenericEditableSelector b) => GenericEditableSelector (a G.:*: b) where
	genericEditableSelectorLayout (a G.:*: b) setter = do
		update1 <- genericEditableSelectorLayout a (setter . Left)
		update2 <- genericEditableSelectorLayout b (setter . Right)
		return $ \(na G.:*: nb) change -> case change of
			Left l -> update1 na l
			Right r -> update2 nb r
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- value
instance (BasicEntity a, EditableEntity a) => GenericEditableValue (G.K1 G.R a) where
	genericEditableValueLayout initialEntity setter = (. G.unK1) <$> editableEntityLayout (G.unK1 initialEntity) setter
	{-# INLINEABLE genericEditableValueLayout #-}

-- | Create editable layout watching at specified entity id.
editableLayoutForEntityId :: EditableLayoutState -> EntityId -> (forall a. FlowLayoutM a -> STM a) -> IO ()
editableLayoutForEntityId s@EditableLayoutState
	{ elsBook = bk
	, elsEntityManager = entityManager
	} entityId makeLayout = do
	someEntityVar <- getSomeEntityVar entityManager entityId

	-- every time entity changes its type, initForEntityType is called
	-- it allocates flow and other stuff for watching entity's history
	-- while it's of the given type
	dbk <- book bk newDynamicBook
	let initForEntityType = join $ atomically $ do -- STM monad
		SomeEntity initialEntity <- readSomeEntityVar someEntityVar
		let
			getInitialEntityVar :: a -> EntityVar a
			getInitialEntityVar _ = EntityVar someEntityVar
			entityVar = getInitialEntityVar initialEntity
		entityHistoryChan <- entityVarHistory entityVar
		return $ do -- IO monad
			-- make layout for entity if it's editable
			updateLayout <- case interfaceEntity (Proxy :: Proxy EditableEntity) initialEntity of
				EntityInterfaced -> atomically $ makeLayout $ runReaderT (editableEntityLayout initialEntity (applyEntityChange entityVar)) s
					{ elsBook = dbk
					}
				EntityNotInterfaced -> return $ \_ _ -> return () -- empty layout
			book dbk $ forkFlow $ forever $ do
				m <- atomically $ catchSTM (Just <$> readEntityHistoryChan entityHistoryChan) $ \EntityWrongTypeException -> return Nothing
				case m of
					Just (newEntity, entityChange) -> atomically $ updateLayout newEntity entityChange
					Nothing -> do
						-- run new init
						rdbk <- releaseBook dbk
						initForEntityType
						-- kill itself
						rdbk
	initForEntityType
