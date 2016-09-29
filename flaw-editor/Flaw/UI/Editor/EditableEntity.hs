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
import Flaw.Flow
import Flaw.Oil.Entity
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
	editableEntityLayout :: (EntityChange a -> STM ()) -> EditableLayoutM (a -> EntityChange a -> STM ())
	default editableEntityLayout :: (G.Generic a, GenericEditableDatatype (G.Rep a)) => ((a -> a) -> STM ()) -> EditableLayoutM (a -> (a -> a) -> STM ())
	editableEntityLayout setter = (\f -> \v c -> f (G.from v) (G.from . c . G.to)) <$> genericEditableDatatypeLayout (\f -> setter $ G.to . f . G.from)

instance EntityInterface EditableEntity where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "EditableEntity")

class GenericEditableDatatype f where
	genericEditableDatatypeName :: f p -> T.Text
	genericEditableDatatypeLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())

class GenericEditableConstructor f where
	genericEditableConstructorName :: f p -> T.Text
	genericEditableConstructorLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())

class GenericEditableSelector f where
	genericEditableSelectorLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())

class GenericEditableValue f where
	genericEditableValueLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())

-- datatype metadata
instance (G.Datatype c, GenericEditableConstructor f) => GenericEditableDatatype (G.M1 G.D c f) where
	genericEditableDatatypeName = T.pack . G.datatypeName
	genericEditableDatatypeLayout setter = wu $ \u -> do
		lift $ titleInFlowLayout $ genericEditableDatatypeName u
		(\f -> \v c -> f (G.unM1 v) (G.unM1 . c . G.M1)) <$> genericEditableConstructorLayout (\f -> setter $ G.M1 . f . G.unM1)
		where
			wu :: (f p -> EditableLayoutM (f p -> (f p -> f p) -> STM ())) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())
			wu f = f undefined
	{-# INLINEABLE genericEditableDatatypeLayout #-}

-- constructor metadata
instance (G.Constructor c, GenericEditableSelector f) => GenericEditableConstructor (G.M1 G.C c f) where
	genericEditableConstructorName = T.pack . G.conName
	genericEditableConstructorLayout setter = (\f -> \v c -> f (G.unM1 v) (G.unM1 . c . G.M1)) <$> genericEditableSelectorLayout (\f -> setter $ G.M1 . f . G.unM1)
	{-# INLINEABLE genericEditableConstructorLayout #-}

-- selector metadata
instance (G.Selector c, GenericEditableValue f) => GenericEditableSelector (G.M1 G.S c f) where
	genericEditableSelectorLayout setter = wu $ \u -> ReaderT $ \s -> do
		let sublayout = runReaderT (genericEditableValueLayout $ \f -> setter $ G.M1 . f . G.unM1) s
		(\f -> \v c -> f (G.unM1 v) (G.unM1 . c . G.M1)) <$> labeledFlowLayout (T.pack $ G.selName u) sublayout
		where
			wu :: (f p -> EditableLayoutM (f p -> (f p -> f p) -> STM ())) -> EditableLayoutM (f p -> (f p -> f p) -> STM ())
			wu f = f undefined
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- constructor sum metadata
-- instance GenericEditableConstructor (a G.:+: b)
-- We are yet to support multiple constructors, because a way to switch
-- multiple UIs for different constructors (tabs? combobox?) is not implemented yet.

-- selector sum metadata
instance (GenericEditableSelector a, GenericEditableSelector b) => GenericEditableSelector (a G.:*: b) where
	genericEditableSelectorLayout setter = do
		update1 <- genericEditableSelectorLayout (\f -> setter $ \(a G.:*: b) -> f a G.:*: b)
		update2 <- genericEditableSelectorLayout (\f -> setter $ \(a G.:*: b) -> a G.:*: f b)
		return $ \(ra G.:*: rb) c -> do
			-- a bit strange to use end values (ra, rb) as a source,
			-- but must be ok, as we are interested in change in a single field at a time
			update1 ra $ \a -> let (r G.:*: _) = c (a G.:*: rb) in r
			update2 rb $ \b -> let (_ G.:*: r) = c (ra G.:*: b) in r
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- constructor without arguments - nothing to show
instance GenericEditableSelector G.U1 where
	genericEditableSelectorLayout _setter = return $ \_v _c -> return ()
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- value
instance (EditableEntity a, BasicEntity a) => GenericEditableValue (G.K1 G.R a) where
	genericEditableValueLayout setter = (\f -> \v c -> f (G.unK1 v) (G.unK1 . c . G.K1)) <$> editableEntityLayout (\f -> setter $ G.K1 . f . G.unK1)
	{-# INLINEABLE genericEditableValueLayout #-}

-- | Create editable layout watching at specified entity id.
editableLayoutForEntityId :: EditableLayoutState -> EntityId -> (forall a. FlowLayoutM a -> STM a) -> IO ()
editableLayoutForEntityId s@EditableLayoutState
	{ elsBook = bk
	, elsEntityManager = entityManager
	} entityId updateLayout = do
	someEntityVar <- getSomeEntityVar entityManager entityId

	-- every time entity changes its type, init is called
	-- init allocates flow and other stuff for watching entity's history
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
			-- -- make layout for entity if it's editable
			notifyLayout <- case interfaceEntity (Proxy :: Proxy EditableEntity) initialEntity of
				EntityInterfaced -> atomically $ updateLayout $ runReaderT (editableEntityLayout (applyEntityChange entityVar)) s
					{ elsBook = dbk
					}
				EntityNotInterfaced -> return $ \_ _ -> return () -- empty layout
			book dbk $ forkFlow $ forever $ do
				m <- atomically $ catchSTM (Just <$> readEntityHistoryChan entityHistoryChan) $ \EntityWrongTypeException -> return Nothing
				case m of
					Just (newEntity, entityChange) -> atomically $ notifyLayout newEntity entityChange
					Nothing -> do
						-- run new init
						rdbk <- releaseBook dbk
						initForEntityType
						-- kill itself
						rdbk
	initForEntityType
