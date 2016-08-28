{-|
Module: Flaw.UI.Editor.Editable
Description: Class for defining layout to edit entities.
License: MIT
-}

{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeOperators #-}

module Flaw.UI.Editor.Editable
	( EditableLayoutState(..)
	, EditableLayoutM
	, Editable(..)
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
class Editable a where
	{-# MINIMAL #-}

	-- | Get editable's type name.
	editableTypeName :: a -> T.Text
	default editableTypeName :: (G.Generic a, GenericEditableDatatype (G.Rep a)) => a -> T.Text
	editableTypeName = genericEditableDatatypeName . G.from

	-- | Get editable's constructor name.
	editableConstructorName :: a -> T.Text
	default editableConstructorName :: (G.Generic a, G.Rep a ~ G.M1 G.D c f, GenericEditableConstructor f) => a -> T.Text
	editableConstructorName = genericEditableConstructorName . G.unM1 . G.from

	-- | Get editable's layout.
	editableLayout :: ((a -> a) -> STM ()) -> EditableLayoutM (a -> STM ())
	default editableLayout :: (G.Generic a, GenericEditableDatatype (G.Rep a)) => ((a -> a) -> STM ()) -> EditableLayoutM (a -> STM ())
	editableLayout setter = (. G.from) <$> genericEditableDatatypeLayout (\f -> setter $ G.to . f . G.from)

class GenericEditableDatatype f where
	genericEditableDatatypeName :: f p -> T.Text
	genericEditableDatatypeLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> STM ())

class GenericEditableConstructor f where
	genericEditableConstructorName :: f p -> T.Text
	genericEditableConstructorLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> STM ())

class GenericEditableSelector f where
	genericEditableSelectorLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> STM ())

class GenericEditableValue f where
	genericEditableValueLayout :: ((f p -> f p) -> STM ()) -> EditableLayoutM (f p -> STM ())

-- datatype metadata
instance (G.Datatype c, GenericEditableConstructor f) => GenericEditableDatatype (G.M1 G.D c f) where
	genericEditableDatatypeName = T.pack . G.datatypeName
	genericEditableDatatypeLayout setter = wu $ \u -> do
		lift $ titleInFlowLayout $ genericEditableDatatypeName u
		(. G.unM1) <$> genericEditableConstructorLayout (\f -> setter $ G.M1 . f . G.unM1)
		where
			wu :: (f p -> EditableLayoutM (f p -> STM ())) -> EditableLayoutM (f p -> STM ())
			wu f = f undefined
	{-# INLINEABLE genericEditableDatatypeLayout #-}

-- constructor metadata
instance (G.Constructor c, GenericEditableSelector f) => GenericEditableConstructor (G.M1 G.C c f) where
	genericEditableConstructorName = T.pack . G.conName
	genericEditableConstructorLayout setter = (. G.unM1) <$> genericEditableSelectorLayout (\f -> setter $ G.M1 . f . G.unM1)
	{-# INLINEABLE genericEditableConstructorLayout #-}

-- selector metadata
instance (G.Selector c, GenericEditableValue f) => GenericEditableSelector (G.M1 G.S c f) where
	genericEditableSelectorLayout setter = wu $ \u -> ReaderT $ \s -> do
		let sublayout = runReaderT (genericEditableValueLayout (\f -> setter $ G.M1 . f . G.unM1)) s
		(. G.unM1) <$> labeledFlowLayout (T.pack $ G.selName u) sublayout
		where
			wu :: (f p -> EditableLayoutM (f p -> STM ())) -> EditableLayoutM (f p -> STM ())
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
		return $ \(a G.:*: b) -> do
			update1 a
			update2 b
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- constructor without arguments - nothing to show
instance GenericEditableSelector G.U1 where
	genericEditableSelectorLayout _setter = return $ const $ return ()
	{-# INLINEABLE genericEditableSelectorLayout #-}

-- value
instance Editable a => GenericEditableValue (G.K1 G.R a) where
	genericEditableValueLayout setter = (. G.unK1) <$> editableLayout (\f -> setter $ G.K1 . f . G.unK1)
	{-# INLINEABLE genericEditableValueLayout #-}
