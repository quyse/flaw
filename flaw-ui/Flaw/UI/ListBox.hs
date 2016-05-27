{-|
Module: Flaw.UI.ListBox
Description: List box.
License: MIT
-}

{-# LANGUAGE GADTs, RankNTypes #-}

module Flaw.UI.ListBox
	( ListBox(..)
	, ListBoxColumn(..)
	, newListBox
	, addListBoxItem
	, removeListBoxItem
	, changeListBoxItem
	, clearListBox
	, reorderListBox
	, newListBoxTextColumnDesc
	) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Label
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.PileBox

data ListBox v = ListBox
	{ listBoxPanel :: !Panel
	, listBoxColumnHeaderHeight :: {-# UNPACK #-} !Metric
	, listBoxItemHeight :: {-# UNPACK #-} !Metric
	-- | Values ordered by item index.
	, listBoxValuesVar :: !(TVar (IM.IntMap v))
  -- | Items ordered by current sort function.
	, listBoxItemsVar :: !(TVar (ListBoxItems v))
	-- | Index to assign next added item.
	, listBoxNextItemIndexVar :: !(TVar Int)
	-- | Columns.
	, listBoxColumns :: [ListBoxColumn v]
	, listBoxSizeVar :: !(TVar Size)
	, listBoxFocusedVar :: !(TVar Bool)
	, listBoxMousedVar :: !(TVar Bool)
	, listBoxPressedVar :: !(TVar Bool)
	}

-- | Handle for list box item.
-- Allows to remove item from list.
newtype ListBoxItemHandle v = ListBoxItemHandle Int

-- | Wrapper for list box item sorted by some key.
data ListBoxItem k v = ListBoxItem Int (v -> k) v

instance Eq (ListBoxItem k v) where
	{-# INLINE (==) #-}
	ListBoxItem i1 _f1 _v1 == ListBoxItem i2 _f2 _v2 = i1 == i2

instance Ord k => Ord (ListBoxItem k v) where
	{-# INLINE compare #-}
	compare (ListBoxItem i1 f1 v1) (ListBoxItem i2 f2 v2) = case compare (f1 v1) (f2 v2) of
		LT -> LT
		EQ -> compare i1 i2
		GT -> GT

-- | List box items sorted by some key.
data ListBoxItems v where
	ListBoxItems :: Ord k => (v -> k) -> S.Set (ListBoxItem k v) -> ListBoxItems v

-- | Column (also works as column header element).
data ListBoxColumn v = ListBoxColumn
	{ listBoxColumnParent :: !(ListBox v)
	, listBoxColumnDesc :: !(ListBoxColumnDesc v)
	, listBoxColumnWidthVar :: !(TVar Metric)
	, listBoxColumnMousedVar :: !(TVar Bool)
	, listBoxColumnPressedVar :: !(TVar Bool)
	}

-- | Immutable column descrition.
data ListBoxColumnDesc v where
	ListBoxColumnDesc :: Ord k =>
		{ listBoxColumnDescVisual :: !SomeVisual
		, listBoxColumnDescKeyFunc :: !(v -> k)
		, listBoxColumnDescRenderFunc :: !(forall c d. Context c d => v -> Drawer d -> Position -> Size -> Style -> STM (Render c ()))
		} -> ListBoxColumnDesc v

newListBox :: Metrics -> [ListBoxColumnDesc v] -> STM (ListBox v)
newListBox metrics@Metrics
	{ metricsListBoxColumnHeaderHeight = columnHeaderHeight
	, metricsListBoxItemHeight = itemHeight
	} columnDescs = do
	panel <- newPanel False
	listBox@ListBox
		{ listBoxColumns = columns
		} <- mfix $ \listBox -> do
		columns <- forM columnDescs $ \columnDesc -> do
			widthVar <- newTVar 0
			mousedVar <- newTVar False
			pressedVar <- newTVar False
			return ListBoxColumn
				{ listBoxColumnParent = listBox
				, listBoxColumnDesc = columnDesc
				, listBoxColumnWidthVar = widthVar
				, listBoxColumnMousedVar = mousedVar
				, listBoxColumnPressedVar = pressedVar
				}

		valuesVar <- newTVar IM.empty
		itemsVar <- newTVar $ ListBoxItems (const (0 :: Int)) S.empty
		nextItemIndexVar <- newTVar 0
		sizeVar <- newTVar $ Vec2 0 0
		focusedVar <- newTVar False
		mousedVar <- newTVar False
		pressedVar <- newTVar False
		return ListBox
			{ listBoxPanel = panel
			, listBoxColumnHeaderHeight = columnHeaderHeight
			, listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			, listBoxItemsVar = itemsVar
			, listBoxNextItemIndexVar = nextItemIndexVar
			, listBoxColumns = columns
			, listBoxSizeVar = sizeVar
			, listBoxFocusedVar = focusedVar
			, listBoxMousedVar = mousedVar
			, listBoxPressedVar = pressedVar
			}

	-- pile box for column headers
	pileBox <- newPileBox metrics $ map SomeElement columns
	void $ addFreeChild panel pileBox
	setLayoutHandler panel $ \(Vec2 sx _sy) -> layoutElement pileBox $ Vec2 sx columnHeaderHeight

	return listBox

-- | Add new list item to listbox.
addListBoxItem :: ListBox v -> v -> STM (ListBoxItemHandle v)
addListBoxItem ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	, listBoxNextItemIndexVar = nextItemIndexVar
	} value = do
	itemIndex <- readTVar nextItemIndexVar
	writeTVar nextItemIndexVar $ itemIndex + 1
	modifyTVar' valuesVar $ IM.insert itemIndex value
	modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
		ListBoxItems keyFunc $ S.insert (ListBoxItem itemIndex keyFunc value) items
	return $ ListBoxItemHandle itemIndex

-- | Remove list item by handle.
removeListBoxItem :: ListBox v -> ListBoxItemHandle v -> STM ()
removeListBoxItem ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	} (ListBoxItemHandle itemIndex) = do
	values <- readTVar valuesVar
	case IM.lookup itemIndex values of
		Just value -> do
			writeTVar valuesVar $ IM.delete itemIndex values
			modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
				ListBoxItems keyFunc $ S.delete (ListBoxItem itemIndex keyFunc value) items
		Nothing -> return ()

-- | Change list item by handle.
-- List item's handle remains valid.
changeListBoxItem :: ListBox v -> ListBoxItemHandle v -> v -> STM ()
changeListBoxItem ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	} (ListBoxItemHandle itemIndex) newValue = do
	values <- readTVar valuesVar
	case IM.lookup itemIndex values of
		Just oldValue -> do
			writeTVar valuesVar $ IM.insert itemIndex newValue values
			modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
				ListBoxItems keyFunc
				$ S.insert (ListBoxItem itemIndex keyFunc newValue)
				$ S.delete (ListBoxItem itemIndex keyFunc oldValue) items
		Nothing -> return ()

-- | Remove all items from list box.
clearListBox :: ListBox v -> STM ()
clearListBox ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	} = do
	writeTVar valuesVar IM.empty
	modifyTVar' itemsVar $ \(ListBoxItems keyFunc _items) -> ListBoxItems keyFunc S.empty

-- | Reorder list box using new sort function.
reorderListBox :: Ord k => ListBox v -> (v -> k) -> STM ()
reorderListBox ListBox
	{ listBoxItemsVar = itemsVar
	} newSortFunc = modifyTVar' itemsVar $ \(ListBoxItems _oldSortFunc items) ->
	ListBoxItems newSortFunc $ S.fromList $ flip map (S.toList items)
	$ \(ListBoxItem itemIndex _oldSortFunc value) -> ListBoxItem itemIndex newSortFunc value

instance Element (ListBox v) where

	layoutElement ListBox
		{ listBoxPanel = panel
		, listBoxSizeVar = sizeVar
		} size = do
		writeTVar sizeVar size
		layoutElement panel $ size - Vec2 2 2

	dabElement ListBox
		{ listBoxSizeVar = sizeVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	elementMouseCursor = elementMouseCursor . listBoxPanel

	renderElement ListBox
		{ listBoxPanel = panel
		, listBoxColumnHeaderHeight = columnHeaderHeight
		, listBoxItemHeight = itemHeight
		, listBoxItemsVar = itemsVar
		, listBoxColumns = columns
		, listBoxSizeVar = sizeVar
		, listBoxFocusedVar = focusedVar
		, listBoxMousedVar = mousedVar
		, listBoxPressedVar = pressedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerLoweredStyleVariant = StyleVariant
				{ styleVariantNormalStyle = normalStyle
				, styleVariantMousedStyle = mousedStyle
				, styleVariantPressedStyle = pressedStyle
				}
			}
		} (Vec2 px py) = do
		-- get state
		Vec2 sx sy <- readTVar sizeVar
		focused <- readTVar focusedVar
		moused <- readTVar mousedVar
		pressed <- readTVar pressedVar
		-- get style
		let style
			| pressed = pressedStyle
			| moused || focused = mousedStyle
			| otherwise = normalStyle
	
		-- calculate rendering of items
		renderItemColumns <- let
			f left (ListBoxColumn
				{ listBoxColumnDesc = ListBoxColumnDesc
					{ listBoxColumnDescRenderFunc = renderFunc
					}
				, listBoxColumnWidthVar = widthVar
				} : restColumns) = do
				width <- readTVar widthVar
				((left, width, renderFunc) :) <$> f (left + width) restColumns
			f _ [] = return []
			in f (px + 1) columns
		ListBoxItems _keyFunc items <- readTVar itemsVar
		let
			renderItems top (ListBoxItem _itemIndex _keyFunc value : restItems) = do
				r <- (sequence_ <$>) . forM renderItemColumns $ \(left, width, renderFunc) -> do
					r <- renderFunc value drawer (Vec2 left top) (Vec2 width itemHeight) style
					return $ renderScope $ do
						renderIntersectScissor $ Vec4 left top (left + width) (top + itemHeight)
						r
				(r >>) <$> renderItems (top + itemHeight) restItems
			renderItems _ [] = return $ return ()
		itemsRender <- renderItems (py + 1 + columnHeaderHeight) $ S.toAscList items
		panelRender <- renderElement panel drawer (Vec2 (px + 1) (py + 1))
		return $ do
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor style) (styleBorderColor style)
			renderIntersectScissor $ Vec4 (px + 1) (py + 1) (px + sx - 1) (py + sy - 1)
			panelRender
			itemsRender

	processInputEvent = processInputEvent . listBoxPanel

	focusElement ListBox
		{ listBoxFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True

	unfocusElement ListBox
		{ listBoxFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance Element (ListBoxColumn v) where

	layoutElement ListBoxColumn
		{ listBoxColumnWidthVar = widthVar
		} (Vec2 sx _sy) = writeTVar widthVar sx

	dabElement ListBoxColumn
		{ listBoxColumnParent = ListBox
			{ listBoxColumnHeaderHeight = columnHeaderHeight
			}
		, listBoxColumnWidthVar = widthVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 || y >= columnHeaderHeight then return False
		else (x <) <$> readTVar widthVar

	renderElement ListBoxColumn
		{ listBoxColumnParent = ListBox
			{ listBoxColumnHeaderHeight = columnHeaderHeight
			}
		, listBoxColumnDesc = ListBoxColumnDesc
			{ listBoxColumnDescVisual = SomeVisual visual
			}
		, listBoxColumnWidthVar = widthVar
		, listBoxColumnMousedVar = mousedVar
		, listBoxColumnPressedVar = pressedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerRaisedStyleVariant = StyleVariant
				{ styleVariantNormalStyle = normalStyle
				, styleVariantMousedStyle = mousedStyle
				, styleVariantPressedStyle = pressedStyle
				}
			}
		} (Vec2 px py) = do
		-- get state
		sx <- readTVar widthVar
		let sy = columnHeaderHeight
		moused <- readTVar mousedVar
		pressed <- readTVar pressedVar
		-- get style
		let style
			| pressed = pressedStyle
			| moused = mousedStyle
			| otherwise = normalStyle
		-- calculate visual rendering
		visualRender <- renderVisual visual drawer (Vec2 (px + 1) (py + 1)) (Vec2 sx sy) style
		-- return rendering
		return $ do
			drawBorderedRectangle canvas
				(Vec4 px px (px + sx - 1) (px + sx))
				(Vec4 py py (py + sy - 1) (py + sy))
				(styleFillColor style) (styleBorderColor style)
			renderIntersectScissor $ Vec4 (px + 1) (py + 1) (px + sx - 1) (py + sy - 1)
			renderScope visualRender

	processInputEvent ListBoxColumn
		{ listBoxColumnParent = parent
		, listBoxColumnDesc = ListBoxColumnDesc
			{ listBoxColumnDescKeyFunc = keyFunc
			}
		, listBoxColumnMousedVar = mousedVar
		, listBoxColumnPressedVar = pressedVar
		} inputEvent _inputState = case inputEvent of
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				writeTVar pressedVar True
				reorderListBox parent keyFunc
				return True
			MouseUpEvent LeftMouseButton -> do
				writeTVar pressedVar False
				return True
			CursorMoveEvent _x _y -> do
				writeTVar mousedVar True
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar mousedVar False
			writeTVar pressedVar False
			return True
		_ -> return False

newListBoxTextColumnDesc :: T.Text -> (v -> T.Text) -> STM (ListBoxColumnDesc v)
newListBoxTextColumnDesc title keyFunc = do
	label <- newTextLabel
	setText label title
	return ListBoxColumnDesc
		{ listBoxColumnDescVisual = SomeVisual label
		, listBoxColumnDescKeyFunc = keyFunc
		, listBoxColumnDescRenderFunc = \value drawer position size style -> return $ renderLabel (keyFunc value) fontScriptUnknown LabelStyleText drawer position size style
		}
