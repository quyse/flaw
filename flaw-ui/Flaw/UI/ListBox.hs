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
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Label
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.PileBox
import Flaw.UI.ScrollBox

data ListBox v = ListBox
	{ listBoxPanel :: !Panel
	, listBoxColumnHeaderHeight :: {-# UNPACK #-} !Metric
	, listBoxItemHeight :: {-# UNPACK #-} !Metric
	-- | Values ordered by item index.
	, listBoxValuesVar :: !(TVar (IM.IntMap v))
  -- | Items ordered by current sort function.
	, listBoxItemsVar :: !(TVar (ListBoxItems v))
	-- | Selected values.
	, listBoxSelectedValuesVar :: {-# UNPACK #-} !(TVar IS.IntSet)
	-- | Index to assign next added item.
	, listBoxNextItemIndexVar :: !(TVar Int)
	-- | Columns.
	, listBoxColumns :: [ListBoxColumn v]
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

data ListBoxContent v = ListBoxContent
	{ listBoxContentParent :: !(ListBox v)
	, listBoxContentWidthVar :: {-# UNPACK #-} !(TVar Metric)
	, listBoxContentFocusedVar :: {-# UNPACK #-} !(TVar Bool)
	, listBoxContentLastMousePositionVar :: {-# UNPACK #-} !(TVar (Maybe Position))
	}

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
		selectedValuesVar <- newTVar IS.empty
		nextItemIndexVar <- newTVar 0
		return ListBox
			{ listBoxPanel = panel
			, listBoxColumnHeaderHeight = columnHeaderHeight
			, listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			, listBoxItemsVar = itemsVar
			, listBoxSelectedValuesVar = selectedValuesVar
			, listBoxNextItemIndexVar = nextItemIndexVar
			, listBoxColumns = columns
			}

	-- pile box for column headers
	pileBox <- newPileBox metrics $ map SomeElement columns
	pileBoxChild <- addFreeChild panel pileBox

	-- content element
	widthVar <- newTVar 0
	focusedVar <- newTVar False
	lastMousePositionVar <- newTVar Nothing
	let content = ListBoxContent
		{ listBoxContentParent = listBox
		, listBoxContentWidthVar = widthVar
		, listBoxContentFocusedVar = focusedVar
		, listBoxContentLastMousePositionVar = lastMousePositionVar
		}

	-- scroll box
	scrollBox <- newScrollBox content
	scrollBoxChild <- addFreeChild panel scrollBox

	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel pileBoxChild $ Vec2 1 1
		layoutElement pileBox $ Vec2 (sx - 2) columnHeaderHeight
		placeFreeChild panel scrollBoxChild $ Vec2 1 (1 + columnHeaderHeight)
		layoutElement scrollBox $ Vec2 (sx - 2) (sy - 2 - columnHeaderHeight)

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
	, listBoxSelectedValuesVar = selectedValuesVar
	} (ListBoxItemHandle itemIndex) = do
	values <- readTVar valuesVar
	case IM.lookup itemIndex values of
		Just value -> do
			writeTVar valuesVar $ IM.delete itemIndex values
			modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
				ListBoxItems keyFunc $ S.delete (ListBoxItem itemIndex keyFunc value) items
		Nothing -> return ()
	modifyTVar' selectedValuesVar $ IS.delete itemIndex

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
	, listBoxSelectedValuesVar = selectedValuesVar
	} = do
	writeTVar valuesVar IM.empty
	modifyTVar' itemsVar $ \(ListBoxItems keyFunc _items) -> ListBoxItems keyFunc S.empty
	writeTVar selectedValuesVar IS.empty

-- | Reorder list box using new sort function.
reorderListBox :: Ord k => ListBox v -> (v -> k) -> STM ()
reorderListBox ListBox
	{ listBoxItemsVar = itemsVar
	} newSortFunc = modifyTVar' itemsVar $ \(ListBoxItems _oldSortFunc items) ->
	ListBoxItems newSortFunc $ S.fromList $ flip map (S.toList items)
	$ \(ListBoxItem itemIndex _oldSortFunc value) -> ListBoxItem itemIndex newSortFunc value

instance Element (ListBox v) where
	layoutElement = layoutElement . listBoxPanel
	dabElement = dabElement . listBoxPanel
	elementMouseCursor = elementMouseCursor . listBoxPanel
	renderElement ListBox
		{ listBoxPanel = panel@Panel
			{ panelSizeVar = sizeVar
			}
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerLoweredStyleVariant = StyleVariant
				{ styleVariantNormalStyle = Style
					{ styleFillColor = fillColor
					, styleBorderColor = borderColor
					}
				}
			}
		} position@(Vec2 px py) = do
		Vec2 sx sy <- readTVar sizeVar
		r <- renderElement panel drawer position
		return $ do
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				fillColor borderColor
			renderIntersectScissor $ Vec4 (px + 1) (py + 1) (px + sx - 2) (py + sy - 2)
			r
	processInputEvent = processInputEvent . listBoxPanel
	focusElement = focusElement . listBoxPanel
	unfocusElement = unfocusElement . listBoxPanel

instance Element (ListBoxContent v) where
	layoutElement ListBoxContent
		{ listBoxContentWidthVar = widthVar
		} (Vec2 sx _sy) = writeTVar widthVar sx
	dabElement content (Vec2 x y) = if x < 0 || y < 0 then return False else do
		Vec2 sx sy <- scrollableElementSize content
		return $ x < sx && y < sy
	renderElement _ _ _ = return $ return ()
	processInputEvent ListBoxContent
		{ listBoxContentParent = ListBox
			{ listBoxItemHeight = itemHeight
			, listBoxItemsVar = itemsVar
			, listBoxSelectedValuesVar = selectedValuesVar
			}
		, listBoxContentLastMousePositionVar = lastMousePositionVar
		} inputEvent InputState
		{ inputStateKeyboard = keyboardState
		} = case inputEvent of
		KeyboardInputEvent keyboardEvent -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				maybeLastMousePosition <- readTVar lastMousePositionVar
				case maybeLastMousePosition of
					Just (Vec2 _x y) -> do
						let itemOrderIndex = y `quot` itemHeight
						ListBoxItems _keyFunc items <- readTVar itemsVar
						shiftLPressed <- getKeyState keyboardState KeyShiftL
						shiftRPressed <- getKeyState keyboardState KeyShiftR
						selectedValues <- if shiftLPressed || shiftRPressed then readTVar selectedValuesVar else return IS.empty
						writeTVar selectedValuesVar $
							if itemOrderIndex >= 0 && itemOrderIndex < S.size items then
								let ListBoxItem itemIndex _keyFunc _value = S.elemAt itemOrderIndex items
								in IS.insert itemIndex selectedValues
							else selectedValues
						return True
					Nothing -> return False
			CursorMoveEvent x y -> do
				writeTVar lastMousePositionVar $ Just $ Vec2 x y
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar lastMousePositionVar Nothing
			return True
	focusElement ListBoxContent
		{ listBoxContentFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True
	unfocusElement ListBoxContent
		{ listBoxContentFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance Scrollable (ListBoxContent v) where
	renderScrollableElement ListBoxContent
		{ listBoxContentParent = ListBox
			{ listBoxItemHeight = itemHeight
			, listBoxItemsVar = itemsVar
			, listBoxSelectedValuesVar = selectedValuesVar
			, listBoxColumns = columns
			}
		, listBoxContentFocusedVar = focusedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerLoweredStyleVariant = StyleVariant
				{ styleVariantNormalStyle = normalStyle
				, styleVariantMousedStyle = mousedStyle
				, styleVariantSelectedFocusedStyle = selectedFocusedStyle
				, styleVariantSelectedUnfocusedStyle = selectedUnfocusedStyle
				}
			}
		} (Vec2 px py) (Vec4 left top right bottom) = do
		-- get state
		focused <- readTVar focusedVar
		let unselectedStyle = if focused then mousedStyle else normalStyle
		let selectedStyle = if focused then selectedFocusedStyle else selectedUnfocusedStyle
		selectedValues <- readTVar selectedValuesVar
	
		-- calculate rendering of items
		renderItemColumns <- let
			f x (ListBoxColumn
				{ listBoxColumnDesc = ListBoxColumnDesc
					{ listBoxColumnDescRenderFunc = renderFunc
					}
				, listBoxColumnWidthVar = widthVar
				} : restColumns) = do
				width <- readTVar widthVar
				((x, width, renderFunc) :) <$> f (x + width) restColumns
			f _ [] = return []
			in f px columns
		ListBoxItems _keyFunc items <- readTVar itemsVar
		let
			renderItems y (ListBoxItem itemIndex _keyFunc value : restItems) = do
				let selected = IS.member itemIndex selectedValues
				let style = if selected then selectedStyle else unselectedStyle
				r <- (sequence_ <$>) . forM renderItemColumns $ \(x, width, renderFunc) -> do
					r <- renderFunc value drawer (Vec2 (x + 1) (y + 1)) (Vec2 (width - 2) (itemHeight - 2)) style
					return $ renderScope $ do
						renderIntersectScissor $ Vec4 (x + 1) (y + 1) (x + width - 2) (y + itemHeight - 2)
						r
				let rr = if selected then do
					drawBorderedRectangle canvas
						(Vec4 (px + left) (px + left + 1) (px + right - 1) (px + right))
						(Vec4 y (y + 1) (y + itemHeight - 1) (y + itemHeight))
						(styleFillColor style) (styleBorderColor style)
					r
					else r
				(rr >>) <$> renderItems (y + itemHeight) restItems
			renderItems _ [] = return $ return ()
		renderItems py $ S.toAscList items

	scrollableElementSize ListBoxContent
		{ listBoxContentParent = ListBox
			{ listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			}
		, listBoxContentWidthVar = widthVar
		} = do
		width <- readTVar widthVar
		height <- (itemHeight *) . IM.size <$> readTVar valuesVar
		return $ Vec2 width height

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
