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
	, getListBoxSelectedValues
	, newListBoxTextColumnDesc
	) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
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
	-- | Change handler.
	, listBoxChangeHandlerVar :: {-# UNPACK #-} !(TVar (STM ()))
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
		, listBoxColumnDescWidth :: {-# UNPACK #-} !Metric
		, listBoxColumnDescKeyFunc :: !(v -> k)
		, listBoxColumnDescRenderFunc :: !(forall c d. Context c d => v -> Drawer d -> Position -> Size -> Style -> STM (Render c ()))
		} -> ListBoxColumnDesc v

data ListBoxContent v = ListBoxContent
	{ listBoxContentParent :: !(ListBox v)
	, listBoxContentScrollBarVar :: {-# UNPACK #-} !(TVar ScrollBar)
	, listBoxContentSizeVar :: {-# UNPACK #-} !(TVar Size)
	, listBoxContentFocusedVar :: {-# UNPACK #-} !(TVar Bool)
	, listBoxContentLastMousePositionVar :: {-# UNPACK #-} !(TVar (Maybe Position))
	}

newListBox :: Metrics -> [ListBoxColumnDesc v] -> STM (ListBox v)
newListBox metrics@Metrics
	{ metricsListBoxColumnHeaderHeight = columnHeaderHeight
	, metricsListBoxItemHeight = itemHeight
	, metricsScrollBarWidth = scrollBarWidth
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
		changeHandlerVar <- newTVar $ return ()
		return ListBox
			{ listBoxPanel = panel
			, listBoxColumnHeaderHeight = columnHeaderHeight
			, listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			, listBoxItemsVar = itemsVar
			, listBoxSelectedValuesVar = selectedValuesVar
			, listBoxNextItemIndexVar = nextItemIndexVar
			, listBoxColumns = columns
			, listBoxChangeHandlerVar = changeHandlerVar
			}

	-- pile box for column headers
	pileBox <- newPileBox metrics $ flip map columns $ \column@ListBoxColumn
		{ listBoxColumnDesc = ListBoxColumnDesc
			{ listBoxColumnDescWidth = columnWidth
			}
		} -> PileBoxItemDesc
		{ pileBoxItemDescElement = SomeElement column
		, pileBoxItemDescWidth = columnWidth
		}
	pileBoxChild <- addFreeChild panel pileBox

	-- content element
	scrollBarVar <- newTVar undefined
	contentSizeVar <- newTVar $ Vec2 0 0
	focusedVar <- newTVar False
	lastMousePositionVar <- newTVar Nothing
	let content = ListBoxContent
		{ listBoxContentParent = listBox
		, listBoxContentScrollBarVar = scrollBarVar
		, listBoxContentSizeVar = contentSizeVar
		, listBoxContentFocusedVar = focusedVar
		, listBoxContentLastMousePositionVar = lastMousePositionVar
		}

	-- scroll box
	scrollBox <- newScrollBox content
	scrollBoxChild <- addFreeChild panel scrollBox

	-- scroll bar
	scrollBar <- newVerticalScrollBar scrollBox
	scrollBarChild <- addFreeChild panel scrollBar

	writeTVar scrollBarVar scrollBar

	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel pileBoxChild $ Vec2 1 1
		layoutElement pileBox $ Vec2 (sx - 2) columnHeaderHeight
		placeFreeChild panel scrollBoxChild $ Vec2 1 (1 + columnHeaderHeight)
		layoutElement scrollBox $ Vec2 (sx - 1 - scrollBarWidth) (sy - 2 - columnHeaderHeight)
		placeFreeChild panel scrollBarChild $ Vec2 (sx - scrollBarWidth) (1 + columnHeaderHeight)
		layoutElement scrollBar $ Vec2 scrollBarWidth (sy - 1 - columnHeaderHeight)

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
	, listBoxChangeHandlerVar = changeHandlerVar
	} (ListBoxItemHandle itemIndex) = do
	values <- readTVar valuesVar
	case IM.lookup itemIndex values of
		Just value -> do
			writeTVar valuesVar $ IM.delete itemIndex values
			modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
				ListBoxItems keyFunc $ S.delete (ListBoxItem itemIndex keyFunc value) items
		Nothing -> return ()
	selectedValues <- readTVar selectedValuesVar
	when (IS.member itemIndex selectedValues) $ do
		writeTVar selectedValuesVar $ IS.delete itemIndex selectedValues
		join $ readTVar changeHandlerVar

-- | Change list item by handle.
-- List item's handle remains valid.
changeListBoxItem :: ListBox v -> ListBoxItemHandle v -> v -> STM ()
changeListBoxItem ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	, listBoxSelectedValuesVar = selectedValuesVar
	, listBoxChangeHandlerVar = changeHandlerVar
	} (ListBoxItemHandle itemIndex) newValue = do
	values <- readTVar valuesVar
	case IM.lookup itemIndex values of
		Just oldValue -> do
			writeTVar valuesVar $ IM.insert itemIndex newValue values
			modifyTVar' itemsVar $ \(ListBoxItems keyFunc items) ->
				ListBoxItems keyFunc
				$ S.insert (ListBoxItem itemIndex keyFunc newValue)
				$ S.delete (ListBoxItem itemIndex keyFunc oldValue) items
			selectedValues <- readTVar selectedValuesVar
			when (IS.member itemIndex selectedValues) $ join $ readTVar changeHandlerVar
		Nothing -> return ()

-- | Remove all items from list box.
clearListBox :: ListBox v -> STM ()
clearListBox ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxItemsVar = itemsVar
	, listBoxSelectedValuesVar = selectedValuesVar
	, listBoxChangeHandlerVar = changeHandlerVar
	} = do
	writeTVar valuesVar IM.empty
	modifyTVar' itemsVar $ \(ListBoxItems keyFunc _items) -> ListBoxItems keyFunc S.empty
	selectionWasEmpty <- IS.null <$> readTVar selectedValuesVar
	writeTVar selectedValuesVar IS.empty
	unless selectionWasEmpty $ join $ readTVar changeHandlerVar

-- | Reorder list box using new sort function.
reorderListBox :: Ord k => ListBox v -> (v -> k) -> STM ()
reorderListBox ListBox
	{ listBoxItemsVar = itemsVar
	} newSortFunc = modifyTVar' itemsVar $ \(ListBoxItems _oldSortFunc items) ->
	ListBoxItems newSortFunc $ S.fromList $ flip map (S.toList items)
	$ \(ListBoxItem itemIndex _oldSortFunc value) -> ListBoxItem itemIndex newSortFunc value

-- | Get list of selected values from list box.
getListBoxSelectedValues :: ListBox v -> STM [v]
getListBoxSelectedValues ListBox
	{ listBoxValuesVar = valuesVar
	, listBoxSelectedValuesVar = selectedValuesVar
	} = do
	values <- readTVar valuesVar
	selectedValues <- readTVar selectedValuesVar
	return $ map (fromJust . flip IM.lookup values) $ IS.toList selectedValues

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

instance HasChangeHandler (ListBox v) where
	setChangeHandler = writeTVar . listBoxChangeHandlerVar

instance Element (ListBoxContent v) where

	layoutElement ListBoxContent
		{ listBoxContentSizeVar = sizeVar
		} = writeTVar sizeVar

	dabElement _ _ = return True

	renderElement _ _ _ = return $ return ()

	processInputEvent ListBoxContent
		{ listBoxContentParent = ListBox
			{ listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			, listBoxItemsVar = itemsVar
			, listBoxSelectedValuesVar = selectedValuesVar
			, listBoxChangeHandlerVar = changeHandlerVar
			}
		, listBoxContentScrollBarVar = scrollBarVar
		, listBoxContentLastMousePositionVar = lastMousePositionVar
		} inputEvent inputState@InputState
		{ inputStateKeyboard = keyboardState
		} = do
		scrollBar@ScrollBar
			{ scrollBarScrollBox = scrollBox@ScrollBox
				{ scrollBoxSizeVar = boxSizeVar
				}
			} <- readTVar scrollBarVar

		let
			moveSelection getEdgeItemIndex adjustItemOrderIndex = do
				selectedValues <- readTVar selectedValuesVar
				if IS.null selectedValues then selectByItemOrderIndex 0 else do
					values <- readTVar valuesVar
					ListBoxItems keyFunc items <- readTVar itemsVar
					let
						itemIndex = getEdgeItemIndex selectedValues
						value = fromJust $ IM.lookup itemIndex values
						itemOrderIndex = S.findIndex (ListBoxItem itemIndex keyFunc value) items
						itemOrderIndexToSelect = adjustItemOrderIndex itemOrderIndex
					selectByItemOrderIndex itemOrderIndexToSelect
			-- select by item order index, possibly unselecting currently selected items
			selectByItemOrderIndex itemOrderIndex = do
				ListBoxItems _keyFunc items <- readTVar itemsVar
				when (itemOrderIndex >= 0 && itemOrderIndex < S.size items) $ do
					shiftLPressed <- getKeyState keyboardState KeyShiftL
					shiftRPressed <- getKeyState keyboardState KeyShiftR
					ctrlLPressed <- getKeyState keyboardState KeyControlL
					ctrlRPressed <- getKeyState keyboardState KeyControlR
					-- clear selection if shift or ctrl is not pressed
					selectedValues <- if shiftLPressed || shiftRPressed || ctrlLPressed || ctrlRPressed then readTVar selectedValuesVar else return IS.empty
					writeTVar selectedValuesVar $
						let ListBoxItem itemIndex _keyFunc _value = S.elemAt itemOrderIndex items
						in IS.insert itemIndex selectedValues
					-- ensure selected item is visible
					let itemY = itemOrderIndex * itemHeight
					ensureVisibleScrollBoxArea scrollBox $ Vec4 0 itemY 0 (itemY + itemHeight)
					-- call change handler
					join $ readTVar changeHandlerVar

		processedByScrollBar <- processScrollBarEvent scrollBar inputEvent inputState
		if processedByScrollBar then return True else case inputEvent of
			KeyboardInputEvent keyboardEvent -> case keyboardEvent of
				KeyDownEvent KeyDown -> do
					moveSelection IS.findMax (+ 1)
					return True
				KeyDownEvent KeyUp -> do
					moveSelection IS.findMin (+ (-1))
					return True
				KeyDownEvent KeyPageDown -> do
					Vec2 _sx sy <- readTVar boxSizeVar
					ListBoxItems _keyFunc items <- readTVar itemsVar
					moveSelection IS.findMax $ \i -> min (S.size items - 1) $ i + sy `quot` itemHeight
					return True
				KeyDownEvent KeyPageUp -> do
					Vec2 _sx sy <- readTVar boxSizeVar
					moveSelection IS.findMin $ \i -> max 0 $ i - sy `quot` itemHeight
					return True
				KeyDownEvent KeyHome -> do
					selectByItemOrderIndex 0
					return True
				KeyDownEvent KeyEnd -> do
					ListBoxItems _keyFunc items <- readTVar itemsVar
					unless (S.null items) $ selectByItemOrderIndex (S.size items - 1)
					return True
				_ -> return False
			MouseInputEvent mouseEvent -> case mouseEvent of
				MouseDownEvent LeftMouseButton -> do
					maybeLastMousePosition <- readTVar lastMousePositionVar
					case maybeLastMousePosition of
						Just (Vec2 _x y) -> do
							selectByItemOrderIndex $ y `quot` itemHeight
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
			renderItems _i y _ | y >= py + bottom = return $ return ()
			renderItems _i _y [] = return $ return ()
			renderItems i y (ListBoxItem itemIndex _keyFunc value : restItems) = do
				let
					selected = IS.member itemIndex selectedValues
					isOdd = (i .&. 1) > 0
					style = if selected then selectedStyle else unselectedStyle
				r <- (sequence_ <$>) . forM renderItemColumns $ \(x, width, renderFunc) -> do
					r <- renderFunc value drawer (Vec2 (x + 1) (y + 1)) (Vec2 (width - 2) (itemHeight - 2)) style
					return $ renderScope $ do
						renderIntersectScissor $ Vec4 (x + 1) (y + 1) (x + width - 2) (y + itemHeight - 2)
						r
				let rr =
					if selected then do
						drawBorderedRectangle canvas
							(Vec4 (px + left) (px + left + 1) (px + right - 1) (px + right))
							(Vec4 y (y + 1) (y + itemHeight - 1) (y + itemHeight))
							(styleFillColor style) (styleBorderColor style)
						r
					else if isOdd then do
						let evenColor = styleFillColor selectedStyle * Vec4 1 1 1 0.05
						drawBorderedRectangle canvas
							(Vec4 (px + left) (px + left) (px + right) (px + right))
							(Vec4 y y (y + itemHeight) (y + itemHeight))
							evenColor evenColor
						r
					else r
				(rr >>) <$> renderItems (i + 1) (y + itemHeight) restItems
			topOrderedIndex = top `quot` itemHeight
			(visibleItems, firstVisibleItemOrderedIndex) = if topOrderedIndex <= 0 then (items, 0)
				else if topOrderedIndex >= S.size items then (S.empty, 0)
				else let
					ListBoxItem firstVisibleItemIndex firstVisibleItemKeyFunc firstVisibleItemValue = S.elemAt topOrderedIndex items
					-- split by special non-existent item which will be just before first item
					in (snd $ S.split (ListBoxItem (firstVisibleItemIndex - 1) firstVisibleItemKeyFunc firstVisibleItemValue) items, topOrderedIndex)
		renderItems firstVisibleItemOrderedIndex (py + firstVisibleItemOrderedIndex * itemHeight) $ S.toAscList visibleItems

	scrollableElementSize ListBoxContent
		{ listBoxContentParent = ListBox
			{ listBoxItemHeight = itemHeight
			, listBoxValuesVar = valuesVar
			}
		, listBoxContentSizeVar = sizeVar
		} = do
		Vec2 sx _sy <- readTVar sizeVar
		height <- (itemHeight *) . IM.size <$> readTVar valuesVar
		return $ Vec2 sx height

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

-- | Description of most normal column: text column title, item is shown as text.
newListBoxTextColumnDesc
	:: Ord k
	=> T.Text -- ^ Column title.
	-> Metric -- ^ Column width.
	-> (v -> k) -- ^ Key function, returns key to sort by.
	-> (v -> T.Text) -- ^ Display text function, returns text to display for item.
	-> STM (ListBoxColumnDesc v)
newListBoxTextColumnDesc title width keyFunc textFunc = do
	label <- newTextLabel
	setText label title
	return ListBoxColumnDesc
		{ listBoxColumnDescVisual = SomeVisual label
		, listBoxColumnDescWidth = width
		, listBoxColumnDescKeyFunc = keyFunc
		, listBoxColumnDescRenderFunc = \value drawer position size style -> return $ renderLabel (textFunc value) fontScriptUnknown LabelStyleText drawer position size style
		}
