{-|
Module: Flaw.UI.PileBox
Description: Container element allowing user to resize child elements.
License: MIT
-}

module Flaw.UI.PileBox
  ( PileBox(..)
  , PileBoxItem(..)
  , PileBoxItemDesc(..)
  , newPileBox
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix

import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Metrics
import Flaw.UI.Panel

data PileBox = PileBox
  { pileBoxPanel :: !Panel
  , pileBoxElementsPanel :: !Panel
  , pileBoxItems :: ![PileBoxItem]
  , pileBoxItemsChildren :: ![FreeContainerChild Panel]
  , pileBoxGripWidth :: {-# UNPACK #-} !Metric
  , pileBoxHeightVar :: {-# UNPACK #-} !(TVar Metric)
  }

data PileBoxItem = PileBoxItem
  { pileBoxItemParent :: !PileBox
  , pileBoxItemElement :: !SomeElement
  , pileBoxItemElementChild :: !(FreeContainerChild Panel)
  , pileBoxItemWidthVar :: {-# UNPACK #-} !(TVar Metric)
  , pileBoxItemLastMousePositionVar :: {-# UNPACK #-} !(TVar (Maybe Position))
  , pileBoxItemPressedVar :: {-# UNPACK #-} !(TVar Bool)
  }

data PileBoxItemDesc = PileBoxItemDesc
  { pileBoxItemDescElement :: !SomeElement
  , pileBoxItemDescWidth :: {-# UNPACK #-} !Metric
  }

newPileBox :: Metrics -> [PileBoxItemDesc] -> STM PileBox
newPileBox Metrics
  { metricsPileBoxGripWidth = gripWidth
  } itemDescs = mfix $ \pileBox -> do

  -- create main panel
  panel <- newPanel False

  -- create elements panel and add elements to it
  elementsPanel <- newPanel False
  (items, itemsChildren) <- (unzip <$>) . forM itemDescs $ \PileBoxItemDesc
    { pileBoxItemDescElement = e@(SomeElement element)
    , pileBoxItemDescWidth = itemWidth
    }-> do
    elementChild <- addFreeChild elementsPanel element
    widthVar <- newTVar itemWidth
    lastMousePositionVar <- newTVar Nothing
    pressedVar <- newTVar False
    let
      item = PileBoxItem
        { pileBoxItemParent = pileBox
        , pileBoxItemElement = e
        , pileBoxItemElementChild = elementChild
        , pileBoxItemWidthVar = widthVar
        , pileBoxItemLastMousePositionVar = lastMousePositionVar
        , pileBoxItemPressedVar = pressedVar
        }
    -- add item to main panel
    itemChild <- addFreeChild panel item
    return (item, itemChild)

  -- add elements panel to main panel
  void $ addFreeChild panel elementsPanel
  -- set layout handler
  setLayoutHandler panel $ layoutElement elementsPanel

  heightVar <- newTVar 0

  return PileBox
    { pileBoxPanel = panel
    , pileBoxElementsPanel = elementsPanel
    , pileBoxItems = items
    , pileBoxItemsChildren = itemsChildren
    , pileBoxGripWidth = gripWidth
    , pileBoxHeightVar = heightVar
    }

instance Element PileBox where
  layoutElement pileBox@PileBox
    { pileBoxPanel = panel
    , pileBoxHeightVar = heightVar
    } size@(Vec2 _sx sy) = do
    layoutElement panel size
    writeTVar heightVar sy
    relayoutPileBox pileBox
  dabElement = dabElement . pileBoxPanel
  elementMouseCursor = elementMouseCursor . pileBoxPanel
  renderElement = renderElement . pileBoxPanel
  processInputEvent = processInputEvent . pileBoxPanel
  focusElement = focusElement . pileBoxPanel
  unfocusElement = unfocusElement . pileBoxPanel

instance Element PileBoxItem where

  layoutElement _ _ = return ()

  dabElement PileBoxItem
    { pileBoxItemParent = PileBox
      { pileBoxGripWidth = gripWidth
      , pileBoxHeightVar = heightVar
      }
    } (Vec2 x y) =
    if x < 0 || y < 0 || x >= gripWidth then return False
    else do
      height <- readTVar heightVar
      return $ y < height

  elementMouseCursor _ = return MouseCursorSizeWE

  renderElement _ _ _ = return $ return ()

  processInputEvent PileBoxItem
    { pileBoxItemParent = parent
    , pileBoxItemWidthVar = widthVar
    , pileBoxItemLastMousePositionVar = lastMousePositionVar
    , pileBoxItemPressedVar = pressedVar
    } inputEvent _inputState = case inputEvent of
    MouseInputEvent mouseEvent -> case mouseEvent of
      MouseDownEvent LeftMouseButton -> do
        writeTVar pressedVar True
        return True
      MouseUpEvent LeftMouseButton -> do
        writeTVar pressedVar False
        return True
      CursorMoveEvent x y -> do
        let writeLastMousePosition = writeTVar lastMousePositionVar $ Just $ Vec2 x y
        pressed <- readTVar pressedVar
        if pressed then do
          maybeLastMousePosition <- readTVar lastMousePositionVar
          case maybeLastMousePosition of
            Just (Vec2 lx _ly) -> do
              oldWidth <- readTVar widthVar
              let newWidth = max 0 $ oldWidth + x - lx
              writeTVar widthVar newWidth
              writeTVar lastMousePositionVar $ Just $ Vec2 (x - (newWidth - oldWidth)) y
              relayoutPileBox parent
            Nothing -> writeLastMousePosition
        else writeLastMousePosition
        return True
      _ -> return False
    MouseLeaveEvent -> do
      writeTVar lastMousePositionVar Nothing
      return True
    _ -> return False

relayoutPileBox :: PileBox -> STM ()
relayoutPileBox PileBox
  { pileBoxPanel = panel
  , pileBoxElementsPanel = elementsPanel
  , pileBoxItems = items
  , pileBoxItemsChildren = itemsChildren
  , pileBoxGripWidth = gripWidth
  , pileBoxHeightVar = heightVar
  } = do
  height <- readTVar heightVar
  let
    foldWidth totalWidth (PileBoxItem
      { pileBoxItemElement = SomeElement element
      , pileBoxItemElementChild = elementChild
      , pileBoxItemWidthVar = widthVar
      }, itemChild) = do
      width <- readTVar widthVar
      layoutElement element $ Vec2 width height
      placeFreeChild elementsPanel elementChild $ Vec2 totalWidth 0
      placeFreeChild panel itemChild $ Vec2 (totalWidth + width - gripWidth `quot` 2) 0
      return $ totalWidth + width
  foldM_ foldWidth 0 $ zip items itemsChildren
