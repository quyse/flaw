{-|
Module: Flaw.UI.ScrollBox
Description: Scroll box.
License: MIT
-}

module Flaw.UI.ScrollBox
  ( ScrollBox(..)
  , newScrollBox
  , ScrollBar(..)
  , newScrollBar
  , newVerticalScrollBar
  , newHorizontalScrollBar
  , processScrollBarEvent
  , ensureVisibleScrollBoxArea
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data ScrollBox = ScrollBox
  { scrollBoxElement :: !SomeScrollable
  -- | Position of left-top corner of child element
  -- relative to scroll box' left-top corner, i.e. <= 0.
  , scrollBoxScrollVar :: {-# UNPACK #-} !(TVar Position)
  , scrollBoxSizeVar :: {-# UNPACK #-} !(TVar Size)
  }

newScrollBox :: Scrollable e => e -> STM ScrollBox
newScrollBox element = do
  scrollVar <- newTVar $ Vec2 0 0
  sizeVar <- newTVar $ Vec2 0 0
  return ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxScrollVar = scrollVar
    , scrollBoxSizeVar = sizeVar
    }

instance Element ScrollBox where

  layoutElement ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxSizeVar = sizeVar
    } size = do
    writeTVar sizeVar size
    layoutElement element size

  dabElement ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxScrollVar = scrollVar
    , scrollBoxSizeVar = sizeVar
    } (Vec2 x y) = if x < 0 || y < 0 then return False else do
    size <- readTVar sizeVar
    let Vec2 sx sy = size
    if x < sx && y < sy then do
      scroll <- readTVar scrollVar
      let Vec2 ox oy = scroll
      dabElement element $ Vec2 (x - ox) (y - oy)
    else return False

  elementMouseCursor ScrollBox
    { scrollBoxElement = SomeScrollable element
    } = elementMouseCursor element

  renderElement ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxScrollVar = scrollVar
    , scrollBoxSizeVar = sizeVar
    } drawer position@(Vec2 px py) = do
    scroll <- readTVar scrollVar
    size <- readTVar sizeVar
    ssize <- scrollableElementSize element
    -- correct scroll if needed
    let
      Vec2 ox oy = scroll
      Vec2 sx sy = size
      Vec2 ssx ssy = ssize
      newScroll@(Vec2 nox noy) = Vec2
        (min 0 $ max ox $ sx - ssx)
        (min 0 $ max oy $ sy - ssy)

    when (scroll /= newScroll) $ writeTVar scrollVar newScroll
    r <- renderScrollableElement element drawer (position + newScroll) (Vec4 (-nox) (-noy) (sx - nox) (sy - noy))
    return $ do
      renderIntersectScissor $ Vec4 px py (px + sx) (py + sy)
      r

  processInputEvent ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxScrollVar = scrollVar
    } inputEvent inputState = case inputEvent of
    MouseInputEvent mouseEvent -> case mouseEvent of
      CursorMoveEvent x y -> do
        scroll <- readTVar scrollVar
        let Vec2 ox oy = scroll
        processInputEvent element (MouseInputEvent (CursorMoveEvent (x - ox) (y - oy))) inputState
      _ -> processInputEvent element inputEvent inputState
    _ -> processInputEvent element inputEvent inputState

  focusElement ScrollBox
    { scrollBoxElement = SomeScrollable element
    } = focusElement element

  unfocusElement ScrollBox
    { scrollBoxElement = SomeScrollable element
    } = unfocusElement element

data ScrollBar = ScrollBar
  { scrollBarScrollBox :: !ScrollBox
  , scrollBarDirection :: {-# UNPACK #-} !(Vec2 Metric)
  , scrollBarSizeVar :: {-# UNPACK #-} !(TVar Size)
  , scrollBarLastMousePositionVar :: {-# UNPACK #-} !(TVar (Maybe Position))
  , scrollBarPressedVar :: {-# UNPACK #-} !(TVar Bool)
  }

newScrollBar :: Vec2 Metric -> ScrollBox -> STM ScrollBar
newScrollBar direction scrollBox = do
  sizeVar <- newTVar $ Vec2 0 0
  lastMousePositionVar <- newTVar Nothing
  pressedVar <- newTVar False
  return ScrollBar
    { scrollBarScrollBox = scrollBox
    , scrollBarDirection = direction
    , scrollBarSizeVar = sizeVar
    , scrollBarLastMousePositionVar = lastMousePositionVar
    , scrollBarPressedVar = pressedVar
    }

newVerticalScrollBar :: ScrollBox -> STM ScrollBar
newVerticalScrollBar = newScrollBar $ Vec2 0 1

newHorizontalScrollBar :: ScrollBox -> STM ScrollBar
newHorizontalScrollBar = newScrollBar $ Vec2 1 0

instance Element ScrollBar where

  layoutElement ScrollBar
    { scrollBarSizeVar = sizeVar
    } = writeTVar sizeVar

  dabElement ScrollBar
    { scrollBarSizeVar = sizeVar
    } (Vec2 x y) = if x < 0 || y < 0 then return False else do
    size <- readTVar sizeVar
    let Vec2 sx sy = size
    return $ x < sx && y < sy

  renderElement scrollBar@ScrollBar
    { scrollBarSizeVar = barSizeVar
    , scrollBarLastMousePositionVar = lastMousePositionVar
    , scrollBarPressedVar = pressedVar
    } Drawer
    { drawerCanvas = canvas
    , drawerStyles = DrawerStyles
      { drawerFlatStyleVariant = StyleVariant
        { styleVariantNormalStyle = flatNormalStyle
        }
      , drawerRaisedStyleVariant = StyleVariant
        { styleVariantNormalStyle = raisedNormalStyle
        , styleVariantMousedStyle = raisedMousedStyle
        , styleVariantPressedStyle = raisedPressedStyle
        }
      }
    } (Vec2 px py) = do
    barSize <- readTVar barSizeVar
    let Vec2 sx sy = barSize
    piece <- scrollBarPiece scrollBar
    moused <- isJust <$> readTVar lastMousePositionVar
    pressed <- readTVar pressedVar
    let
      pieceStyle
        | pressed = raisedPressedStyle
        | moused = raisedMousedStyle
        | otherwise = raisedNormalStyle
    return $ do
      -- render border
      drawBorderedRectangle canvas
        (Vec4 px (px + 1) (px + sx - 1) (px + sx))
        (Vec4 py (py + 1) (py + sy - 1) (py + sy))
        (styleFillColor flatNormalStyle) (styleBorderColor flatNormalStyle)
      case piece of
        Just ScrollBarPiece
          { scrollBarPieceRect = pieceRect
          } -> let Vec4 ppx ppy pqx pqy = pieceRect + Vec4 px py px py in
          -- render piece
          drawBorderedRectangle canvas
            (Vec4 ppx (ppx + 1) (pqx - 1) pqx)
            (Vec4 ppy (ppy + 1) (pqy - 1) pqy)
            (styleFillColor pieceStyle) (styleBorderColor pieceStyle)
        Nothing -> return ()

  processInputEvent scrollBar@ScrollBar
    { scrollBarScrollBox = ScrollBox
      { scrollBoxScrollVar = scrollVar
      }
    , scrollBarLastMousePositionVar = lastMousePositionVar
    , scrollBarPressedVar = pressedVar
    } inputEvent _inputState = case inputEvent of
    MouseInputEvent mouseEvent -> case mouseEvent of
      MouseDownEvent LeftMouseButton -> do
        writeTVar pressedVar True
        return True
      MouseUpEvent LeftMouseButton -> do
        writeTVar pressedVar False
        return True
      RawMouseMoveEvent _x _y z -> do
        piece <- scrollBarPiece scrollBar
        case piece of
          Just ScrollBarPiece
            { scrollBarPieceOffsetMultiplier = offsetMultiplier
            } -> do
            modifyTVar' scrollVar (+ signum offsetMultiplier * vecFromScalar (floor (z * (-15))))
            return True
          Nothing -> return False
      CursorMoveEvent x y -> do
        pressed <- readTVar pressedVar
        when pressed $ do
          maybeLastMousePosition <- readTVar lastMousePositionVar
          case maybeLastMousePosition of
            Just lastMousePosition -> do
              piece <- scrollBarPiece scrollBar
              case piece of
                Just ScrollBarPiece
                  { scrollBarPieceOffsetMultiplier = offsetMultiplier
                  } -> modifyTVar' scrollVar (+ (Vec2 x y - lastMousePosition) * offsetMultiplier)
                Nothing -> return ()
            Nothing -> return ()
        writeTVar lastMousePositionVar $ Just $ Vec2 x y
        return True
      _ -> return False
    MouseLeaveEvent -> do
      writeTVar lastMousePositionVar Nothing
      writeTVar pressedVar False
      return True
    _ -> return False

-- | Internal information about piece.
data ScrollBarPiece = ScrollBarPiece
  { scrollBarPieceRect :: {-# UNPACK #-} !Rect
  , scrollBarPieceOffsetMultiplier :: {-# UNPACK #-} !(Vec2 Metric)
  }

-- | Get scroll bar piece rect and piece offset multiplier.
scrollBarPiece :: ScrollBar -> STM (Maybe ScrollBarPiece)
scrollBarPiece ScrollBar
  { scrollBarScrollBox = ScrollBox
    { scrollBoxElement = SomeScrollable element
    , scrollBoxScrollVar = scrollVar
    , scrollBoxSizeVar = boxSizeVar
    }
  , scrollBarDirection = direction
  , scrollBarSizeVar = barSizeVar
  } = do
  barSize <- readTVar barSizeVar
  let Vec2 sx sy = barSize
  boxSize <- readTVar boxSizeVar
  scrollableSize <- scrollableElementSize element
  scroll <- readTVar scrollVar
  let
    padding = 2
    contentOffset = dot scroll direction
    boxLength = dot boxSize direction
    contentLength = dot scrollableSize direction
    barLength = dot barSize direction - padding * 2
    minPieceLength = min sx sy - padding * 2
    pieceLength = max minPieceLength $ (barLength * boxLength) `quot` max 1 contentLength
    pieceOffset = (negate contentOffset * (barLength - pieceLength)) `quot` max 1 (contentLength - boxLength)
    Vec2 ppx ppy = vecFromScalar padding + direction * vecFromScalar pieceOffset
    Vec2 psx psy = vecfmap (max minPieceLength) $ direction * vecFromScalar pieceLength
    piece = if contentLength > boxLength then
      Just ScrollBarPiece
        { scrollBarPieceRect = Vec4 ppx ppy (ppx + psx) (ppy + psy)
        , scrollBarPieceOffsetMultiplier = direction * vecFromScalar (negate $ (contentLength - boxLength) `quot` max 1 (barLength - pieceLength))
        }
      else Nothing
  return piece

-- | Process possibly scroll bar event.
-- Could be used for passing scrolling events from other elements.
processScrollBarEvent :: ScrollBar -> InputEvent -> InputState -> STM Bool
processScrollBarEvent scrollBar inputEvent inputState = case inputEvent of
  MouseInputEvent RawMouseMoveEvent {} -> processInputEvent scrollBar inputEvent inputState
  _ -> return False

-- | Adjust scrolling so the specified area (in content coords) is visible.
ensureVisibleScrollBoxArea :: ScrollBox -> Rect -> STM ()
ensureVisibleScrollBoxArea ScrollBox
  { scrollBoxScrollVar = scrollVar
  , scrollBoxSizeVar = sizeVar
  } (Vec4 left top right bottom) = do
  scroll <- readTVar scrollVar
  let Vec2 ox oy = scroll
  size <- readTVar sizeVar
  let Vec2 sx sy = size
  -- dimensions are to be adjusted independently
  -- function to adjust one dimension
  let
    adjust o s a b =
      if a + o >= 0 then
        if b + o <= s then o
        else s - b
      else -a
    newScroll = Vec2 (adjust ox sx left right) (adjust oy sy top bottom)
  unless (scroll == newScroll) $ writeTVar scrollVar newScroll
