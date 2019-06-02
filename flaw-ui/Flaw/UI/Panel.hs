{-|
Module: Flaw.UI.Panel
Description: Panel is a free container element.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.UI.Panel
  ( Panel(..)
  , newPanel
  ) where

import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Flaw.Graphics
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI

data Panel = Panel
  { panelChildrenVar :: {-# UNPACK #-} !(TVar (S.Set PanelChild))
  , panelChildIndexVar :: {-# UNPACK #-} !(TVar Int)
  , panelChildrenRenderOrderVar :: {-# UNPACK #-} !(TVar [PanelChild])
  , panelLayoutHandlerVar :: {-# UNPACK #-} !(TVar (Size -> STM ()))
  , panelSizeVar :: {-# UNPACK #-} !(TVar Size)
  , panelStickyFocus :: !Bool
  , panelFocusedChildVar :: {-# UNPACK #-} !(TVar (Maybe PanelChild))
  , panelLastFocusedChildVar :: {-# UNPACK #-} !(TVar (Maybe PanelChild))
  , panelLastMousedChildVar :: {-# UNPACK #-} !(TVar (Maybe PanelChild))
  , panelDefaultElementVar :: {-# UNPACK #-} !(TVar (Maybe SomeElement))
  , panelCancelElementVar :: {-# UNPACK #-} !(TVar (Maybe SomeElement))
  , panelCommitHandlerVar :: {-# UNPACK #-} !(TVar (CommitReason -> STM Bool))
  }

data PanelChild = PanelChild
  { panelChildIndex :: {-# UNPACK #-} !Int
  , panelChildElement :: !SomeElement
  , panelChildPositionVar :: {-# UNPACK #-} !(TVar Position)
  }

newPanel :: Bool -> STM Panel
newPanel stickyFocus = do
  childrenVar <- newTVar S.empty
  childIndexVar <- newTVar 0
  childrenRenderOrderVar <- newTVar []
  layoutHandlerVar <- newTVar $ \_ -> return ()
  sizeVar <- newTVar $ Vec2 0 0
  focusedChildVar <- newTVar Nothing
  lastFocusedChildVar <- newTVar Nothing
  lastMousedChildVar <- newTVar Nothing
  defaultElementVar <- newTVar Nothing
  cancelElementVar <- newTVar Nothing
  commitHandlerVar <- newTVar $ const $ return False
  return Panel
    { panelChildrenVar = childrenVar
    , panelChildIndexVar = childIndexVar
    , panelChildrenRenderOrderVar = childrenRenderOrderVar
    , panelLayoutHandlerVar = layoutHandlerVar
    , panelSizeVar = sizeVar
    , panelStickyFocus = stickyFocus
    , panelFocusedChildVar = focusedChildVar
    , panelLastFocusedChildVar = lastFocusedChildVar
    , panelLastMousedChildVar = lastMousedChildVar
    , panelDefaultElementVar = defaultElementVar
    , panelCancelElementVar = cancelElementVar
    , panelCommitHandlerVar = commitHandlerVar
    }

instance Eq PanelChild where
  child1 == child2 = panelChildIndex child1 == panelChildIndex child2

instance Ord PanelChild where
  compare child1 child2 = compare (panelChildIndex child1) (panelChildIndex child2)

instance Element Panel where

  layoutElement Panel
    { panelLayoutHandlerVar = layoutHandlerVar
    , panelSizeVar = sizeVar
    } size = do
    layoutHandler <- readTVar layoutHandlerVar
    layoutHandler size
    writeTVar sizeVar size

  dabElement Panel
    { panelChildrenVar = childrenVar
    , panelSizeVar = sizeVar
    } point@(Vec2 px py) =
    if px < 0 || py < 0 then return False
    else do
      size <- readTVar sizeVar
      let Vec2 sx sy = size
      if px >= sx || py >= sy then return False
      else let
        dabChildren (PanelChild
          { panelChildElement = SomeElement element
          , panelChildPositionVar = childPositionVar
          } : restChildren) = do
          childPosition <- readTVar childPositionVar
          r <- dabElement element $ point - childPosition
          if r then return True else dabChildren restChildren
        dabChildren [] = return False
        in dabChildren . S.toDescList =<< readTVar childrenVar

  elementMouseCursor Panel
    { panelLastMousedChildVar = lastMousedChildVar
    } = do
    lastMousedChild <- readTVar lastMousedChildVar
    case lastMousedChild of
      Just PanelChild
        { panelChildElement = SomeElement childElement
        } -> elementMouseCursor childElement
      Nothing -> return MouseCursorArrow

  renderElement Panel
    { panelChildrenRenderOrderVar = childrenRenderOrderVar
    , panelSizeVar = sizeVar
    } drawer position@(Vec2 px py) = do
    size <- readTVar sizeVar
    let Vec2 sx sy = size
    -- compose rendering of children
    childrenRenderOrder <- readTVar childrenRenderOrderVar
    let
      drawChild PanelChild
        { panelChildElement = SomeElement element
        , panelChildPositionVar = childPositionVar
        } = do
        childPosition <- readTVar childPositionVar
        renderScope <$> renderElement element drawer (position + childPosition)
    renderChildren <- foldrM (\a b -> fmap (>> b) a) (return ()) $ map drawChild childrenRenderOrder
    -- return
    return $ do
      renderIntersectScissor $ Vec4 px py (px + sx) (py + sy)
      renderChildren

  processInputEvent panel@Panel
    { panelChildrenVar = childrenVar
    , panelChildrenRenderOrderVar = childrenRenderOrderVar
    , panelStickyFocus = stickyFocus
    , panelFocusedChildVar = focusedChildVar
    , panelLastFocusedChildVar = lastFocusedChildVar
    , panelLastMousedChildVar = lastMousedChildVar
    , panelDefaultElementVar = defaultElementVar
    , panelCancelElementVar = cancelElementVar
    , panelCommitHandlerVar = commitHandlerVar
    } inputEvent inputState@InputState
    { inputStateKeyboard = keyboardState
    , inputStateMouse = mouseState
    } = case inputEvent of

    KeyboardInputEvent keyboardEvent -> do
      -- own processing: handle tab-moving focus, default and cancel elements
      let
        tryPassToDefaultElement = do
          defaultElement <- readTVar defaultElementVar
          processed <- case defaultElement of
            Just (SomeElement element) -> processInputEvent element inputEvent inputState
            Nothing -> return False
          if processed then return True else do
            commitHandler <- readTVar commitHandlerVar
            commitHandler CommitAccept
        tryPassToCancelElement = do
          cancelElement <- readTVar cancelElementVar
          processed <- case cancelElement of
            Just (SomeElement element) -> processInputEvent element inputEvent inputState
            Nothing -> return False
          if processed then return True else do
            commitHandler <- readTVar commitHandlerVar
            commitHandler CommitCancel
        moveFocus back = do
          focusedChild <- readTVar focusedChildVar
          children <- readTVar childrenVar
          case focusedChild of
            Just child@PanelChild
              { panelChildElement = SomeElement focusedElement
              } -> do
              let (before, after) = S.split child children
              focusedNewChild <- focusSomeChild panel $
                if back then
                  S.toDescList before ++ (if stickyFocus then S.toDescList after else [])
                else
                  S.toAscList after ++ (if stickyFocus then S.toAscList before else [])
              when focusedNewChild $ unfocusElement focusedElement
              return focusedNewChild
            Nothing -> focusSomeChild panel $ (if back then S.toDescList else S.toAscList) children
        ownProcessEvent = case keyboardEvent of
          KeyDownEvent KeyTab -> do
            keyShiftLPressed <- getKeyState keyboardState KeyShiftL
            keyShiftRPressed <- getKeyState keyboardState KeyShiftR
            moveFocus $ keyShiftLPressed || keyShiftRPressed
          KeyDownEvent KeyRight -> moveFocus False
          KeyDownEvent KeyDown -> moveFocus False
          KeyDownEvent KeyLeft -> moveFocus True
          KeyDownEvent KeyUp -> moveFocus True
          KeyDownEvent KeyReturn -> tryPassToDefaultElement
          KeyUpEvent KeyReturn -> tryPassToDefaultElement
          KeyDownEvent KeyEscape -> tryPassToCancelElement
          KeyUpEvent KeyEscape -> tryPassToCancelElement
          _ -> return False

      -- send keyboard event to focused element
      focusedChild <- readTVar focusedChildVar
      case focusedChild of
        Just PanelChild
          { panelChildElement = SomeElement element
          } -> do
          processed <- processInputEvent element inputEvent inputState
          if processed then return True else ownProcessEvent
        Nothing -> ownProcessEvent

    MouseInputEvent mouseEvent -> do
      -- send event to last moused child (without any correction)
      let
        sendToLastChild = do
          lastMousedChild <- readTVar lastMousedChildVar
          case lastMousedChild of
            Just PanelChild
              { panelChildElement = SomeElement lastMousedChildElement
              } -> processInputEvent lastMousedChildElement inputEvent inputState
            Nothing -> return False
      -- select by mouse event
      case mouseEvent of
        MouseDownEvent _mouseButton -> do
          -- focus-by-click
          lastMousedChild <- readTVar lastMousedChildVar
          case lastMousedChild of
            Just PanelChild
              { panelChildElement = SomeElement lastMousedChildElement
              } -> do
              -- get currently focused child
              focusedChild <- readTVar focusedChildVar
              -- if it's not the same one
              when (lastMousedChild /= focusedChild) $ do
                -- try to focus element under mouse
                focusAccepted <- focusElement lastMousedChildElement
                when focusAccepted $ do
                  writeTVar focusedChildVar lastMousedChild
                  writeTVar lastFocusedChildVar lastMousedChild
                  -- unfocus previously focused child
                  case focusedChild of
                    Just PanelChild
                      { panelChildElement = SomeElement focusedElement
                      } -> unfocusElement focusedElement
                    Nothing -> return ()
              -- send mouse event in any case
              processInputEvent lastMousedChildElement inputEvent inputState
            Nothing -> return False
        MouseUpEvent _mouseButton -> sendToLastChild
        RawMouseMoveEvent _dx _dy _dz -> sendToLastChild
        CursorMoveEvent x y -> do
          -- if no mouse button is pressed, we can update "moused" child
          -- so we do "mouse capture" by default
          mousePressed <- fmap or $ forM [minBound .. maxBound] $ getMouseButtonState mouseState
          mousedChild <- if mousePressed then readTVar lastMousedChildVar else do
            -- determine child with the mouse on it
            let
              pickChild (child@PanelChild
                { panelChildElement = SomeElement element
                , panelChildPositionVar = childPositionVar
                } : restChildren) point = do
                -- correct position and ask child element
                childPosition <- readTVar childPositionVar
                r <- dabElement element $ point - childPosition
                if r then return $ Just child else pickChild restChildren point
              pickChild [] _point = return Nothing
            childrenRenderOrder <- readTVar childrenRenderOrderVar
            mousedChild <- pickChild (reverse childrenRenderOrder) $ Vec2 x y
            -- update last moused child
            lastMousedChild <- readTVar lastMousedChildVar
            when (mousedChild /= lastMousedChild) $ do
              writeTVar lastMousedChildVar mousedChild
              case lastMousedChild of
                Just PanelChild
                  { panelChildElement = SomeElement lastMousedChildElement
                  } -> void $ processInputEvent lastMousedChildElement MouseLeaveEvent inputState
                Nothing -> return ()
            return mousedChild
          -- if mouse points to some element now
          case mousedChild of
            Just PanelChild
              { panelChildElement = SomeElement childElement
              , panelChildPositionVar = childPositionVar
              } -> do
              -- correct coordinates and send event
              size <- readTVar childPositionVar
              let Vec2 px py = size
              processInputEvent childElement (MouseInputEvent (CursorMoveEvent (x - px) (y - py))) inputState
            Nothing -> return False
    MouseLeaveEvent -> do
      lastMousedChild <- readTVar lastMousedChildVar
      case lastMousedChild of
        Just PanelChild
          { panelChildElement = SomeElement element
          } -> processInputEvent element MouseLeaveEvent inputState
        Nothing -> return False

  focusElement panel@Panel
    { panelChildrenVar = childrenVar
    , panelFocusedChildVar = focusedChildVar
    , panelLastFocusedChildVar = lastFocusedChildVar
    } = do
    focusedChild <- readTVar focusedChildVar
    if isNothing focusedChild then do
      children <- readTVar childrenVar
      maybeLastFocusedChild <- readTVar lastFocusedChildVar
      focusSomeChild panel $ case maybeLastFocusedChild of
        Just lastFocusedChild -> let
          (childrenBefore, childrenAfter) = S.split lastFocusedChild children
          in lastFocusedChild : S.toAscList childrenAfter ++ S.toAscList childrenBefore
        Nothing -> S.toAscList children
    else return True

  unfocusElement Panel
    { panelFocusedChildVar = focusedChildVar
    , panelCommitHandlerVar = commitHandlerVar
    } = do
    focusedChild <- readTVar focusedChildVar
    case focusedChild of
      Just PanelChild
        { panelChildElement = SomeElement element
        } -> do
        unfocusElement element
        writeTVar focusedChildVar Nothing
      Nothing -> return ()
    commitHandler <- readTVar commitHandlerVar
    void $ commitHandler CommitLostFocus

instance FreeContainer Panel where

  type FreeContainerChild Panel = PanelChild

  setLayoutHandler Panel
    { panelLayoutHandlerVar = layoutHandlerVar
    , panelSizeVar = sizeVar
    } layoutHandler = do
    writeTVar layoutHandlerVar layoutHandler
    layoutHandler =<< readTVar sizeVar

  addFreeChild Panel
    { panelChildrenVar = childrenVar
    , panelChildIndexVar = childIndexVar
    , panelChildrenRenderOrderVar = childrenRenderOrderVar
    } element = do
    -- get index for new child
    childIndex <- readTVar childIndexVar
    writeTVar childIndexVar $ childIndex + 1
    -- create child
    positionVar <- newTVar $ Vec2 0 0
    let
      child = PanelChild
        { panelChildIndex = childIndex
        , panelChildElement = SomeElement element
        , panelChildPositionVar = positionVar
        }
    -- add it
    children <- readTVar childrenVar
    writeTVar childrenVar $ S.insert child children
    childrenRenderOrder <- readTVar childrenRenderOrderVar
    writeTVar childrenRenderOrderVar $ child : childrenRenderOrder
    -- return
    return child

  removeFreeChild panel@Panel
    { panelChildrenVar = childrenVar
    , panelChildrenRenderOrderVar = childrenRenderOrderVar
    , panelFocusedChildVar = focusedChildVar
    , panelLastFocusedChildVar = lastFocusedChildVar
    } child@PanelChild
    { panelChildElement = SomeElement element
    } = do
    children <- readTVar childrenVar
    -- remove from children
    let newChildren = S.delete child children
    -- removal must happen before calling `unfocusElement` to be reentrant
    -- because element may call `removeFreeChild` again
    writeTVar childrenVar newChildren
    -- remove from render order
    modifyTVar' childrenRenderOrderVar $ delete child
    -- if this element is focused
    focusedChild <- readTVar focusedChildVar
    when (focusedChild == Just child) $ do
      -- unfocus it
      writeTVar focusedChildVar Nothing -- before `unfocusElement` for reentrancy
      unfocusElement element
      -- try to focus some other child, starting from next one
      let (childrenBefore, childrenAfter) = S.split child newChildren
      _ <- focusSomeChild panel $ S.toAscList childrenAfter ++ S.toAscList childrenBefore
      return ()
    -- if this element was last-focused, forget it
    lastFocusedChild <- readTVar lastFocusedChildVar
    when (lastFocusedChild == Just child) $ writeTVar lastFocusedChildVar Nothing

  placeFreeChild _panel PanelChild
    { panelChildPositionVar = childPositionVar
    } = writeTVar childPositionVar

  placeFreeChildRelatively _panel PanelChild
    { panelChildPositionVar = childPositionVar
    } positionChange = modifyTVar' childPositionVar (+positionChange)

  bringFreeChildOnTop Panel
    { panelChildrenRenderOrderVar = childrenRenderOrderVar
    } child = modifyTVar' childrenRenderOrderVar $ (++ [child]) . delete child

  focusFreeChild Panel
    { panelFocusedChildVar = focusedChildVar
    , panelLastFocusedChildVar = lastFocusedChildVar
    } child@PanelChild
    { panelChildIndex = childIndex
    , panelChildElement = SomeElement element
    } = do
    focusedChild <- readTVar focusedChildVar
    case focusedChild of
      Just PanelChild
        { panelChildIndex = focusedChildIndex
        , panelChildElement = SomeElement focusedElement
        } -> when (childIndex /= focusedChildIndex) $ do
        focusAccepted <- focusElement element
        when focusAccepted $ do
          writeTVar focusedChildVar $ Just child
          writeTVar lastFocusedChildVar $ Just child
          unfocusElement focusedElement
      Nothing -> do
        focusAccepted <- focusElement element
        when focusAccepted $ do
          writeTVar focusedChildVar $ Just child
          writeTVar lastFocusedChildVar $ Just child

-- | Helper function, trying to focus first child in a list accepting the focus.
-- Writes index of a child accepted focus to panel.
focusSomeChild :: Panel -> [PanelChild] -> STM Bool
focusSomeChild Panel
  { panelFocusedChildVar = focusedChildVar
  , panelLastFocusedChildVar = lastFocusedChildVar
  , panelStickyFocus = stickyFocus
  } = tryToFocus where
  tryToFocus (child@PanelChild
    { panelChildElement = SomeElement element
    } : restChildren) = do
    focusAccepted <- focusElement element
    if focusAccepted then do
      writeTVar focusedChildVar $ Just child
      writeTVar lastFocusedChildVar $ Just child
      return True
    else tryToFocus restChildren
  tryToFocus [] = return stickyFocus

instance DefaultActionRedirector Panel where
  setDefaultElement Panel
    { panelDefaultElementVar = defaultElementVar
    } element = writeTVar defaultElementVar $ Just $ SomeElement element
  setCancelElement Panel
    { panelCancelElementVar = cancelElementVar
    } element = writeTVar cancelElementVar $ Just $ SomeElement element

instance HasCommitHandler Panel where
  setCommitHandler = writeTVar . panelCommitHandlerVar
