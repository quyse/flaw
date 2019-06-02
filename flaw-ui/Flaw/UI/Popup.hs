{-|
Module: Flaw.UI.Popup
Description: Popup.
License: MIT
-}

module Flaw.UI.Popup
  ( Popup(..)
  , PopupService(..)
  , newPopupService
  , newPopup
  ) where

import Control.Concurrent.STM

import Flaw.Math
import Flaw.UI
import Flaw.UI.Metrics
import Flaw.UI.Panel

data Popup = Popup
  { popupPanel :: !Panel
  , popupClose :: !(STM ())
  }

data PopupService = PopupService
  { popupServiceMetrics :: !Metrics
  , popupServiceContainer :: !SomeFreeContainer
  }

newPopupService :: FreeContainer fc => Metrics -> fc -> STM PopupService
newPopupService metrics container = return PopupService
  { popupServiceMetrics = metrics
  , popupServiceContainer = SomeFreeContainer container
  }

newPopup :: PopupService -> Rect -> STM Popup
newPopup PopupService
  { popupServiceContainer = SomeFreeContainer container
  } (Vec4 left top right bottom) = do
  panel <- newPanel True
  panelChild <- addFreeChild container panel
  let
    close = removeFreeChild container panelChild
    closeByCommit = do
      close
      return True
  setCommitHandler panel $ \commitReason -> let
    in case commitReason of
      CommitAccept -> return False
      CommitCancel -> closeByCommit
      CommitLostFocus -> closeByCommit
  placeFreeChild container panelChild $ Vec2 left top
  bringFreeChildOnTop container panelChild
  focusFreeChild container panelChild
  layoutElement panel $ Vec2 (right - left) (bottom - top)
  return Popup
    { popupPanel = panel
    , popupClose = close
    }
