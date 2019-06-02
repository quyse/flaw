{-|
Module: Flaw.UI.Layout
Description: Helper functions for placing UI elements in free containers.
License: MIT
-}

module Flaw.UI.Layout
  ( FlowLayoutState(..)
  , FlowLayoutM
  , panelFlowLayout
  , frameFlowLayout
  , titleInFlowLayout
  , labeledFlowLayout
  , checkBoxedFlowLayout
  , elementInFlowLayout
  , elementWithSizeInFlowLayout
  , okCancelButtonsInFlowLayout
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import qualified Data.Text as T

import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.CheckBox
import Flaw.UI.Frame
import Flaw.UI.Label
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.VisualElement

data FlowLayoutState = FlowLayoutState
  { flsMetrics :: !Metrics
  , flsParentElement :: !Panel
  , flsLayoutHandler :: !(Rect -> STM Rect)
  , flsPreSize :: !Size
  }

type FlowLayoutM = StateT FlowLayoutState STM

{-# INLINEABLE panelFlowLayout #-}
panelFlowLayout :: Metrics -> FlowLayoutM () -> STM Panel
panelFlowLayout metrics flowLayout = do
  panel <- newPanel False
  FlowLayoutState
    { flsLayoutHandler = layoutHandler
    } <- execStateT flowLayout FlowLayoutState
    { flsMetrics = metrics
    , flsParentElement = panel
    , flsLayoutHandler = return
    , flsPreSize = Vec2 0 0
    }
  setLayoutHandler panel $ \(Vec2 sx sy) -> void $ layoutHandler $ Vec4 0 0 sx sy
  return panel

{-# INLINEABLE frameFlowLayout #-}
frameFlowLayout :: Metrics -> FlowLayoutM () -> STM Frame
frameFlowLayout metrics@Metrics
  { metricsGap = gap
  , metricsBigGap = bigGap
  , metricsFrameClient = frameClient
  } flowLayout = do
  panel <- newPanel True
  FlowLayoutState
    { flsLayoutHandler = layoutHandler
    , flsPreSize = preSize
    } <- execStateT flowLayout FlowLayoutState
    { flsMetrics = metrics
    , flsParentElement = panel
    , flsLayoutHandler = return
    , flsPreSize = Vec2 0 0
    }
  setLayoutHandler panel $ \(Vec2 sx sy) ->
    void $ layoutHandler (Vec4 bigGap bigGap (sx - bigGap) (sy - bigGap))
  frame <- newFrame panel metrics
  layoutElement frame $ xy__ frameClient + zw__ frameClient + preSize + Vec2 (bigGap * 2) (bigGap * 2 - gap)
  return frame

-- | Title in a layout.
titleInFlowLayout :: T.Text -> FlowLayoutM ()
titleInFlowLayout text = do
  labelVE <- lift $ do
    label <- newTitleLabel
    setText label text
    newVisualElement label
  FlowLayoutState
    { flsMetrics = Metrics
      { metricsLabelSize = labelSize
      , metricsTitleHeight = titleHeight
      }
    } <- get
  let Vec2 labelWidth _labelHeight = labelSize
  elementWithSizeInFlowLayout labelVE $ Vec2 labelWidth titleHeight

-- | Label a sublayout.
-- Sublayout will be placed to the right of the label.
{-# INLINEABLE labeledFlowLayout #-}
labeledFlowLayout :: T.Text -> FlowLayoutM a -> FlowLayoutM a
labeledFlowLayout text subLayout = do
  -- get state
  s@FlowLayoutState
    { flsMetrics = Metrics
      { metricsGap = gap
      , metricsLabelSize = labelSize
      }
    , flsParentElement = parentElement
    , flsLayoutHandler = lh
    , flsPreSize = ps
    } <- get
  let
    Vec2 labelWidth labelHeight = labelSize
    Vec2 psx psy = ps
  -- create label
  label <- lift newTextLabel
  lift $ setText label text
  labelVE <- lift $ newVisualElement label
  labelVEChild <- lift $ addFreeChild parentElement labelVE
  -- run sub layout
  (r, FlowLayoutState
    { flsLayoutHandler = subLayoutHandler
    , flsPreSize = sps
    }) <- lift $ runStateT subLayout s
    { flsLayoutHandler = return
    , flsPreSize = Vec2 0 0
    }
  let Vec2 spsx spsy = sps
  put s
    { flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
      placeFreeChild parentElement labelVEChild (Vec2 px py)
      layoutElement labelVE labelSize
      sub <- subLayoutHandler $ Vec4 (px + labelWidth + gap) py qx qy
      let Vec4 _subpx subpy _subqx subqy = sub
      return $ Vec4 px (max (py + labelHeight + gap) subpy) qx (min qy subqy)
    , flsPreSize = Vec2 (max psx (labelWidth + gap + spsx)) (psy + max (labelHeight + gap) spsy)
    }
  return r

-- | Label a sublayout with checkbox.
-- Sublayout will be placed to the right of the checkbox.
{-# INLINEABLE checkBoxedFlowLayout #-}
checkBoxedFlowLayout :: T.Text -> (CheckBox -> FlowLayoutM a) -> FlowLayoutM a
checkBoxedFlowLayout text subLayout = do
  -- get state
  s@FlowLayoutState
    { flsMetrics = Metrics
      { metricsGap = gap
      , metricsLabelSize = labelSize
      }
    , flsParentElement = parentElement
    , flsLayoutHandler = lh
    , flsPreSize = ps
    } <- get
  let
    Vec2 labelWidth labelHeight = labelSize
    Vec2 psx psy = ps
  -- create label
  checkBox <- lift $ newLabeledCheckBox text
  checkBoxChild <- lift $ addFreeChild parentElement checkBox
  -- run sub layout
  (r, FlowLayoutState
    { flsLayoutHandler = subLayoutHandler
    , flsPreSize = sps
    }) <- lift $ runStateT (subLayout checkBox) s
    { flsLayoutHandler = return
    , flsPreSize = Vec2 0 0
    }
  let Vec2 spsx spsy = sps
  put s
    { flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
      placeFreeChild parentElement checkBoxChild (Vec2 px py)
      layoutElement checkBox labelSize
      sub <- subLayoutHandler $ Vec4 (px + labelWidth + gap) py qx qy
      let Vec4 _subpx subpy _subqx subqy = sub
      return $ Vec4 px (max (py + labelHeight + gap) subpy) qx (min qy subqy)
    , flsPreSize = Vec2 (max psx (labelWidth + gap + spsx)) (psy + max (labelHeight + gap) spsy)
    }
  return r

-- | Place element in layout.
-- Adds gap after element.
{-# INLINEABLE elementInFlowLayout #-}
elementInFlowLayout :: (Element e, HasPreferredSize e) => e -> FlowLayoutM ()
elementInFlowLayout element = do
  FlowLayoutState
    { flsMetrics = metrics
    } <- get
  elementWithSizeInFlowLayout element (preferredSize metrics element)

-- | Place explicitly sized element in layout.
-- Adds gap after element.
{-# INLINEABLE elementWithSizeInFlowLayout #-}
elementWithSizeInFlowLayout :: Element e => e -> Size -> FlowLayoutM ()
elementWithSizeInFlowLayout element (Vec2 epsx epsy) = do
  -- get state
  s@FlowLayoutState
    { flsMetrics = Metrics
      { metricsGap = gap
      }
    , flsParentElement = parentElement
    , flsLayoutHandler = lh
    , flsPreSize = ps
    } <- get
  let Vec2 psx psy = ps
  -- add to container
  elementChild <- lift $ addFreeChild parentElement element
  put s
    { flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
      placeFreeChild parentElement elementChild (Vec2 px py)
      layoutElement element $ Vec2 (qx - px) epsy
      return $ Vec4 px (py + epsy + gap) qx qy
    , flsPreSize = Vec2 (max psx epsx) (psy + epsy + gap)
    }

-- | Right-aligned pair of buttons.
okCancelButtonsInFlowLayout :: Button -> Button -> FlowLayoutM ()
okCancelButtonsInFlowLayout okButton cancelButton = do
  s@FlowLayoutState
    { flsMetrics = Metrics
      { metricsGap = gap
      , metricsBigGap = bigGap
      , metricsButtonSize = buttonSize
      }
    , flsParentElement = parentElement
    , flsLayoutHandler = layoutHandler
    , flsPreSize = preSize
    } <- get
  let
    Vec2 buttonWidth buttonHeight = buttonSize
    Vec2 psx psy = preSize
  okButtonChild <- lift $ addFreeChild parentElement okButton
  cancelButtonChild <- lift $ addFreeChild parentElement cancelButton
  lift $ do
    layoutElement okButton buttonSize
    layoutElement cancelButton buttonSize
    setDefaultElement parentElement okButton
    setCancelElement parentElement cancelButton
    setButtonDefault okButton
    setButtonCancel cancelButton
  put s
    { flsLayoutHandler = layoutHandler >=> \(Vec4 px py qx qy) -> do
      placeFreeChild parentElement okButtonChild $ Vec2 (qx - buttonWidth * 2 - gap) (py + bigGap - gap)
      placeFreeChild parentElement cancelButtonChild $ Vec2 (qx - buttonWidth) (py + bigGap - gap)
      return $ Vec4 px (py + buttonHeight + gap) qx qy
    , flsPreSize = Vec2 (max psx $ buttonWidth * 2 + gap) (psy + buttonHeight + bigGap)
    }
