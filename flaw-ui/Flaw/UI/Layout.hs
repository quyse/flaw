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
	, labeledFlowLayout
	, checkBoxedFlowLayout
	, elementInFlowLayout
	) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import qualified Data.Text as T

import Flaw.UI
import Flaw.UI.Elements
import Flaw.Math
import Flaw.UI.Metrics

data FlowLayoutState = FlowLayoutState
	{ flsMetrics :: !Metrics
	, flsParentElement :: !SomeFreeContainer
	, flsLayoutHandler :: !(Rect -> STM Rect)
	, flsPreSize :: !Size
	}

type FlowLayoutM = StateT FlowLayoutState STM

panelFlowLayout :: Metrics -> FlowLayoutM () -> STM Panel
panelFlowLayout metrics flowLayout = do
	panel <- newPanel False
	FlowLayoutState
		{ flsLayoutHandler = layoutHandler
		} <- execStateT flowLayout FlowLayoutState
		{ flsMetrics = metrics
		, flsParentElement = SomeFreeContainer panel
		, flsLayoutHandler = return
		, flsPreSize = Vec2 0 0
		}
	setLayoutHandler panel $ \(Vec2 sx sy) -> void $ layoutHandler $ Vec4 0 0 sx sy
	return panel

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
		, flsParentElement = SomeFreeContainer panel
		, flsLayoutHandler = return
		, flsPreSize = Vec2 0 0
		}
	setLayoutHandler panel $ \(Vec2 sx sy) ->
		void $ layoutHandler (Vec4 bigGap bigGap (sx - bigGap) (sy - bigGap))
	frame <- newFrame panel metrics
	layoutElement frame $ xy__ frameClient + zw__ frameClient + preSize + Vec2 (bigGap * 2) (bigGap * 2 - gap)
	return frame

-- | Label a sublayout.
-- Sublayout will be placed to the right of the label.
labeledFlowLayout :: T.Text -> FlowLayoutM a -> FlowLayoutM a
labeledFlowLayout text subLayout = do
	-- get state
	s@FlowLayoutState
		{ flsMetrics = Metrics
			{ metricsGap = gap
			, metricsLabelSize = labelSize@(Vec2 labelWidth labelHeight)
			}
		, flsParentElement = SomeFreeContainer parentElement
		, flsLayoutHandler = lh
		, flsPreSize = Vec2 psx psy
		} <- get
	-- create label
	label <- lift newTextLabel
	lift $ setText label text
	labelVE <- lift $ newVisualElement label
	labelVEChild <- lift $ addFreeChild parentElement labelVE
	-- run sub layout
	(r, FlowLayoutState
		{ flsLayoutHandler = subLayoutHandler
		, flsPreSize = Vec2 spsx spsy
		}) <- lift $ runStateT subLayout s
		{ flsLayoutHandler = return
		, flsPreSize = Vec2 0 0
		}
	put s
		{ flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
			placeFreeChild parentElement labelVEChild (Vec2 px py)
			layoutElement labelVE labelSize
			Vec4 _subpx subpy _subqx subqy <- subLayoutHandler $ Vec4 (px + labelWidth + gap) py qx qy
			return $ Vec4 px (max (py + labelHeight + gap) subpy) qx (min qy subqy)
		, flsPreSize = Vec2 (max psx (labelWidth + gap + spsx)) (psy + max (labelHeight + gap) spsy)
		}
	return r

-- | Label a sublayout with checkbox.
-- Sublayout will be placed to the right of the checkbox.
checkBoxedFlowLayout :: T.Text -> (CheckBox -> FlowLayoutM a) -> FlowLayoutM a
checkBoxedFlowLayout text subLayout = do
	-- get state
	s@FlowLayoutState
		{ flsMetrics = Metrics
			{ metricsGap = gap
			, metricsLabelSize = labelSize@(Vec2 labelWidth labelHeight)
			}
		, flsParentElement = SomeFreeContainer parentElement
		, flsLayoutHandler = lh
		, flsPreSize = Vec2 psx psy
		} <- get
	-- create label
	checkBox <- lift $ newLabeledCheckBox text
	checkBoxChild <- lift $ addFreeChild parentElement checkBox
	-- run sub layout
	(r, FlowLayoutState
		{ flsLayoutHandler = subLayoutHandler
		, flsPreSize = Vec2 spsx spsy
		}) <- lift $ runStateT (subLayout checkBox) s
		{ flsLayoutHandler = return
		, flsPreSize = Vec2 0 0
		}
	put s
		{ flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
			placeFreeChild parentElement checkBoxChild (Vec2 px py)
			layoutElement checkBox labelSize
			Vec4 _subpx subpy _subqx subqy <- subLayoutHandler $ Vec4 (px + labelWidth + gap) py qx qy
			return $ Vec4 px (max (py + labelHeight + gap) subpy) qx (min qy subqy)
		, flsPreSize = Vec2 (max psx (labelWidth + gap + spsx)) (psy + max (labelHeight + gap) spsy)
		}
	return r

-- | Place element in layout.
-- Adds gap after element.
elementInFlowLayout :: (Element e, HasPreferredSize e) => e -> FlowLayoutM ()
elementInFlowLayout element = do
	-- get state
	s@FlowLayoutState
		{ flsMetrics = metrics@Metrics
			{ metricsGap = gap
			}
		, flsParentElement = SomeFreeContainer parentElement
		, flsLayoutHandler = lh
		, flsPreSize = Vec2 psx psy
		} <- get
	-- get preferred size
	let Vec2 epsx epsy = preferredSize metrics element
	-- add to container
	elementChild <- lift $ addFreeChild parentElement element
	put s
		{ flsLayoutHandler = lh >=> \(Vec4 px py qx qy) -> do
			placeFreeChild parentElement elementChild (Vec2 px py)
			layoutElement element $ Vec2 (qx - px) epsy
			return $ Vec4 px (py + epsy + gap) qx qy
		, flsPreSize = Vec2 (max psx epsx) (psy + epsy + gap)
		}
