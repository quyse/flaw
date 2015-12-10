{-|
Module: Flaw.UI.Metrics
Description: Style and look-n-feel information.
License: MIT
-}

module Flaw.UI.Metrics
	( Metric
	, Position
	, Size
	, Rect
	, Metrics(..)
	) where

import Flaw.Math

-- | Base type for length values.
type Metric = Int
-- | Base type for two-dimensional position values.
type Position = Vec2 Metric
-- | Base type for two-dimensional length values.
type Size = Vec2 Metric
-- | Base type for rectangle (left, top, right, bottom).
type Rect = Vec4 Metric

-- | Set of "standard" metrics for UI.
data Metrics = Metrics
	{
	-- | Normal gap between elements.
	  metricsGap :: !Metric
	-- | Big gap between elements.
	, metricsBigGap :: !Metric
	-- | Frame client rect.
	, metricsFrameClient :: !Rect
	-- | Frame top border height.
	, metricsFrameTopBorder :: !Metric
	-- | Size of button.
	, metricsButtonSize :: !Size
	-- | Height of edit box.
	, metricsEditBoxHeight :: !Metric
	-- | Height of label.
	, metricsLabelHeight :: !Metric
	}
