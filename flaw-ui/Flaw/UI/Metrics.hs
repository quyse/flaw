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
	, defaultMetrics
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

defaultMetrics :: Metrics
defaultMetrics = Metrics
	{ metricsGap = 7
	, metricsBigGap = 10
	, metricsFrameClient = Vec4 5 25 5 5
	, metricsFrameTopBorder = 5
	, metricsButtonSize = Vec2 80 24
	, metricsEditBoxHeight = 24
	, metricsLabelHeight = 20
	}
