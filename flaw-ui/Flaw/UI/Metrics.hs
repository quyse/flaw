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
  , HasPreferredSize(..)
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
    metricsGap :: {-# UNPACK #-} !Metric
  -- | Big gap between elements.
  , metricsBigGap :: {-# UNPACK #-} !Metric
  -- | Frame client rect.
  , metricsFrameClient :: {-# UNPACK #-} !Rect
  -- | Frame top border height.
  , metricsFrameTopBorder :: {-# UNPACK #-} !Metric
  -- | Typical width of "main" control in a dialog.
  , metricsMainWidth :: {-# UNPACK #-} !Metric
  -- | Size of button.
  , metricsButtonSize :: {-# UNPACK #-} !Size
  -- | Height of edit box.
  , metricsEditBoxHeight :: {-# UNPACK #-} !Metric
  -- | Size of label.
  , metricsLabelSize :: {-# UNPACK #-} !Size
  -- | Height of title.
  , metricsTitleHeight :: {-# UNPACK #-} !Metric
  -- | Width of the slider piece.
  , metricsSliderPieceWidth :: {-# UNPACK #-} !Metric
  -- | Height of the slider.
  , metricsSliderHeight :: {-# UNPACK #-} !Metric
  -- | Width of resizing grip in pile box.
  , metricsPileBoxGripWidth :: {-# UNPACK #-} !Metric
  -- | Height of listbox column header.
  , metricsListBoxColumnHeaderHeight :: {-# UNPACK #-} !Metric
  -- | Height of listbox item.
  , metricsListBoxItemHeight :: {-# UNPACK #-} !Metric
  -- | Width of scroll bar.
  , metricsScrollBarWidth :: {-# UNPACK #-} !Metric
  }

-- | Class of something which has preferred size in metrics.
class HasPreferredSize a where
  preferredSize :: Metrics -> a -> Size
