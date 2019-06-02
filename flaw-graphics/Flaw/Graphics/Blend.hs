{-|
Module: Flaw.Graphics.Blend
Description: Abstract blend things.
License: MIT
-}

module Flaw.Graphics.Blend
  ( ColorSource(..)
  , AlphaSource(..)
  , BlendOperation(..)
  , BlendStateInfo(..)
  ) where

import Data.Default

-- | Color source for blending.
data ColorSource
  = ColorSourceZero
  | ColorSourceOne
  | ColorSourceSrc
  | ColorSourceInvSrc
  | ColorSourceSrcAlpha
  | ColorSourceInvSrcAlpha
  | ColorSourceDest
  | ColorSourceInvDest
  | ColorSourceDestAlpha
  | ColorSourceInvDestAlpha
  | ColorSourceSecondSrc
  | ColorSourceInvSecondSrc
  | ColorSourceSecondSrcAlpha
  | ColorSourceInvSecondSrcAlpha
  deriving (Eq, Show)

-- | Alpha source for blending.
data AlphaSource
  = AlphaSourceZero
  | AlphaSourceOne
  | AlphaSourceSrc
  | AlphaSourceInvSrc
  | AlphaSourceDest
  | AlphaSourceInvDest
  | AlphaSourceSecondSrc
  | AlphaSourceInvSecondSrc
  deriving (Eq, Show)

-- | Blend operation.
data BlendOperation
  = BlendOperationAdd
  | BlendOperationSubtractAB
  | BlendOperationSubtractBA
  | BlendOperationMin
  | BlendOperationMax
  deriving (Eq, Show)

data BlendStateInfo = BlendStateInfo
  { blendSourceColor :: ColorSource
  , blendDestColor :: ColorSource
  , blendColorOperation :: BlendOperation
  , blendSourceAlpha :: AlphaSource
  , blendDestAlpha :: AlphaSource
  , blendAlphaOperation :: BlendOperation
  } deriving (Eq, Show)

instance Default BlendStateInfo where
  def = BlendStateInfo
    { blendSourceColor = ColorSourceOne
    , blendDestColor = ColorSourceZero
    , blendColorOperation = BlendOperationAdd
    , blendSourceAlpha = AlphaSourceOne
    , blendDestAlpha = AlphaSourceZero
    , blendAlphaOperation = BlendOperationAdd
    }
