{-|
Module: Flaw.Graphics.Sampler
Description: Abstract sampler things.
License: MIT
-}

module Flaw.Graphics.Sampler
	( SamplerWrap(..)
	, SamplerFilter(..)
	, SamplerInfo(..)
	, defaultSamplerInfo
	) where

import Flaw.Math

-- | Wrapping mode.
data SamplerWrap
	= SamplerWrapRepeat
	| SamplerWrapRepeatMirror
	| SamplerWrapClamp
	| SamplerWrapBorder
	deriving Show

-- | Filtering mode.
data SamplerFilter
	= SamplerPointFilter
	| SamplerLinearFilter
	deriving Show

-- | Sampler settings.
data SamplerInfo = SamplerInfo
	{ samplerMinFilter :: SamplerFilter
	, samplerMipFilter :: SamplerFilter
	, samplerMagFilter :: SamplerFilter
	, samplerMipMapping :: Bool
	, samplerWrapU :: SamplerWrap
	, samplerWrapV :: SamplerWrap
	, samplerWrapW :: SamplerWrap
	, samplerMinLOD :: Float
	, samplerMaxLOD :: Float
	, samplerBorderColor :: Vec4f
	} deriving Show

defaultSamplerInfo :: SamplerInfo
defaultSamplerInfo = SamplerInfo
	{ samplerMinFilter = SamplerPointFilter
	, samplerMipFilter = SamplerPointFilter
	, samplerMagFilter = SamplerPointFilter
	, samplerMipMapping = False
	, samplerWrapU = SamplerWrapRepeat
	, samplerWrapV = SamplerWrapRepeat
	, samplerWrapW = SamplerWrapRepeat
	, samplerMinLOD = -1000
	, samplerMaxLOD = 1000
	, samplerBorderColor = Vec4 0 0 0 0
	}
