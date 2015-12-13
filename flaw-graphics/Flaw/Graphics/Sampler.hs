{-|
Module: Flaw.Graphics.Sampler
Description: Abstract sampler things.
License: MIT
-}

module Flaw.Graphics.Sampler
	( SamplerWrap(..)
	, SamplerFilter(..)
	, SamplerStateInfo(..)
	, defaultSamplerStateInfo
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
data SamplerStateInfo = SamplerStateInfo
	{ samplerMinFilter :: !SamplerFilter
	, samplerMipFilter :: !SamplerFilter
	, samplerMagFilter :: !SamplerFilter
	, samplerWrapU :: !SamplerWrap
	, samplerWrapV :: !SamplerWrap
	, samplerWrapW :: !SamplerWrap
	, samplerMinLod :: !Float
	, samplerMaxLod :: !Float
	, samplerBorderColor :: !Float4
	} deriving Show

defaultSamplerStateInfo :: SamplerStateInfo
defaultSamplerStateInfo = SamplerStateInfo
	{ samplerMinFilter = SamplerPointFilter
	, samplerMipFilter = SamplerPointFilter
	, samplerMagFilter = SamplerPointFilter
	, samplerWrapU = SamplerWrapRepeat
	, samplerWrapV = SamplerWrapRepeat
	, samplerWrapW = SamplerWrapRepeat
	, samplerMinLod = -1000
	, samplerMaxLod = 1000
	, samplerBorderColor = Vec4 0 0 0 0
	}
