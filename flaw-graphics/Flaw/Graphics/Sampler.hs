{-|
Module: Flaw.Graphics.Sampler
Description: Abstract sampler things.
License: MIT
-}

module Flaw.Graphics.Sampler
	( SamplerWrap(..)
	, SamplerFilter(..)
	, SamplerStateInfo(..)
	) where

import Data.Default

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
	, samplerMinLod :: {-# UNPACK #-} !Float
	, samplerMaxLod :: {-# UNPACK #-} !Float
	, samplerBorderColor :: {-# UNPACK #-} !Float4
	, samplerMaxAnisotropy :: {-# UNPACK #-} !Int
	} deriving Show

instance Default SamplerStateInfo where
	def = SamplerStateInfo
		{ samplerMinFilter = SamplerPointFilter
		, samplerMipFilter = SamplerPointFilter
		, samplerMagFilter = SamplerPointFilter
		, samplerWrapU = SamplerWrapRepeat
		, samplerWrapV = SamplerWrapRepeat
		, samplerWrapW = SamplerWrapRepeat
		, samplerMinLod = -1000
		, samplerMaxLod = 1000
		, samplerBorderColor = Vec4 0 0 0 0
		, samplerMaxAnisotropy = 1
		}
