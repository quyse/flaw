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
	, defaultBlendStateInfo
	) where

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
	deriving Show

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
	deriving Show

-- | Blend operation.
data BlendOperation
	= BlendOperationAdd
	| BlendOperationSubtractAB
	| BlendOperationSubtractBA
	| BlendOperationMin
	| BlendOperationMax
	deriving Show

data BlendStateInfo = BlendStateInfo
	{ blendSourceColor :: ColorSource
	, blendDestColor :: ColorSource
	, blendColorOperation :: BlendOperation
	, blendSourceAlpha :: AlphaSource
	, blendDestAlpha :: AlphaSource
	, blendAlphaOperation :: BlendOperation
	} deriving Show

defaultBlendStateInfo :: BlendStateInfo
defaultBlendStateInfo = BlendStateInfo
	{ blendSourceColor = ColorSourceOne
	, blendDestColor = ColorSourceZero
	, blendColorOperation = BlendOperationAdd
	, blendSourceAlpha = AlphaSourceOne
	, blendDestAlpha = AlphaSourceZero
	, blendAlphaOperation = BlendOperationAdd
	}
