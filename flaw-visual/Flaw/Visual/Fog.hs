{-|
Module: Flaw.Visual.Fog
Description: Fog shader functions.
License: MIT
-}

module Flaw.Visual.Fog
	( exponentialHeightFog
	, exponentialHeightFogParams
	) where

import Flaw.Graphics.Program
import Flaw.Math

-- | Height-distance based fog with exponential fall-off
-- http://developer.amd.com/wordpress/media/2012/10/Wenzel-Real-time_Atmospheric_Effects_in_Games.pdf
exponentialHeightFog :: Node Float3 -> Node Float3 -> Node Float2 -> Program (Node Float)
exponentialHeightFog eyePosition pointPosition fogParams = do
	toPoint <- temp $ pointPosition - eyePosition
	toPointDirection <- temp $ normalize toPoint
	k1 <- temp $ x_ fogParams
	k2 <- temp $ y_ fogParams
	temp $ exp $ (exp (k1 * z_ eyePosition + k2) - exp (k1 * z_ pointPosition + k2)) / (z_ toPointDirection * k1)

{-# INLINABLE exponentialHeightFogParams #-}
exponentialHeightFogParams :: Float -> Float -> Float -> Float -> Float -> Vec2 Float
exponentialHeightFogParams visibilityLow visibilityHigh heightLow heightHigh distance = Vec2 k1 k2 where
	logLow = log (log visibilityLow / negate distance)
	logHigh = log (log visibilityHigh / negate distance)
	k1 = (logLow - logHigh) / (heightLow - heightHigh)
	k2 = (heightLow * logHigh - logLow * heightHigh) / (heightLow - heightHigh)
