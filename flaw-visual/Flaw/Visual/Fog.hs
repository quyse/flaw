{-|
Module: Flaw.Visual.Fog
Description: Fog shader functions.
License: MIT
-}

module Flaw.Visual.Fog
	( exponentialHeightFog
	, exponentialHeightSkyFog
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
	let k1 = x_ fogParams
	let k2 = y_ fogParams
	let generalCase = exp $ (exp (k1 * z_ eyePosition + k2) - exp (k1 * z_ pointPosition + k2)) / (z_ toPointDirection * k1)
	let horizontalCase = exp $ negate (norm toPoint) * exp (k1 * z_ pointPosition + k2)
	temp $ if_ (abs (z_ toPointDirection) `less_` constf 0.01) horizontalCase generalCase

-- | The same as `exponentialHeightFog`, but for infinitely far point.
exponentialHeightSkyFog :: Node Float3 -> Node Float3 -> Node Float2 -> Program (Node Float)
exponentialHeightSkyFog eyePosition toPointDirection fogParams = do
	let k1 = x_ fogParams
	let k2 = y_ fogParams
	temp $ if_ (z_ toPointDirection `less_` constf 0.01) (constf 0) $ exp $ exp (k1 * z_ eyePosition + k2) / (z_ toPointDirection * k1)

{-# INLINABLE exponentialHeightFogParams #-}
exponentialHeightFogParams :: Float -> Float -> Float -> Float -> Float -> Vec2 Float
exponentialHeightFogParams visibilityLow visibilityHigh heightLow heightHigh distance = Vec2 k1 k2 where
	logLow = log (log visibilityLow / negate distance)
	logHigh = log (log visibilityHigh / negate distance)
	k1 = (logLow - logHigh) / (heightLow - heightHigh)
	k2 = (heightLow * logHigh - logLow * heightHigh) / (heightLow - heightHigh)
