{-|
Module: Flaw.Visual
Description: Shader stuff.
License: MIT
-}

module Flaw.Visual
	( applyQuat
	, applyQuatOffset
	, rgb2xyz
	, xyz2rgb
	, xyz2xyY
	, xyY2xyz
	, tangentFrame
	, encodeLambertAzimuthalEqualArea
	, decodeLambertAzimuthalEqualArea
	, lambertReflectance
	, schulerSpecularReflectance
	, schulerAmbientReflectance
	, gaussianWeights
	) where

import Flaw.Graphics.Program
import Flaw.Math

applyQuat :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuat q v = v + cross (xyz__ q) (cross (xyz__ q) v + v * www__ q) * cnst (Vec3 2 2 2)

applyQuatOffset :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuatOffset q o v = applyQuat q v + o

-- | Conversion from RGB (linear sRGB profile) to XYZ.
rgb2xyz :: (OfScalarType a, Vectorized a, Fractional a) => Node (Vec3 a) -> Program (Node (Vec3 a))
rgb2xyz rgb = temp $ cvec111
	(dot rgb $ cnst $ Vec3 0.4360747 0.3850649 0.1430804)
	(dot rgb $ cnst $ Vec3 0.2225045 0.7168786 0.0606169)
	(dot rgb $ cnst $ Vec3 0.0139322 0.0971045 0.7141733)

-- | Conversion from XYZ to RGB (linear sRGB profile).
xyz2rgb :: (OfScalarType a, Vectorized a, Fractional a) => Node (Vec3 a) -> Program (Node (Vec3 a))
xyz2rgb xyz = temp $ cvec111
	(dot xyz $ cnst $ Vec3   3.1338561  (-1.6168667) (-0.4906146))
	(dot xyz $ cnst $ Vec3 (-0.9787684)   1.9161415    0.0334540 )
	(dot xyz $ cnst $ Vec3   0.0719453  (-0.2289914)   1.4052427 )

-- | Conversion from XYZ to xyY.
xyz2xyY :: (OfScalarType a, Vectorized a, Fractional a) => Node (Vec3 a) -> Program (Node (Vec3 a))
xyz2xyY xyz = do
	s <- temp $ x_ xyz + y_ xyz + z_ xyz
	temp $ cvec111 (x_ xyz / s) (y_ xyz / s) (y_ xyz)

-- | Conversion from xyY to XYZ.
xyY2xyz :: (OfScalarType a, Vectorized a, Fractional a) => Node (Vec3 a) -> Program (Node (Vec3 a))
xyY2xyz xyY = temp $ cvec111
	(z_ xyY * x_ xyY / y_ xyY)
	(z_ xyY)
	(z_ xyY * (1 - x_ xyY - y_ xyY) / y_ xyY)

-- | Calculate tangent frame for bump mapping from position, normal and texcoord.
-- Position and normal must be in whatever space we need a frame, presumably world space.
tangentFrame :: Node Float3 -> Node Float3 -> Node Float2 -> Program (Node Float3, Node Float3, Node Float3)
tangentFrame position normal texcoord = do
	let dxPosition = ddx position
	let dyPosition = ddy position
	dxTexcoord <- temp $ ddx texcoord
	dyTexcoord <- temp $ ddy texcoord

	nn <- temp $ normalize normal
	r1 <- temp $ cross dyPosition nn
	r2 <- temp $ cross nn dxPosition

	t <- temp $ r1 * xxx__ dxTexcoord + r2 * xxx__ dyTexcoord
	b <- temp $ r1 * yyy__ dxTexcoord + r2 * yyy__ dyTexcoord

	s <- temp $ invSqrt $ max_ (dot t t) (dot b b)

	tt <- temp $ t * vecFromScalar s
	bb <- temp $ b * vecFromScalar s

	return (tt, bb, nn)

-- | Encode normalized 3-float into 2 floats using lambert azimuthal equal-area projection.
encodeLambertAzimuthalEqualArea :: Node Float3 -> Program (Node Float2)
encodeLambertAzimuthalEqualArea v = temp $ xy__ v / vecFromScalar (sqrt (z_ v * 8 + 8)) + vecFromScalar 0.5

-- | Decode normalized 3-float from 2 floats using lambert azimuthal equal-area projection.
decodeLambertAzimuthalEqualArea :: Node Float2 -> Program (Node Float3)
decodeLambertAzimuthalEqualArea v = do
	fenc <- temp $ v * vecFromScalar 4 - vecFromScalar 2
	f <- temp $ dot fenc fenc
	temp $ cvec21 (fenc * vecFromScalar (sqrt $ 1 - f * 0.25)) (1 - f * 0.5)

-- | Standard diffuse (Lambertian) reflectance.
-- Need to be multiplied by light irrandiance and material's diffuse color.
lambertReflectance
	:: Node Float3 -- ^ Normal.
	-> Node Float3 -- ^ Normalized direction to light.
	-> Program (Node Float)
lambertReflectance normal toLightDirection = temp $ dot normal toLightDirection

-- | Shading model from "An Efficient and Physically Plausible Real-Time Shading Model"
-- , ShaderX 7, by Christian Schüler.
-- As in original paper, result need to be multiplied by lambertian reflectance
-- (and then by light irrandiance and material's specular color).
schulerSpecularReflectance
	:: Node Float3 -- ^ Normal.
	-> Node Float3 -- ^ Normalized half-vector between directions to eye and to light.
	-> Node Float3 -- ^ Normalized direction to light.
	-> Node Float -- ^ Material glossiness, from 0 (rough) to 1 (smooth).
	-> Program (Node Float)
schulerSpecularReflectance normal toEyeLightHalfDirection toLightDirection glossiness = do
	-- e = pow 2 (12 * g) = exp (12 * g * log 2)
	e <- temp $ exp (glossiness * constf (12 * log 2))
	lh <- temp $ dot toLightDirection toEyeLightHalfDirection
	temp $ (1 + e) / (8 * (lh * lh * lh)) * exp (e * log (max_ 1e-8 $ dot normal toEyeLightHalfDirection))

-- | Shading model from "An Efficient and Physically Plausible Real-Time Shading Model"
-- , ShaderX 7, by Christian Schüler, ambient lighting.
schulerAmbientReflectance
	:: Node Float3 -- ^ Normal.
	-> Node Float3 -- ^ Normalized direction to eye.
	-> Node Float -- ^ Specular coef, from 0 to 1.
	-> Node Float -- ^ Material glossiness, from 0 (rough) to 1 (smooth).
	-> Program (Node Float)
schulerAmbientReflectance normal toEyeDirection specular glossiness = do
	t <- temp $ 1 - glossiness * max_ 0 (dot normal toEyeDirection)
	t2 <- temp $ t * t
	t4 <- temp $ t2 * t2
	temp $ lerp specular (min_ (specular * 60) 1) t4

-- | Calculate gaussian weights.
-- Accepts half-number of taps, so with n = 3 it will be 7 weights.
gaussianWeights :: Int -> [Float]
gaussianWeights tapsHalfCount = normalizedWeights where
	-- 3-sigma rule
	sigma = fromIntegral tapsHalfCount / 3
	a = 1 / (sqrt (2 * pi) * sigma)
	b = (-1) / (2 * sigma * sigma)
	gauss x = a * exp (b * x * x)
	weights = map (gauss . fromIntegral) [(-tapsHalfCount) .. tapsHalfCount]
	normalizedWeights = map (/ sum weights) weights
