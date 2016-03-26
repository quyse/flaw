{-|
Module: Flaw.Visual
Description: Shader stuff.
License: MIT
-}

module Flaw.Visual
	( applyQuat
	, applyQuatOffset
	, tangentFrame
	, lambertReflectance
	, schulerSpecularReflectance
	) where

import Flaw.Graphics.Program
import Flaw.Math

applyQuat :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuat q v = v + cross (xyz__ q) (cross (xyz__ q) v + v * www__ q) * cnst (Vec3 2 2 2)

applyQuatOffset :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuatOffset q o v = applyQuat q v + o

-- | Calculate tangent frame for bump mapping from position, normal and texcoord.
-- Position and normal must be in whatever space we need a frame, presumably world space.
tangentFrame :: Node Float3 -> Node Float3 -> Node Float2 -> Program (Node Float3, Node Float3, Node Float3)
tangentFrame position normal texcoord = do
	let dxPosition = ddx position
	let dyPosition = ddy position
	dxTexcoord <- temp $ ddx texcoord
	dyTexcoord <- temp $ ddy texcoord

	r1 <- temp $ cross dyPosition normal
	r2 <- temp $ cross normal dxPosition

	t <- temp $ r1 * xxx__ dxTexcoord + r2 * xxx__ dyTexcoord
	b <- temp $ r1 * yyy__ dxTexcoord + r2 * yyy__ dyTexcoord

	s <- temp $ invSqrt $ max_ (norm2 t) (norm2 b)

	tt <- temp $ t * vecFromScalar s
	bb <- temp $ b * vecFromScalar s

	return (tt, bb, normal)

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
	temp $ (1 + e) / (8 * (lh * lh * lh)) * exp (e * log (dot normal toEyeLightHalfDirection))
