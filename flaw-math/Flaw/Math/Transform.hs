{-|
Module: Flaw.Math.Transform
Description: Geometric functions.
License: MIT
-}

module Flaw.Math.Transform
	( Transform(..)
	, QuatOffset(..)
	, DualQuaternion(..)
	) where

import Flaw.Math

class Transform t where
	identityTransform :: Num a => t a
	applyTransform :: Num a => t a -> Vec3 a -> Vec3 a
	combineTransform :: Num a => t a -> t a -> t a
	transformToMatrix :: Num a => t a -> Mat4x4 a
	transformTranslation :: Fractional a => Vec3 a -> t a
	transformAxisRotation :: Floating a => Vec3 a -> a -> t a

instance Transform Mat4x4 where
	identityTransform = Mat4x4
		1 0 0 0
		0 1 0 0
		0 0 1 0
		0 0 0 1
	applyTransform m (Vec3 x y z) = Vec3 rx ry rz where
		Vec4 rx ry rz _rw = mul m (Vec4 x y z 1)
	combineTransform = mul
	transformToMatrix = id
	transformTranslation (Vec3 x y z) = Mat4x4
		1 0 0 x
		0 1 0 y
		0 0 1 z
		0 0 0 1
	transformAxisRotation = undefined

quaternionAxisRotation :: Floating a => Vec3 a -> a -> Quaternion a
quaternionAxisRotation (Vec3 x y z) angle = r where
	ha = angle * 0.5
	sa = sin ha
	ca = cos ha
	r = Quaternion $ Vec4 (x * sa) (y * sa) (z * sa) ca

-- | 3D transformation represented by normalized quaternion and offset.
data QuatOffset a = QuatOffset (Quaternion a) (Vec3 a)

instance Transform QuatOffset where
	identityTransform = QuatOffset (Quaternion (Vec4 0 0 0 1)) (Vec3 0 0 0)
	applyTransform (QuatOffset q p) (Vec3 x y z) = (Vec3 rx ry rz) + p where
		Quaternion (Vec4 rx ry rz _rw) = q * (Quaternion (Vec4 x y z 0)) * (conjugate q)
	combineTransform t2@(QuatOffset q2 _p2) (QuatOffset q1 p1) = QuatOffset (q2 * q1) (applyTransform t2 p1)
	transformToMatrix (QuatOffset (Quaternion (Vec4 x y z w)) (Vec3 px py pz)) = r where
		ww = w * w
		xx = x * x
		yy = y * y
		zz = z * z
		wx2 = w * x * 2
		wy2 = w * y * 2
		wz2 = w * z * 2
		xy2 = x * y * 2
		xz2 = x * z * 2
		yz2 = y * z * 2
		r = Mat4x4
			(ww + xx - yy - zz) (xy2 - wz2) (xz2 + wy2) px
			(xy2 + wz2) (ww - xx + yy - zz) (yz2 - wx2) py
			(xz2 - wy2) (yz2 + wx2) (ww - xx - yy + zz) pz
			0 0 0 1
	transformTranslation p = QuatOffset (Quaternion (Vec4 0 0 0 1)) p
	transformAxisRotation axis angle = QuatOffset (quaternionAxisRotation axis angle) (Vec3 0 0 0)

-- | Dual quaternion representing transforms in 3D space.
data DualQuaternion a = DualQuaternion (Quaternion a) (Quaternion a)

instance Num a => Num (DualQuaternion a) where
	(DualQuaternion q1 p1) + (DualQuaternion q2 p2) = DualQuaternion (q1 + q2) (p1 + p2)
	(DualQuaternion q1 p1) - (DualQuaternion q2 p2) = DualQuaternion (q1 - q2) (p1 - p2)
	(DualQuaternion q1 p1) * (DualQuaternion q2 p2) = DualQuaternion (q1 * q2) (q1 * p2 + p1 * q2)
	negate (DualQuaternion q p) = DualQuaternion (negate q) (negate p)
	abs (DualQuaternion q p) = DualQuaternion (abs q) (abs p)
	signum = undefined
	fromInteger = undefined

instance Num a => Conjugate (DualQuaternion a) where
	conjugate (DualQuaternion q p) = DualQuaternion (conjugate q) (conjugate p)

dualConjugate :: Num a => DualQuaternion a -> DualQuaternion a
dualConjugate (DualQuaternion q p) = DualQuaternion q (-p)

instance Transform DualQuaternion where
	identityTransform = DualQuaternion (Quaternion (Vec4 0 0 0 1)) (Quaternion (Vec4 0 0 0 0))
	applyTransform q (Vec3 x y z) = Vec3 rx ry rz where
		DualQuaternion _rq (Quaternion (Vec4 rx ry rz _rw)) = q * a * (dualConjugate $ conjugate q)
		a = DualQuaternion (Quaternion (Vec4 0 0 0 1)) (Quaternion (Vec4 x y z 0))
	combineTransform q2 q1 = q2 * q1
	transformToMatrix = undefined
	transformTranslation (Vec3 x y z) = DualQuaternion (Quaternion (Vec4 0 0 0 1)) (Quaternion (Vec4 (x * 0.5) (y * 0.5) (z * 0.5) 0))
	transformAxisRotation axis angle = DualQuaternion (quaternionAxisRotation axis angle) (Quaternion (Vec4 0 0 0 0))
