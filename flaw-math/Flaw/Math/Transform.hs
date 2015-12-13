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
	identityTransform :: Quaternionized a => t a
	applyTransform :: Quaternionized a => t a -> Vec3 a -> Vec3 a
	combineTransform :: Quaternionized a => t a -> t a -> t a
	transformToMatrix :: Quaternionized a => t a -> Mat4x4 a
	transformFromMatrix :: Quaternionized a => Mat4x4 a -> t a
	transformTranslation :: Quaternionized a => Vec3 a -> t a
	transformAxisRotation :: Quaternionized a => Vec3 a -> a -> t a

instance Transform Mat4x4 where
	{-# INLINE identityTransform #-}
	identityTransform = Mat4x4
		1 0 0 0
		0 1 0 0
		0 0 1 0
		0 0 0 1
	{-# INLINE applyTransform #-}
	applyTransform m (Vec3 x y z) = Vec3 rx ry rz where
		Vec4 rx ry rz _rw = mul m (Vec4 x y z 1)
	{-# INLINE combineTransform #-}
	combineTransform = mul
	{-# INLINE transformToMatrix #-}
	transformToMatrix = id
	{-# INLINE transformFromMatrix #-}
	transformFromMatrix = id
	{-# INLINE transformTranslation #-}
	transformTranslation (Vec3 x y z) = Mat4x4
		1 0 0 x
		0 1 0 y
		0 0 1 z
		0 0 0 1
	transformAxisRotation axis angle = transformToMatrix $ QuatOffset (quaternionAxisRotation axis angle) (Vec3 0 0 0)

{-# INLINE quaternionAxisRotation #-}
quaternionAxisRotation :: Quaternionized a => Vec3 a -> a -> Quat a
quaternionAxisRotation (Vec3 x y z) angle = r where
	ha = angle * 0.5
	sa = sin ha
	ca = cos ha
	r = Quat $ Vec4 (x * sa) (y * sa) (z * sa) ca

-- | 3D transformation represented by normalized quaternion and offset.
data QuatOffset a = QuatOffset (Quat a) (Vec3 a) deriving (Eq, Ord, Show)

instance Transform QuatOffset where
	{-# INLINE identityTransform #-}
	identityTransform = QuatOffset (Quat (Vec4 0 0 0 1)) (Vec3 0 0 0)
	{-# INLINE applyTransform #-}
	applyTransform (QuatOffset q p) (Vec3 x y z) = (Vec3 rx ry rz) + p where
		Quat (Vec4 rx ry rz _rw) = q * (Quat (Vec4 x y z 0)) * (conjugate q)
	{-# INLINE combineTransform #-}
	combineTransform t2@(QuatOffset q2 _p2) (QuatOffset q1 p1) = QuatOffset (q2 * q1) (applyTransform t2 p1)
	{-# INLINE transformToMatrix #-}
	transformToMatrix (QuatOffset (Quat (Vec4 x y z w)) (Vec3 px py pz)) = r where
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
	{-# INLINE transformFromMatrix #-}
	transformFromMatrix = undefined
	{-# INLINE transformTranslation #-}
	transformTranslation p = QuatOffset (Quat (Vec4 0 0 0 1)) p
	{-# INLINE transformAxisRotation #-}
	transformAxisRotation axis angle = QuatOffset (quaternionAxisRotation axis angle) (Vec3 0 0 0)

-- | Dual quaternion representing transforms in 3D space.
data DualQuaternion a = DualQuaternion (Quat a) (Quat a) deriving (Eq, Ord, Show)

instance Quaternionized a => Num (DualQuaternion a) where
	{-# INLINE (+) #-}
	(DualQuaternion q1 p1) + (DualQuaternion q2 p2) = DualQuaternion (q1 + q2) (p1 + p2)
	{-# INLINE (-) #-}
	(DualQuaternion q1 p1) - (DualQuaternion q2 p2) = DualQuaternion (q1 - q2) (p1 - p2)
	{-# INLINE (*) #-}
	(DualQuaternion q1 p1) * (DualQuaternion q2 p2) = DualQuaternion (q1 * q2) (q1 * p2 + p1 * q2)
	{-# INLINE negate #-}
	negate (DualQuaternion q p) = DualQuaternion (negate q) (negate p)
	{-# INLINE abs #-}
	abs (DualQuaternion q p) = DualQuaternion (abs q) (abs p)
	{-# INLINE signum #-}
	signum = undefined
	{-# INLINE fromInteger #-}
	fromInteger = undefined

instance Quaternionized a => Conjugate (DualQuaternion a) where
	{-# INLINE conjugate #-}
	conjugate (DualQuaternion q p) = DualQuaternion (conjugate q) (conjugate p)

{-# INLINE dualConjugate #-}
dualConjugate :: Quaternionized a => DualQuaternion a -> DualQuaternion a
dualConjugate (DualQuaternion q p) = DualQuaternion q (-p)

instance Transform DualQuaternion where
	{-# INLINE identityTransform #-}
	identityTransform = DualQuaternion (Quat (Vec4 0 0 0 1)) (Quat (Vec4 0 0 0 0))
	{-# INLINE applyTransform #-}
	applyTransform q (Vec3 x y z) = Vec3 rx ry rz where
		DualQuaternion _rq (Quat (Vec4 rx ry rz _rw)) = q * a * (dualConjugate $ conjugate q)
		a = DualQuaternion (Quat (Vec4 0 0 0 1)) (Quat (Vec4 x y z 0))
	{-# INLINE combineTransform #-}
	combineTransform q2 q1 = q2 * q1
	{-# INLINE transformToMatrix #-}
	transformToMatrix = undefined
	{-# INLINE transformFromMatrix #-}
	transformFromMatrix = undefined
	{-# INLINE transformTranslation #-}
	transformTranslation (Vec3 x y z) = DualQuaternion (Quat (Vec4 0 0 0 1)) (Quat (Vec4 (x * 0.5) (y * 0.5) (z * 0.5) 0))
	{-# INLINE transformAxisRotation #-}
	transformAxisRotation axis angle = DualQuaternion (quaternionAxisRotation axis angle) (Quat (Vec4 0 0 0 0))
