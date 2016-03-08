{-|
Module: Flaw.Math.Transform
Description: Geometric functions.
License: MIT
-}

{-# LANGUAGE Safe #-}

module Flaw.Math.Transform
	(
	-- * Transform class
	  Transform(..)
	-- * Quaternion plus offset
	, QuatOffset(..)
	, FloatQO, DoubleQO
	-- * Dual quaternion
	, DualQuat(..)
	, FloatDQ, DoubleDQ
	-- * Helper functions
	, quatAxisRotation
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
	transformAxisRotation axis angle = transformToMatrix $ QuatOffset (quatAxisRotation axis angle) (Vec3 0 0 0)

-- | Quaternion representing rotation around specified axis.
-- Axis should be normalized.
{-# INLINE quatAxisRotation #-}
quatAxisRotation :: Quaternionized a => Vec3 a -> a -> Quat a
quatAxisRotation (Vec3 x y z) angle = r where
	ha = angle * 0.5
	sa = sin ha
	ca = cos ha
	r = Quat $ Vec4 (x * sa) (y * sa) (z * sa) ca

-- | 3D transformation represented by normalized quaternion and offset.
data QuatOffset a = QuatOffset (Quat a) (Vec3 a) deriving (Eq, Ord, Show)

type FloatQO = QuatOffset Float
type DoubleQO = QuatOffset Double

instance Transform QuatOffset where
	{-# INLINE identityTransform #-}
	identityTransform = QuatOffset (Quat (Vec4 0 0 0 1)) (Vec3 0 0 0)
	{-# INLINE applyTransform #-}
	applyTransform (QuatOffset q p) (Vec3 x y z) = Vec3 rx ry rz + p where
		Quat (Vec4 rx ry rz _rw) = q * Quat (Vec4 x y z 0) * conjugate q
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
	transformFromMatrix (Mat4x4
		m11 m12 m13 m14
		m21 m22 m23 m24
		m31 m32 m33 m34
		_41 _42 _43 _44) = QuatOffset (normalize $ Quat (Vec4 x y z w)) (Vec3 m14 m24 m34) where
		k = sqrt (1 + m11 + m22 + m33)
		kk = 0.5 / k
		x = (m32 - m23) * kk
		y = (m13 - m31) * kk
		z = (m21 - m12) * kk
		w = k * 0.5
	{-# INLINE transformTranslation #-}
	transformTranslation p = QuatOffset (Quat (Vec4 0 0 0 1)) p
	{-# INLINE transformAxisRotation #-}
	transformAxisRotation axis angle = QuatOffset (quatAxisRotation axis angle) (Vec3 0 0 0)

-- | Dual quaternion representing transforms in 3D space.
data DualQuat a = DualQuat !(Quat a) !(Quat a) deriving (Eq, Ord, Show)

type FloatDQ = DualQuat Float
type DoubleDQ = DualQuat Double

instance Quaternionized a => Num (DualQuat a) where
	{-# INLINE (+) #-}
	(DualQuat q1 p1) + (DualQuat q2 p2) = DualQuat (q1 + q2) (p1 + p2)
	{-# INLINE (-) #-}
	(DualQuat q1 p1) - (DualQuat q2 p2) = DualQuat (q1 - q2) (p1 - p2)
	{-# INLINE (*) #-}
	(DualQuat q1 p1) * (DualQuat q2 p2) = DualQuat (q1 * q2) (q1 * p2 + p1 * q2)
	{-# INLINE negate #-}
	negate (DualQuat q p) = DualQuat (negate q) (negate p)
	{-# INLINE abs #-}
	abs (DualQuat q p) = DualQuat (abs q) (abs p)
	{-# INLINE signum #-}
	signum = error "signum for DualQuat is not implemented"
	{-# INLINE fromInteger #-}
	fromInteger = error "fromInteger for DualQuat is not implemented"

instance Quaternionized a => Conjugate (DualQuat a) where
	{-# INLINE conjugate #-}
	conjugate (DualQuat q p) = DualQuat (conjugate q) (conjugate p)

{-# INLINE dualConjugate #-}
dualConjugate :: Quaternionized a => DualQuat a -> DualQuat a
dualConjugate (DualQuat q p) = DualQuat q (negate p)

-- | Equivalent of (dualConjugate . conjugate)
{-# INLINE dualConjugateConjugate #-}
dualConjugateConjugate :: Quaternionized a => DualQuat a -> DualQuat a
dualConjugateConjugate (DualQuat (Quat (Vec4 qx qy qz qw)) (Quat (Vec4 px py pz pw))) =
	DualQuat (Quat (Vec4 (-qx) (-qy) (-qz) qw)) (Quat (Vec4 px py pz (-pw)))

instance Transform DualQuat where
	{-# INLINE identityTransform #-}
	identityTransform = DualQuat (Quat (Vec4 0 0 0 1)) (Quat (Vec4 0 0 0 0))
	{-# INLINE applyTransform #-}
	applyTransform q (Vec3 x y z) = Vec3 rx ry rz where
		DualQuat _rq (Quat (Vec4 rx ry rz _rw)) = q * a * dualConjugateConjugate q
		a = DualQuat (Quat (Vec4 0 0 0 1)) (Quat (Vec4 x y z 0))
	{-# INLINE combineTransform #-}
	combineTransform = (*)
	{-# INLINE transformToMatrix #-}
	transformToMatrix (DualQuat q p) = transformToMatrix $ QuatOffset q (Vec3 (x * 2) (y * 2) (z * 2)) where
		Quat (Vec4 x y z _w) = p * conjugate q
	{-# INLINE transformFromMatrix #-}
	transformFromMatrix (Mat4x4
		m11 m12 m13 m14
		m21 m22 m23 m24
		m31 m32 m33 m34
		_41 _42 _43 _44) = DualQuat q (Quat (Vec4 (m14 * 0.5) (m24 * 0.5) (m34 * 0.5) 0) * q) where
		k = sqrt (1 + m11 + m22 + m33)
		kk = 0.5 / k
		x = (m32 - m23) * kk
		y = (m13 - m31) * kk
		z = (m21 - m12) * kk
		w = k * 0.5
		q = normalize $ Quat $ Vec4 x y z w
	{-# INLINE transformTranslation #-}
	transformTranslation (Vec3 x y z) = DualQuat (Quat (Vec4 0 0 0 1)) (Quat (Vec4 (x * 0.5) (y * 0.5) (z * 0.5) 0))
	{-# INLINE transformAxisRotation #-}
	transformAxisRotation axis angle = DualQuat (quatAxisRotation axis angle) (Quat (Vec4 0 0 0 0))
