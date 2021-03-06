{-|
Module: Flaw.Math.Geometry
Description: Geometric functions.
License: MIT
-}

module Flaw.Math.Geometry
  ( affineIdentity
  , affineTranslation
  , affineScaling
  , affineLookAt
  , affineAxisRotation
  , affineFromQuat
  , projectionOrtho
  , projectionPerspectiveFov
  ) where

import Flaw.Math

{-# INLINE affineIdentity #-}
affineIdentity :: (Vectorized a, Num a) => Mat4x4 a
affineIdentity = Mat4x4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

{-# INLINE affineTranslation #-}
affineTranslation :: (Vectorized a, Num a) => Vec3 a -> Mat4x4 a
affineTranslation (Vec3 x y z) = Mat4x4
  1 0 0 x
  0 1 0 y
  0 0 1 z
  0 0 0 1

{-# INLINE affineScaling #-}
affineScaling :: (Vectorized a, Num a) => Vec3 a -> Mat4x4 a
affineScaling (Vec3 x y z) = Mat4x4
  x 0 0 0
  0 y 0 0
  0 0 z 0
  0 0 0 1

{-# INLINE affineLookAt #-}
affineLookAt :: (Vectorized a, Floating a) => Vec3 a -> Vec3 a -> Vec3 a -> Mat4x4 a
affineLookAt eye target up = r where
  z@(Vec3 zx zy zz) = normalize $ eye - target
  x@(Vec3 xx xy xz) = normalize $ cross up z
  y@(Vec3 yx yy yz) = cross z x
  ox = dot x eye
  oy = dot y eye
  oz = dot z eye
  r = Mat4x4
    xx xy xz (-ox)
    yx yy yz (-oy)
    zx zy zz (-oz)
    0  0  0  1

{-# INLINE affineAxisRotation #-}
affineAxisRotation :: Quaternionized a => Vec3 a -> a -> Quat a
affineAxisRotation (Vec3 x y z) angle = r where
  ha = angle * 0.5
  sa = sin ha
  ca = cos ha
  r = Quat $ Vec4 (x * sa) (y * sa) (z * sa) ca

{-# INLINE affineFromQuat #-}
affineFromQuat :: Quaternionized a => Quat a -> Mat4x4 a
affineFromQuat (Quat (Vec4 x y z w)) = r where
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
    (ww + xx - yy - zz) (xy2 - wz2)         (xz2 + wy2)         0
    (xy2 + wz2)         (ww - xx + yy - zz) (yz2 - wx2)         0
    (xz2 - wy2)         (yz2 + wx2)         (ww - xx - yy + zz) 0
    0                   0                   0                   1

-- | Orthographic projection matrix.
{-# INLINE projectionOrtho #-}
projectionOrtho :: (Vectorized a, Floating a)
  => a -- ^ Width of screen in view-space units.
  -> a -- ^ Height of screen in view-space units.
  -> a -- ^ Z mapped to 0.
  -> a -- ^ Z mapped to 1.
  -> Mat4x4 a
projectionOrtho width height z0 z1 = Mat4x4
  (2 / width) 0 0 0
  0 (2 / height) 0 0
  0 0 (1 / (z1 - z0)) (z0 / (z0 - z1))
  0 0 0 1

-- | Perspective projection matrix.
{-# INLINE projectionPerspectiveFov #-}
projectionPerspectiveFov :: (Vectorized a, Floating a)
  => a -- ^ Vertical field of view in radians.
  -> a -- ^ Viewport width / height.
  -> a -- ^ Linear Z mapped to homogeneous 0.
  -> a -- ^ Linear Z mapped to homogeneous 1.
  -> Mat4x4 a
projectionPerspectiveFov fovY aspect z0 z1 = r where
  ys = 1 / tan (fovY * 0.5)
  xs = ys / aspect
  r = Mat4x4
    xs 0  0                0
    0  ys 0                0
    0  0  (z1 / (z0 - z1)) (z0 * z1 / (z1 - z0))
    0  0  (-1)             0
