{-|
Module: Flaw.Visual.Geometry.Basic
Description: Basic geometry generation.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Visual.Geometry.Basic
  ( patchTopology
  , planeVertices
  , sphereVertices
  , twoHemispheresVertices
  , openCylinderVertices
  ) where

import qualified Data.Vector.Generic as VG

-- | Permute and duplicate vertices to produce a patch.
patchTopology :: (VG.Vector v a, VG.Vector v Int) => v a -> Int -> Int -> v a
patchTopology vertices width height = VG.backpermute vertices indices where
  indices = VG.fromList $ do
    i <- [0 .. height - 2]
    j <- [0 .. width - 2]
    let ni = i + 1
    let nj = j + 1
    [   vertexIndex i j
      , vertexIndex i nj
      , vertexIndex ni j
      , vertexIndex i nj
      , vertexIndex ni nj
      , vertexIndex ni j
      ]
  vertexIndex i j = i * width + j

-- | Raw vertices for a plane.
planeVertices
  :: (VG.Vector v a, VG.Vector v Int)
  => (Float -> Float -> a) -- ^ Function producing vertex for specified coordinates (from 0 to 1).
  -> Int -- ^ Width in cells (i.e. 1 for a simple quad).
  -> Int -- ^ Height in cells (i.e. 1 for a simple quad).
  -> v a
planeVertices f width height = patchTopology (VG.fromList $ map (uncurry f) vertices) (width + 1) (height + 1) where
  vertices =
    [ ( fromIntegral i * heightCoef
      , fromIntegral j * widthCoef
      )
    | i <- [0 .. height]
    , j <- [0 .. width]
    ]
  widthCoef = 1 / fromIntegral width
  heightCoef = 1 / fromIntegral height

-- | Raw vertices for a sphere.
-- For 0th meridian it produces double vertices (with longitude = 0 and pi * 2), giving a chance for different vertices
-- (for example, to use different texture coordinates).
sphereVertices
  :: (VG.Vector v a, VG.Vector v Int)
  => (Float -> Float -> a) -- ^ Function producing vertex for specified longitude (from 0 to pi * 2) and latitude (from -pi / 2 to pi / 2).
  -> Int -- ^ Meridians count.
  -> Int -- ^ Parallels half-count.
  -> v a
sphereVertices f meridiansCount halfParallelsCount = patchTopology vertices (halfParallelsCount * 2 + 1) (meridiansCount + 1) where
  meridianCoef = pi * 2 / fromIntegral meridiansCount
  parallelCoef = pi * 0.5 / fromIntegral halfParallelsCount
  angles =
    [ ( fromIntegral i * meridianCoef
      , fromIntegral j * parallelCoef
      )
    | i <- [0 .. meridiansCount]
    , j <- [-halfParallelsCount .. halfParallelsCount]
    ]
  vertices = VG.fromList $ map (uncurry f) angles

-- | Raw vertices for sphere with different number of parallels for top and bottom hemispheres.
twoHemispheresVertices
  :: (VG.Vector v a, VG.Vector v Int)
  => (Float -> Float -> a)
  -> Int -- ^ Meridians count.
  -> Int -- ^ Top parallels count.
  -> Int -- ^ Bottom parallels count.
  -> v a
twoHemispheresVertices f meridiansCount topParallelsCount bottomParallelsCount = patchTopology vertices (bottomParallelsCount + 1 + topParallelsCount) (meridiansCount + 1) where
  meridianCoef = pi * 2 / fromIntegral meridiansCount
  topParallelCoef = pi * 0.5 / fromIntegral topParallelsCount
  bottomParallelCoef = pi * 0.5 / fromIntegral bottomParallelsCount
  parallels = map ((* bottomParallelCoef) . fromIntegral) [(-bottomParallelsCount) .. (-1)] ++ 0 : map ((* topParallelCoef) . fromIntegral) [1 .. topParallelsCount]
  angles =
    [ ( fromIntegral i * meridianCoef
      , parallel
      )
    | i <- [0 .. meridiansCount]
    , parallel <- parallels
    ]
  vertices = VG.fromList $ map (uncurry f) angles

-- | Raw vertices for an open cylinder.
openCylinderVertices
  :: (VG.Vector v a, VG.Vector v Int)
  => (Float -> Float -> a) -- ^ Function producing vertex for specified longitude (from 0 to pi * 2) and parallel (from 0 to 1).
  -> Int -- ^ Meridians count.
  -> Int -- ^ Parallels count.
  -> v a
openCylinderVertices f meridiansCount parallelsCount = patchTopology vertices parallelsCount (meridiansCount + 1) where
  meridianCoef = pi * 2 / fromIntegral meridiansCount
  parallelCoef = 1 / fromIntegral (parallelsCount - 1)
  vertices = VG.fromList $ map (uncurry f)
    [ ( fromIntegral i * meridianCoef
      , fromIntegral j * parallelCoef
      )
    | i <- [0 .. meridiansCount]
    , j <- [0 .. (parallelsCount - 1)]
    ]
