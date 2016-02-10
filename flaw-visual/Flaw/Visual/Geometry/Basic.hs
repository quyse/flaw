{-|
Module: Flaw.Visual.Geometry.Basic
Description: Basic geometry generation.
License: MIT
-}

module Flaw.Visual.Geometry.Basic
	( patchTopology
	, sphereVertices
	, twoHemispheresVertices
	) where

import qualified Data.Vector as V

-- | Permute and duplicate vertices to produce a patch.
patchTopology :: V.Vector v -> Int -> Int -> V.Vector v
patchTopology vertices width height = V.backpermute vertices indices where
	indices = V.fromList $ do
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

-- | Raw vertices for a sphere.
-- For 0th meridian it produces double vertices (with longitude = 0 and pi * 2), giving a chance for different vertices
-- (for example, to use different texture coordinates).
sphereVertices
	:: (Float -> Float -> v) -- ^ Function producing vertex for specified longitude and latitude.
	-> Int -- ^ Meridians count.
	-> Int -- ^ Parallels half-count.
	-> V.Vector v
sphereVertices f meridiansCount halfParallelsCount = patchTopology vertices (halfParallelsCount * 2 + 1) (meridiansCount + 1) where
	meridianCoef = pi * 2 / fromIntegral meridiansCount
	parallelCoef = pi * 0.5 / fromIntegral halfParallelsCount
	angles = V.fromList
		[ ( fromIntegral i * meridianCoef
			, fromIntegral j * parallelCoef
			)
		| i <- [0 .. meridiansCount]
		, j <- [-halfParallelsCount .. halfParallelsCount]
		]
	vertices = V.map (uncurry f) angles

-- | Raw vertices for sphere with different number of parallels for top and bottom hemispheres.
twoHemispheresVertices
	:: (Float -> Float -> v)
	-> Int -- ^ Meridians count.
	-> Int -- ^ Top parallels count.
	-> Int -- ^ Bottom parallels count.
	-> V.Vector v
twoHemispheresVertices f meridiansCount topParallelsCount bottomParallelsCount = patchTopology vertices (bottomParallelsCount + 1 + topParallelsCount) (meridiansCount + 1) where
	meridianCoef = pi * 2 / fromIntegral meridiansCount
	topParallelCoef = pi * 0.5 / fromIntegral topParallelsCount
	bottomParallelCoef = pi * 0.5 / fromIntegral bottomParallelsCount
	parallels = map ((* bottomParallelCoef) . fromIntegral) [(-bottomParallelsCount) .. (-1)] ++ 0 : map ((* topParallelCoef) . fromIntegral) [1 .. topParallelsCount]
	angles = V.fromList
		[ ( fromIntegral i * meridianCoef
			, parallel
			)
		| i <- [0 .. meridiansCount]
		, parallel <- parallels
		]
	vertices = V.map (uncurry f) angles
