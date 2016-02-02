{-|
Module: Flaw.Visual.Skydome
Description: Skydome.
License: MIT
-}

module Flaw.Visual.Skydome
	( skydomeGeometry
	, skydomeTransform
	) where

import qualified Data.Vector as V

import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Math
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Vertex

skydomeGeometry :: Device d => d -> Int -> Int -> Int -> IO (Geometry d, IO ())
skydomeGeometry device meridiansCount topParallelsCount bottomParallelsCount = do

	let
		meridianCoef = pi * 2 / fromIntegral meridiansCount
		topParallelCoef = pi * 0.5 / fromIntegral topParallelsCount
		bottomParallelCoef = pi * 0.5 / fromIntegral bottomParallelsCount
		parallelsCount = bottomParallelsCount + 1 + topParallelsCount

		vertexMatrix = V.fromList $ do
			i <- [0 .. meridiansCount]
			beta <- map ((* bottomParallelCoef) . fromIntegral) [(-bottomParallelsCount) .. (-1)] ++ 0 : map ((* topParallelCoef) . fromIntegral) [1 .. topParallelsCount]
			let alpha = fromIntegral i * meridianCoef
			return VertexPT
				{ f_VertexPT_position = Float3 (cos alpha * cos beta) (sin alpha * cos beta) (sin beta)
				, f_VertexPT_texcoord = Float2 (alpha / (pi * 2)) (beta / (-pi) + 0.5)
				}
		vertex i j = vertexMatrix V.! (i * fromIntegral parallelsCount + j)

		vertices = V.fromList $ do
			i <- [0 .. (meridiansCount - 1)]
			j <- [0 .. (parallelsCount - 2)]
			let ni = (i + 1) `rem` (meridiansCount + 1)
			let nj = j + 1
			[   vertex i j
				, vertex ni j
				, vertex i nj
				, vertex i nj
				, vertex ni j
				, vertex ni nj
				]

	loadPackedGeometry device =<< packGeometry vertices

skydomeTransform :: Node Float4x4 -> Node Float3 -> Float -> Program (Node Float4)
skydomeTransform viewProj position distance = do
	temp $ viewProj `mul` (cvec31 (position * vecFromScalar (constf distance)) (constf 1))
