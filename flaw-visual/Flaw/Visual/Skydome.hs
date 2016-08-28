{-|
Module: Flaw.Visual.Skydome
Description: Skydome.
License: MIT
-}

module Flaw.Visual.Skydome
	( skydomeGeometry
	, skydomeTransform
	) where

import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Math
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Basic
import Flaw.Visual.Geometry.Vertex

skydomeGeometry :: Device d => d -> Int -> Int -> Int -> IO (Geometry d, IO ())
skydomeGeometry device meridiansCount topParallelsCount bottomParallelsCount =
	loadPackedGeometry device =<< packGeometry vertices where

	vertices = twoHemispheresVertices f meridiansCount topParallelsCount bottomParallelsCount
	f alpha beta = VertexPT
		{ f_VertexPT_position = Float3 (cos alpha * cos beta) (sin alpha * cos beta) (negate $ sin beta)
		, f_VertexPT_texcoord = Float2 (alpha / (pi * 2)) (beta / pi + 0.5)
		}

skydomeTransform :: Node Float4x4 -> Node Float3 -> Float -> Program (Node Float4)
skydomeTransform viewProj position distance =
	temp $ viewProj `mul` cvec31 (position * vecFromScalar (constf distance)) (constf 1)
