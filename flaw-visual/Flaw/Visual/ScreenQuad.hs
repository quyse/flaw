{-|
Module: Flaw.Visual.ScreenQuad
Description: Rendering full-screen quads.
License: MIT
-}

module Flaw.Visual.ScreenQuad
	( ScreenQuadRenderer(..)
	, newScreenQuadRenderer
	, renderScreenQuad
	, screenQuadProgram
	) where

import qualified Data.Vector as V
import Foreign.Storable

import Flaw.Book
import Flaw.Build
import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Math
import Flaw.Visual.Geometry.Vertex

newtype ScreenQuadRenderer d = ScreenQuadRenderer
	{ screenQuadRendererVertexBuffer :: VertexBufferId d
	}

newScreenQuadRenderer :: Device d => d -> IO (ScreenQuadRenderer d, IO ())
newScreenQuadRenderer device = withSpecialBook $ \bk -> do
	let
		v0 = QuadVertex (Vec4 (-1) (-1) 0 1)
		v1 = QuadVertex (Vec4   1  (-1) 0 1)
		v2 = QuadVertex (Vec4   1    1  0 1)
		v3 = QuadVertex (Vec4 (-1)   1  0 1)
		bytes = packVector $ V.fromList [v0, v2, v1, v0, v3, v2]
	vb <- book bk $ createStaticVertexBuffer device bytes (sizeOf (undefined :: QuadVertex))
	return ScreenQuadRenderer
		{ screenQuadRendererVertexBuffer = vb
		}

renderScreenQuad :: Context c d => ScreenQuadRenderer d -> Render c ()
renderScreenQuad ScreenQuadRenderer
	{ screenQuadRendererVertexBuffer = vb
	} = renderScope $ do
	renderVertexBuffer 0 vb
	renderIndexBuffer nullIndexBuffer
	renderDraw 6

-- | Works with geometry from 'screenQuadVertices'.
screenQuadProgram :: (Node Float4 -> Program ()) -> Program ()
screenQuadProgram f = do
	aPosition <- attribute 0 0 0 $ AttributeVec4 AttributeFloat32
	screenPositionTexcoord <- temp $ cvec22 (xy__ aPosition) $ screenToTexture $ xy__ aPosition
	rasterize aPosition $ f screenPositionTexcoord
