{-|
Module: Flaw.Graphics.Canvas
Description: 2D painting.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Graphics.Canvas
	( Canvas
	, initCanvas
	, drawBorderedRectangle
	) where

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Program
import Flaw.Math

import qualified Data.ByteString.Unsafe as B
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | Canvas contains additional things for 2D rendering.
data Canvas d = Canvas
	{
	-- | Blend state for usual blending.
	  canvasBlendState :: !(BlendStateId d)
	, canvasUs :: !(UniformStorage d)
	, canvasVbBorderedRectangle :: !(VertexBufferId d)
	, canvasUX :: !(Node Float4)
	, canvasUY :: !(Node Float4)
	, canvasUFillColor :: !(Node Float4)
	, canvasUBorderColor :: !(Node Float4)
	, canvasBorderedRectangleProgram :: !(ProgramId d)
	}

initCanvas :: Device d => d -> IO (Canvas d, IO ())
initCanvas device = withSpecialBook $ \bk -> do

	-- create blend state for usual blending
	blendState <- book bk $ createBlendState device defaultBlendStateInfo
		{ blendSourceColor = ColorSourceSrcAlpha
		, blendDestColor = ColorSourceInvSrcAlpha
		}

	vbBorderedRectangle <- do
		let
			vbStride = sizeOf (undefined :: Float) * 10
			vertices = do
				i <- [0..2] :: [Int] -- rows of quads from bottom to top
				j <- [0..2] :: [Int] -- columns of quads from left to right
				(x, y) <- [(1, 0), (0, 0), (0, 1), (1, 0), (0, 1), (1, 1)] -- 6 vertices per quad
				let r =
					[ if x + j == 0 then 1 else 0 -- x1
					, if x + j == 1 then 1 else 0 -- x2
					, if x + j == 2 then 1 else 0 -- x3
					, if x + j == 3 then 1 else 0 -- x4
					, if y + i == 0 then 1 else 0 -- y1
					, if y + i == 1 then 1 else 0 -- y2
					, if y + i == 2 then 1 else 0 -- y3
					, if y + i == 3 then 1 else 0 -- y4
					, if i == 1 && j == 1 then 1 else 0 -- use fill color?
					, if i == 1 && j == 1 then 0 else 1 -- use border color?
					]
				r
		book bk $ withArray (vertices :: [Float]) $ \ptr -> do
			bytes <- B.unsafePackCStringLen (castPtr ptr, vbStride * BORDERED_RECTANGLE_INDEX_COUNT)
			createStaticVertexBuffer device bytes vbStride

	ubs <- uniformBufferSlot 0
	-- uniform containing four X coordinates (from left to right)
	uX <- uniform ubs
	-- uniform containing four Y coordinates (from bottom to top)
	uY <- uniform ubs
	-- uniform with fill color
	uFillColor <- uniform ubs :: IO (Node Float4)
	-- uniform with border color
	uBorderColor <- uniform ubs :: IO (Node Float4)
	us <- book bk $ createUniformStorage device ubs
	borderedRectangleProgram <- book bk $ createProgram device $ do
		aX <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
		aY <- attribute 0 16 0 (AttributeVec4 AttributeFloat32)
		aC <- attribute 0 32 0 (AttributeVec2 AttributeFloat32)
		color <- temp (uFillColor * vecFromScalar (x_ aC) + uBorderColor * vecFromScalar (y_ aC))
		rasterize (cvec1111 (dot aX uX) (dot aY uY) (constf 0) (constf 1)) $ colorTarget 0 color

	return Canvas
		{ canvasBlendState = blendState
		, canvasUs = us
		, canvasVbBorderedRectangle = vbBorderedRectangle
		, canvasUX = uX
		, canvasUY = uY
		, canvasUFillColor = uFillColor
		, canvasUBorderColor = uBorderColor
		, canvasBorderedRectangleProgram = borderedRectangleProgram
		}

-- | Draw bordered rectangle.
-- Coordinates are integer pixels. Y directed down.
drawBorderedRectangle :: Context c d => Canvas d -> Int4 -> Int4 -> Float4 -> Float4 -> Render c ()
drawBorderedRectangle Canvas
	{ canvasBlendState = blendState
	, canvasUs = us
	, canvasVbBorderedRectangle = vbBorderedRectangle
	, canvasUX = uX
	, canvasUY = uY
	, canvasUFillColor = uFillColor
	, canvasUBorderColor = uBorderColor
	, canvasBorderedRectangleProgram = borderedRectangleProgram
	} (Int4 x1 x2 x3 x4) (Int4 y1 y2 y3 y4) fillColor borderColor = renderScope $ do

	Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
	let
		scaleX = 2 / fromIntegral (viewportRight - viewportLeft)
		scaleY = 2 / fromIntegral (viewportTop - viewportBottom)
		xs = Float4
			(fromIntegral x1 * scaleX - 1)
			(fromIntegral x2 * scaleX - 1)
			(fromIntegral x3 * scaleX - 1)
			(fromIntegral x4 * scaleX - 1)
		ys = Float4 -- note reverse order of y's
			(fromIntegral y4 * scaleY + 1)
			(fromIntegral y3 * scaleY + 1)
			(fromIntegral y2 * scaleY + 1)
			(fromIntegral y1 * scaleY + 1)

	-- setup stuff
	renderVertexBuffer 0 vbBorderedRectangle
	renderIndexBuffer nullIndexBuffer
	renderUniformStorage us
	renderProgram borderedRectangleProgram
	-- enable blending only if needed
	renderBlendState $ if w_ fillColor >= 1 && w_ borderColor >= 1 then nullBlendState else blendState

	-- set uniforms
	renderUniform us uX xs
	renderUniform us uY ys
	renderUniform us uFillColor fillColor
	renderUniform us uBorderColor borderColor
	renderUploadUniformStorage us

	-- draw
	renderDraw BORDERED_RECTANGLE_INDEX_COUNT

pattern BORDERED_RECTANGLE_INDEX_COUNT :: Int
pattern BORDERED_RECTANGLE_INDEX_COUNT = 54
