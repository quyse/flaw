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
	, drawCubicBezierCurve
	) where

import qualified Data.ByteString.Unsafe as B
import Data.Default
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Program
import Flaw.Math

-- | Canvas contains additional things for 2D rendering.
data Canvas d = Canvas
	{
	-- | Blend state for usual blending.
	  canvasBlendState :: !(BlendStateId d)
	, canvasUs :: !(UniformStorage d)
	, canvasUX :: !(Node Float4)
	, canvasUY :: !(Node Float4)
	, canvasUFillColor :: !(Node Float4)
	, canvasUBorderColor :: !(Node Float4)
	, canvasUScaleParams :: !(Node Float4)
	, canvasVbBorderedRectangle :: !(VertexBufferId d)
	, canvasBorderedRectangleProgram :: !(ProgramId d)
	, canvasVbCubicBezierCurve :: !(VertexBufferId d)
	, canvasCubicBezierCurveProgram :: !(ProgramId d)
	}

initCanvas :: Device d => d -> IO (Canvas d, IO ())
initCanvas device = withSpecialBook $ \bk -> do

	-- create blend state for usual blending
	blendState <- book bk $ createBlendState device def
		{ blendSourceColor = ColorSourceSrcAlpha
		, blendDestColor = ColorSourceInvSrcAlpha
		}

	ubs <- uniformBufferSlot 0
	-- uniform containing four X coordinates (from left to right)
	uX <- uniform ubs
	-- uniform containing four Y coordinates (from bottom to top)
	uY <- uniform ubs
	-- uniform with fill color
	uFillColor <- uniform ubs
	-- uniform with border color
	uBorderColor <- uniform ubs
	-- uniform with scale params
	let uScaleParams = uBorderColor -- same uniform
	us <- book bk $ createUniformStorage device ubs

	-- bordered rectangle
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
	borderedRectangleProgram <- book bk $ createProgram device $ do
		aX <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
		aY <- attribute 0 16 0 (AttributeVec4 AttributeFloat32)
		aC <- attribute 0 32 0 (AttributeVec2 AttributeFloat32)
		color <- temp (uFillColor * vecFromScalar (x_ aC) + uBorderColor * vecFromScalar (y_ aC))
		rasterize (cvec1111 (dot aX uX) (dot aY uY) (constf 0) (constf 1)) $ colorTarget 0 color

	-- cubic bezier curve
	vbCubicBezierCurve <- do
		let
			vbStride = sizeOf (undefined :: Float) * 8
			vertices = do
				i <- [0 .. (CUBIC_BEZIER_PIECES_COUNT - 1)]
				(l, k) <- [(1, -1), (0, -1), (0, 1), (1, -1), (0, 1), (1, 1)] -- 6 vertices per quad
				let
					t = fromIntegral (i + l) * (1 / fromIntegral CUBIC_BEZIER_PIECES_COUNT)
				let r =
					-- coefs for bezier point
					[ (1 - t) * (1 - t) * (1 - t)
					, (1 - t) * (1 - t) * t * 3
					, (1 - t) * t * t * 3
					, t * t * t
					-- coefs for bezier tangent (derivative)
					, ((-3) + t * 6 - t * t * 3) * k
					, (3 - t * 12 + t * t * 9) * k
					, (t * 6 - t * t * 9) * k
					, (t * t * 3) * k
					]
				r
		book bk $ withArray (vertices :: [Float]) $ \ptr -> do
			bytes <- B.unsafePackCStringLen (castPtr ptr, vbStride * CUBIC_BEZIER_INDEX_COUNT)
			createStaticVertexBuffer device bytes vbStride
	cubicBezierCurveProgram <- book bk $ createProgram device $ do
		aP <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
		aQ <- attribute 0 16 0 (AttributeVec4 AttributeFloat32)
		p <- temp $
			( cvec11 (dot uX aP) (dot uY aP)
			+ normalize (cvec11 (dot uY aQ) (-(dot uX aQ))) * zz__ uScaleParams
			) * xy__ uScaleParams
		rasterize (cvec211 p 0 1) $ colorTarget 0 uFillColor

	return Canvas
		{ canvasBlendState = blendState
		, canvasUs = us
		, canvasUX = uX
		, canvasUY = uY
		, canvasUFillColor = uFillColor
		, canvasUBorderColor = uBorderColor
		, canvasUScaleParams = uScaleParams
		, canvasVbBorderedRectangle = vbBorderedRectangle
		, canvasBorderedRectangleProgram = borderedRectangleProgram
		, canvasVbCubicBezierCurve = vbCubicBezierCurve
		, canvasCubicBezierCurveProgram = cubicBezierCurveProgram
		}

-- | Draw bordered rectangle.
-- Coordinates are integer pixels. Y directed down.
drawBorderedRectangle :: Context c d => Canvas d -> Int4 -> Int4 -> Float4 -> Float4 -> Render c ()
drawBorderedRectangle Canvas
	{ canvasBlendState = blendState
	, canvasUs = us
	, canvasUX = uX
	, canvasUY = uY
	, canvasUFillColor = uFillColor
	, canvasUBorderColor = uBorderColor
	, canvasVbBorderedRectangle = vbBorderedRectangle
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

drawCubicBezierCurve :: Context c d => Canvas d -> Int4 -> Int4 -> Float4 -> Float -> Render c ()
drawCubicBezierCurve Canvas
	{ canvasBlendState = blendState
	, canvasUs = us
	, canvasUX = uX
	, canvasUY = uY
	, canvasUFillColor = uFillColor
	, canvasUScaleParams = uScaleParams
	, canvasVbCubicBezierCurve = vbCubicBezierCurve
	, canvasCubicBezierCurveProgram = cubicBezierCurveProgram
	} (Int4 x1 x2 x3 x4) (Int4 y1 y2 y3 y4) color thickness = renderScope $ do

	Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
	let
		viewportHalfWidth = fromIntegral (viewportRight - viewportLeft) * 0.5
		viewportHalfHeight = fromIntegral (viewportTop - viewportBottom) * 0.5
		scaleX = 1 / viewportHalfWidth
		scaleY = 1 / viewportHalfHeight
		xs = Float4
			(fromIntegral x1 - viewportHalfWidth)
			(fromIntegral x2 - viewportHalfWidth)
			(fromIntegral x3 - viewportHalfWidth)
			(fromIntegral x4 - viewportHalfWidth)
		ys = Float4
			(fromIntegral y1 + viewportHalfHeight)
			(fromIntegral y2 + viewportHalfHeight)
			(fromIntegral y3 + viewportHalfHeight)
			(fromIntegral y4 + viewportHalfHeight)

	-- setup stuff
	renderVertexBuffer 0 vbCubicBezierCurve
	renderIndexBuffer nullIndexBuffer
	renderUniformStorage us
	renderProgram cubicBezierCurveProgram
	-- enable blending only if needed
	renderBlendState $ if w_ color >= 1 then nullBlendState else blendState

	-- set uniforms
	renderUniform us uX xs
	renderUniform us uY ys
	renderUniform us uFillColor color
	renderUniform us uScaleParams (Vec4 scaleX scaleY thickness 0)
	renderUploadUniformStorage us

	-- draw
	renderDraw CUBIC_BEZIER_INDEX_COUNT


pattern BORDERED_RECTANGLE_INDEX_COUNT :: Int
pattern BORDERED_RECTANGLE_INDEX_COUNT = 54

pattern CUBIC_BEZIER_PIECES_COUNT :: Int
pattern CUBIC_BEZIER_PIECES_COUNT = 32
pattern CUBIC_BEZIER_INDEX_COUNT :: Int
pattern CUBIC_BEZIER_INDEX_COUNT = 192 -- CUBIC_BEZIER_PIECES_COUNT * 6
