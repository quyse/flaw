{-|
Module: Flaw.Graphics.Canvas
Description: 2D painting.
License: MIT
-}

{-# LANGUAGE RankNTypes #-}

module Flaw.Graphics.Canvas
	( Canvas
	, CanvasM
	, initCanvas
	, withCanvas
	, drawBorderedRectangle
	) where

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Program
import Flaw.Math

import Control.Monad.Reader
import qualified Data.ByteString.Unsafe as B
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | Canvas contains additional things for 2D rendering.
data Canvas c d = Canvas
	{
	-- | Blend state for usual blending.
	  canvasBlendState :: !(BlendStateId d)
	-- | Function for rendering bordered triangle.
	, canvasRenderBorderedRectangle :: Context c d => Vec4f -> Vec4f -> Vec4f -> Vec4f -> Render c ()
	}

-- | Canvas monad.
type CanvasM c d = ReaderT (Canvas c d) (Render c)

initCanvas :: Context c d => d -> IO (Canvas c d, IO ())
initCanvas device = do
	bk <- newBook

	-- create blend state for usual blending
	blendState <- book bk $ createBlendState device defaultBlendStateInfo

	renderBorderedRectangle <- do
		let vbStride = sizeOf (undefined :: Float) * 10
		let vertices = do
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
		let ic = 3 * 3 * 2 * 3
		let ib = nullIndexBuffer
		vb <- book bk $ withArray (vertices :: [Float]) $ \ptr -> do
			bytes <- B.unsafePackCStringLen (castPtr ptr, vbStride * ic)
			createStaticVertexBuffer device bytes vbStride

		-- shader for rendering bordered rectangles
		ubs <- uniformBufferSlot 0
		-- uniform containing four X coordinates (from left to right)
		uX <- uniform ubs :: IO (Node Vec4f)
		-- uniform containing four Y coordinates (from bottom to top)
		uY <- uniform ubs :: IO (Node Vec4f)
		-- uniform with fill color
		uFillColor <- uniform ubs :: IO (Node Vec4f)
		-- uniform with border color
		uBorderColor <- uniform ubs :: IO (Node Vec4f)
		us <- book bk $ createUniformStorage device ubs
		program <- book bk $ createProgram device $ do
			aX <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
			aY <- attribute 0 16 0 (AttributeVec4 AttributeFloat32)
			aC <- attribute 0 32 0 (AttributeVec2 AttributeFloat32)
			color <- temp (uFillColor * vecFromScalar (x_ aC) + uBorderColor * vecFromScalar (y_ aC))
			rasterize (combineVec (dot aX uX, dot aY uY, constf 0, constf 1)) $ colorTarget 0 color

		return $ \xs ys fillColor borderColor -> do
			-- setup stuff
			renderVertexBuffer 0 vb
			renderIndexBuffer ib
			renderUniformStorage us
			renderProgram program
			-- enable blending only if needed
			renderBlendState $ if w_ fillColor >= 1 && w_ borderColor >= 1 then nullBlendState else blendState

			-- set uniforms
			renderUniform us uX xs
			renderUniform us uY ys
			renderUniform us uFillColor fillColor
			renderUniform us uBorderColor borderColor
			renderUploadUniformStorage us

			-- draw
			renderDraw ic

	let canvas = Canvas
		{ canvasBlendState = blendState
		, canvasRenderBorderedRectangle = renderBorderedRectangle
		}

	return (canvas, freeBook bk)

withCanvas :: Canvas c d -> CanvasM c d a -> Render c a
withCanvas = flip runReaderT

-- | Draw bordered rectangle.
-- Coordinates are integer pixels. Y directed down.
drawBorderedRectangle :: Context c d => Vec4i -> Vec4i -> Vec4f -> Vec4f -> CanvasM c d ()
drawBorderedRectangle xs ys fillColor borderColor = do
	Canvas
		{ canvasRenderBorderedRectangle = renderBorderedRectangle
		} <- ask
	lift $ renderScope $ do
		(xSize, ySize) <- renderGetViewport
		let x = (fmap fromIntegral xs) * xxxx__ (Vec1 $ 2 / fromIntegral xSize) - Vec4 1 1 1 1
		let y = (wzyx__ $ fmap fromIntegral ys) * xxxx__ (Vec1 $ (-2) / fromIntegral ySize) + Vec4 1 1 1 1
		renderBorderedRectangle x y fillColor borderColor
