{-|
Module: Flaw.Graphics.Sprite
Description: 2D sprite drawing.
License: MIT
-}

module Flaw.Graphics.Sprite
	( initQuadRenderer
	, RenderQuadsM
	, renderQuads
	, RenderableIcon(..)
	, renderIcon
	, RenderableQuad(..)
	, renderQuad
	) where

import Control.Monad.Reader
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Math

data QuadRenderer d = QuadRenderer
	{ quadRendererVertexBuffer :: !(VertexBufferId d)
	, quadRendererSamplerState :: !(SamplerStateId d)
	, quadRendererBlendState :: !(BlendStateId d)
	, quadRendererProgram :: !(ProgramId d)
	, quadRendererCapacity :: !Int -- ^ Max number of quads.
	, quadRendererBuffer :: !(VSM.IOVector Float4)
	}

initQuadRenderer :: Device d => d -> IO (QuadRenderer d, IO ())
initQuadRenderer device = do
	bk <- newBook

	let capacity = 256;

	-- vertex buffer
	let vbStride = sizeOf (undefined :: Float4)
	vb <- book bk $ createDynamicVertexBuffer device (capacity * 6 * vbStride) vbStride

	ss <- book bk $ createSamplerState device defaultSamplerStateInfo
		{ samplerMinFilter = SamplerPointFilter
		, samplerMipFilter = SamplerPointFilter
		, samplerMagFilter = SamplerPointFilter
		, samplerWrapU = SamplerWrapClamp
		, samplerWrapV = SamplerWrapClamp
		, samplerWrapW = SamplerWrapClamp
		}

	bs <- book bk $ createBlendState device $ defaultBlendStateInfo
		{ blendSourceColor = ColorSourceSrcAlpha
		, blendDestColor = ColorSourceInvSrcAlpha
		, blendColorOperation = BlendOperationAdd
		}

	program <- book bk $ createProgram device $ do
		aVertex <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
		texcoord <- temp $ zw__ aVertex
		rasterize (cvec4 (x_ aVertex) (y_ aVertex) (constf 0) (constf 1)) $ do
			colorTarget 0 $ sample (sampler2D4f 0) texcoord

	buffer <- VSM.new $ capacity * 6

	return (QuadRenderer
		{ quadRendererVertexBuffer = vb
		, quadRendererSamplerState = ss
		, quadRendererBlendState = bs
		, quadRendererProgram = program
		, quadRendererCapacity = capacity
		, quadRendererBuffer = buffer
		}, freeBook bk)

data QuadToRender = QuadToRender
	{ quadToRenderPosition :: !Float4 -- ^ Left-bottom (XY) and right-top (ZW) position in pixels.
	, quadToRenderTexcoord :: !Float4 -- ^ Left-bottom (XY) and right-top (ZW) texcoords.
	}

type RenderQuadsM c = ReaderT (QuadToRender -> Render c ()) (Render c)

-- | Draw quads.
renderQuads :: Context c d => QuadRenderer d -> TextureId d -> RenderQuadsM c a -> Render c a
renderQuads QuadRenderer
	{ quadRendererVertexBuffer = vb
	, quadRendererSamplerState = ss
	, quadRendererBlendState = bs
	, quadRendererProgram = program
	, quadRendererCapacity = capacity
	, quadRendererBuffer = buffer
	} textureId m = renderScope $ do

	bufferIndexRef <- liftIO $ newIORef 0

	-- setup stuff
	renderVertexBuffer 0 vb
	renderIndexBuffer nullIndexBuffer
	renderSampler 0 textureId ss
	renderBlendState bs
	renderProgram program

	let flush = do
		count <- liftIO $ readIORef bufferIndexRef
		if count > 0 then do
			-- upload data to vertex buffer
			let (foreignPtr, _size) = VSM.unsafeToForeignPtr0 buffer
			bytes <- liftIO $ B.unsafePackCStringLen (castPtr $ unsafeForeignPtrToPtr foreignPtr, count * 6 * sizeOf (undefined :: Float4))
			renderUploadVertexBuffer vb bytes
			liftIO $ touchForeignPtr foreignPtr
			-- render batch
			renderDraw $ count * 6
		else return ()
		liftIO $ writeIORef bufferIndexRef 0

	Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
	let viewportScale = xyxy__ $ Vec2 (2 / fromIntegral (viewportRight - viewportLeft)) (2 / fromIntegral (viewportTop - viewportBottom))
	let viewportOffset = Vec4 (-1) 1 (-1) 1

	result <- runReaderT m $ \QuadToRender
		{ quadToRenderPosition = position
		, quadToRenderTexcoord = Vec4 t1x t1y t2x t2y
		} -> do
		do
			bufferIndex <- liftIO $ readIORef bufferIndexRef
			if bufferIndex >= capacity then flush
			else return ()
		liftIO $ do
			let Vec4 p1x p1y p2x p2y = position * viewportScale + viewportOffset
			bufferIndex <- readIORef bufferIndexRef
			VSM.write buffer (bufferIndex * 6 + 0) $ Vec4 p1x p1y t1x t1y
			VSM.write buffer (bufferIndex * 6 + 1) $ Vec4 p2x p1y t2x t1y
			VSM.write buffer (bufferIndex * 6 + 2) $ Vec4 p2x p2y t2x t2y
			VSM.write buffer (bufferIndex * 6 + 3) $ Vec4 p1x p1y t1x t1y
			VSM.write buffer (bufferIndex * 6 + 4) $ Vec4 p2x p2y t2x t2y
			VSM.write buffer (bufferIndex * 6 + 5) $ Vec4 p1x p2y t1x t2y
			writeIORef bufferIndexRef $ bufferIndex + 1

	flush

	return result

-- | Runtime data about one sprite of particular sprite set.
data RenderableIcon = RenderableIcon
	{ renderableIconPosition :: !Float4 -- ^ Left-top and right-bottom position in pixels, relative to main point.
	, renderableIconTexcoord :: !Float4 -- ^ Left-top and right-bottom UV.
	}

renderIcon :: RenderableIcon -> Float2 -> RenderQuadsM c ()
renderIcon RenderableIcon
	{ renderableIconPosition = position
	, renderableIconTexcoord = texcoord
	} point = do
	addQuad <- ask
	lift $ addQuad QuadToRender
		{ quadToRenderPosition = position + xyxy__ point
		, quadToRenderTexcoord = texcoord
		}

data RenderableQuad = RenderableQuad
	{ renderableQuadTexcoord :: !Float4 -- ^ Left-top and right-bottom UV.
	}

renderQuad :: RenderableQuad -> Float2 -> Float2 -> RenderQuadsM c ()
renderQuad RenderableQuad
	{ renderableQuadTexcoord = texcoord
	} (Vec2 left top) (Vec2 right bottom) = do
	addQuad <- ask
	lift $ addQuad QuadToRender
		{ quadToRenderPosition = Vec4 left top right bottom
		, quadToRenderTexcoord = texcoord
		}
