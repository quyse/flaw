{-|
Module: Flaw.Graphics.Font.Render
Description: Font rendering.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Flaw.Graphics.Font.Render
	( GlyphRenderer
	, GlyphSubpixelMode(..)
	, initGlyphRenderer
	, RenderableFont()
	, createRenderableFont
	, RenderGlyphsM
	, renderGlyphs
	, renderText
	) where

import Codec.Picture
import Control.Monad.Reader
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Font
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Resource

-- | Glyph renderer keeps resources needed to render glyphs on a graphics device.
data GlyphRenderer d = GlyphRenderer
	{ glyphRendererVertexBuffer :: !(VertexBufferId d)
	, glyphRendererIndexBuffer :: !(IndexBufferId d)
	, glyphRendererUniformBuffer :: !(UniformBufferId d)
	, glyphRendererSamplerState :: !(SamplerStateId d)
	, glyphRendererBlendState :: !(BlendStateId d)
	, glyphRendererProgram :: !(ProgramId d)
	, glyphRendererCapacity :: !Int
	, glyphRendererBuffer :: !(VSM.IOVector Vec4f)
	}

-- | Subpixel antialiasing mode.
data GlyphSubpixelMode
	= GlyphSubpixelModeNone
	| GlyphSubpixelModeHorizontalRGB
	| GlyphSubpixelModeHorizontalBGR
	| GlyphSubpixelModeVerticalRGB
	| GlyphSubpixelModeVerticalBGR

initGlyphRenderer :: (Device d, ResourceIO m) => d -> GlyphSubpixelMode -> m (ReleaseKey, GlyphRenderer d)
initGlyphRenderer device subpixelMode = do
	let vbStride = sizeOf (undefined :: Vec4f)
	(rkvb, vb) <- do
		ptr <- liftIO $ newArray
			[ Vec4 0 1 1 0 :: Vec4f
			, Vec4 1 1 0 0
			, Vec4 1 0 0 1
			, Vec4 0 1 1 0
			, Vec4 1 0 0 1
			, Vec4 0 0 1 1
			]
		bytes <- liftIO $ B.unsafePackCStringLen (castPtr ptr, vbStride * 6)
		q <- createStaticVertexBuffer device bytes vbStride
		liftIO $ free ptr
		return q
	let ib = nullIndexBuffer
	let capacity = 256;
	(rkub, ub) <- createUniformBuffer device (capacity * 3 * sizeOf (undefined :: Vec4f))
	(rkss, ss) <- createSamplerState device defaultSamplerStateInfo
		{ samplerMinFilter = SamplerLinearFilter
		, samplerMipFilter = SamplerPointFilter
		, samplerMagFilter = SamplerLinearFilter
		, samplerWrapU = SamplerWrapClamp
		, samplerWrapV = SamplerWrapClamp
		, samplerWrapW = SamplerWrapClamp
		}

	let nonSubpixelBlendStateInfo = defaultBlendStateInfo
		{ blendSourceColor = ColorSourceSrcAlpha
		, blendDestColor = ColorSourceInvSrcAlpha
		, blendColorOperation = BlendOperationAdd
		}
	let subpixelBlendStateInfo = defaultBlendStateInfo
		{ blendSourceColor = ColorSourceSecondSrc
		, blendDestColor = ColorSourceInvSecondSrc
		, blendColorOperation = BlendOperationAdd
		}
	(rkbs, bs) <- createBlendState device $ case subpixelMode of
		GlyphSubpixelModeNone -> nonSubpixelBlendStateInfo
		GlyphSubpixelModeHorizontalRGB -> subpixelBlendStateInfo
		GlyphSubpixelModeHorizontalBGR -> subpixelBlendStateInfo
		GlyphSubpixelModeVerticalRGB -> subpixelBlendStateInfo
		GlyphSubpixelModeVerticalBGR -> subpixelBlendStateInfo

	ubs <- liftIO $ uniformBufferSlot 0
	uPositions <- liftIO (uniformArray capacity ubs :: IO (Node [Vec4f]))
	uTexcoords <- liftIO (uniformArray capacity ubs :: IO (Node [Vec4f]))
	uColors <- liftIO (uniformArray capacity ubs :: IO (Node [Vec4f]))
	(rkProgram, program) <- createProgram device $ do
		aCorner <- attribute 0 0 0 (AttributeVec4 AttributeFloat32)
		position <- temp $ uPositions ! instanceId
		texcoordCoefs <- temp $ uTexcoords ! instanceId
		color <- temp $ uColors ! instanceId
		texcoord <- temp $ combineVec
			( dot (xz__ aCorner) (xz__ texcoordCoefs)
			, dot (yw__ aCorner) (yw__ texcoordCoefs)
			)
		rasterize (combineVec
			( dot (xz__ aCorner) (xz__ position)
			, dot (yw__ aCorner) (yw__ position)
			, constf 0
			, constf 1
			)) $ do
			case subpixelMode of
				GlyphSubpixelModeNone -> colorTarget 0 $ combineVec (xyz__ color, w_ color * sample (sampler2Df 0) texcoord)
				_ -> do
					colorTarget 0 $ combineVec (xyz__ color, constf 1)
					let (dxr, dxg, dxb, dyr, dyg, dyb) = case subpixelMode of
						GlyphSubpixelModeNone -> undefined -- GHC warning defence, meh :(
						GlyphSubpixelModeHorizontalRGB -> (-1, 0, 1, 0, 0, 0)
						GlyphSubpixelModeHorizontalBGR -> (1, 0, -1, 0, 0, 0)
						GlyphSubpixelModeVerticalRGB -> (0, 0, 0, -1, 0, 1)
						GlyphSubpixelModeVerticalBGR -> (0, 0, 0, 1, 0, -1)
					colorTarget 1 $ combineVec
						( w_ color * (sample (sampler2Df 0) $ texcoord
							+ (ddx texcoord) * (vecFromScalar $ constf $ dxr / 3)
							+ (ddy texcoord) * (vecFromScalar $ constf $ dyr / 3))
						, w_ color * (sample (sampler2Df 0) $ texcoord
							+ (ddx texcoord) * (vecFromScalar $ constf $ dxg / 3)
							+ (ddy texcoord) * (vecFromScalar $ constf $ dyg / 3))
						, w_ color * (sample (sampler2Df 0) $ texcoord
							+ (ddx texcoord) * (vecFromScalar $ constf $ dxb / 3)
							+ (ddy texcoord) * (vecFromScalar $ constf $ dyb / 3))
						, constf 1
						)

	buffer <- liftIO $ VSM.new $ capacity * 3

	rk <- registerRelease $ do
		release rkvb
		release rkub
		release rkss
		release rkbs
		release rkProgram

	return (rk, GlyphRenderer
		{ glyphRendererVertexBuffer = vb
		, glyphRendererIndexBuffer = ib
		, glyphRendererUniformBuffer = ub
		, glyphRendererSamplerState = ss
		, glyphRendererBlendState = bs
		, glyphRendererProgram = program
		, glyphRendererCapacity = capacity
		, glyphRendererBuffer = buffer
		})

-- | Runtime data about glyph of particular font.
data RenderableGlyph = RenderableGlyph
	{ renderableGlyphUV :: !Vec4f -- ^ Left-bottom + right-top UV coordinates.
	, renderableGlyphOffset :: !Vec4f -- ^ Offset from pen point to left-bottom + right-top corner, in pixels.
	}

-- | Runtime data about particular font.
data RenderableFont d = RenderableFont
	{ renderableFontTexture :: !(TextureId d)
	, renderableFontGlyphs :: !(V.Vector RenderableGlyph)
	}

createRenderableFont :: (Device d, ResourceIO m) => d -> Glyphs -> m (ReleaseKey, RenderableFont d)
createRenderableFont device Glyphs
	{ glyphsImage = Image
		{ imageWidth = width
		, imageHeight = height
		, imageData = pixels
		}
	, glyphsInfos = infos
	, glyphsScaleX = scaleX
	, glyphsScaleY = scaleY
	} = do
		-- create texture
		let (pixelsPtr, pixelsLen) = VS.unsafeToForeignPtr0 pixels
		pixelsBytes <- liftIO $ B.unsafePackCStringLen (unsafeForeignPtrToPtr $ castForeignPtr pixelsPtr, pixelsLen)
		(rkTexture, textureId) <- createStaticTexture device TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelR
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel8bit
				, textureFormatColorSpace = LinearColorSpace
				}
			, textureCount = 0
			} pixelsBytes
		liftIO $ touchForeignPtr pixelsPtr

		-- create glyphs
		let invSize = xyxy__ $ Vec2 (1 / fromIntegral width) (1 / fromIntegral height)
		let invScale = xyxy__ $ Vec2 (1 / fromIntegral scaleX) (1 / fromIntegral scaleY)
		let f GlyphInfo
			{ glyphWidth = gw
			, glyphHeight = gh
			, glyphLeftTopX = gltx
			, glyphLeftTopY = glty
			, glyphOffsetX = gox
			, glyphOffsetY = goy
			} = RenderableGlyph
			{ renderableGlyphUV = Vec4 ltx (lty + h) (ltx + w) lty * invSize
			, renderableGlyphOffset = Vec4 ox (oy + h) (ox + w) oy * invScale
			} where
			w = fromIntegral gw
			h = fromIntegral gh
			ltx = fromIntegral gltx
			lty = fromIntegral glty
			ox = fromIntegral gox
			oy = fromIntegral goy
		let glyphs = V.map f infos

		return (rkTexture, RenderableFont
			{ renderableFontTexture = textureId
			, renderableFontGlyphs = glyphs
			})

data GlyphToRender = GlyphToRender
	{ glyphToRenderPosition :: !Vec2f
	, glyphToRenderIndex :: !Int
	, glyphToRenderColor :: !Vec4f
	}

type RenderGlyphsM c = ReaderT (GlyphToRender -> Render c ()) (Render c)

-- | Draw glyphs.
renderGlyphs :: Context c d => GlyphRenderer d -> RenderableFont d -> RenderGlyphsM c a -> Render c a
renderGlyphs GlyphRenderer
	{ glyphRendererVertexBuffer = vb
	, glyphRendererIndexBuffer = ib
	, glyphRendererUniformBuffer = ub
	, glyphRendererSamplerState = ss
	, glyphRendererBlendState = bs
	, glyphRendererProgram = program
	, glyphRendererCapacity = capacity
	, glyphRendererBuffer = buffer
	} RenderableFont
	{ renderableFontTexture = textureId
	, renderableFontGlyphs = glyphs
	} m = renderScope $ do

	bufferIndexRef <- liftIO $ newIORef 0

	-- setup stuff
	renderVertexBuffer 0 vb
	renderIndexBuffer ib
	renderUniformBuffer 0 ub
	renderSampler 0 textureId ss
	renderBlendState bs
	renderProgram program

	let flush = do
		count <- liftIO $ readIORef bufferIndexRef
		if count > 0 then do
			-- upload data to uniform buffer
			renderUploadUniformBuffer ub $ \f -> do
				VSM.unsafeWith buffer $ \ptr -> do
					f (castPtr ptr) $ capacity * 3 * sizeOf (undefined :: Vec4f)
			-- render batch
			renderDrawInstanced count 6
		else return ()
		liftIO $ writeIORef bufferIndexRef 0

	(viewportWidth, viewportHeight) <- renderGetViewport
	let viewportScale = xyxy__ $ Vec2 (2 / fromIntegral viewportWidth) ((-2) / fromIntegral viewportHeight)
	let viewportOffset = Vec4 (-1) 1 (-1) 1

	result <- runReaderT m $ \GlyphToRender
		{ glyphToRenderPosition = position
		, glyphToRenderIndex = index
		, glyphToRenderColor = color
		} -> do
		do
			bufferIndex <- liftIO $ readIORef bufferIndexRef
			if bufferIndex >= capacity then flush
			else return ()
		liftIO $ do
			let RenderableGlyph
				{ renderableGlyphUV = uv
				, renderableGlyphOffset = offset
				} = glyphs V.! index
			bufferIndex <- readIORef bufferIndexRef
			VSM.write buffer bufferIndex $ (xyxy__ position + offset) * viewportScale + viewportOffset
			VSM.write buffer (bufferIndex + capacity) uv
			VSM.write buffer (bufferIndex + capacity * 2) color
			writeIORef bufferIndexRef $ bufferIndex + 1

	flush

	return result

-- | Shape text and output it in RenderGlyphsM monad.
renderText :: FontShaper s => s -> T.Text -> Vec2f -> Vec4f -> RenderGlyphsM c ()
renderText shaper text position color = do
	(positionsAndIndices, _advance) <- liftIO $ shapeText shaper text
	addGlyph <- ask
	forM_ positionsAndIndices $ \(glyphPosition, glyphIndex) -> do
		lift $ addGlyph GlyphToRender
			{ glyphToRenderPosition = position + glyphPosition
			, glyphToRenderIndex = glyphIndex
			, glyphToRenderColor = color
			}
