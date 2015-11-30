{-|
Module: Flaw.Graphics.Font.Render
Description: Font rendering.
License: MIT
-}

{-# LANGUAGE RankNTypes #-}

module Flaw.Graphics.Font.Render
	( GlyphRenderer
	, GlyphSubpixelMode(..)
	, initGlyphRenderer
	, RenderableFont(..)
	, createRenderableFont
	, RenderGlyphsM
	, renderGlyphs
	, RenderTextCursorX(..)
	, RenderTextCursorY(..)
	, renderTextRuns
	, renderTextWithScript
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
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Font
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math

-- | Glyph renderer keeps resources needed to render glyphs on a graphics device.
data GlyphRenderer d = GlyphRenderer
	{ glyphRendererVertexBuffer :: !(VertexBufferId d)
	, glyphRendererIndexBuffer :: !(IndexBufferId d)
	, glyphRendererUniformBuffer :: !(UniformBufferId d)
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

initGlyphRenderer :: Device d => d -> GlyphSubpixelMode -> IO (GlyphRenderer d, IO ())
initGlyphRenderer device subpixelMode = do
	bk <- newBook

	let vbStride = sizeOf (undefined :: Vec4f)
	vb <- book bk $ withArray
		[ Vec4 0 1 1 0 :: Vec4f
		, Vec4 1 1 0 0
		, Vec4 1 0 0 1
		, Vec4 0 1 1 0
		, Vec4 1 0 0 1
		, Vec4 0 0 1 1
		] $ \ptr -> do
		bytes <- B.unsafePackCStringLen (castPtr ptr, vbStride * 6)
		createStaticVertexBuffer device bytes vbStride
	let ib = nullIndexBuffer
	let capacity = 256;
	ub <- book bk $ createUniformBuffer device (capacity * 3 * sizeOf (undefined :: Vec4f))

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
	bs <- book bk $ createBlendState device $ case subpixelMode of
		GlyphSubpixelModeNone -> nonSubpixelBlendStateInfo
		GlyphSubpixelModeHorizontalRGB -> subpixelBlendStateInfo
		GlyphSubpixelModeHorizontalBGR -> subpixelBlendStateInfo
		GlyphSubpixelModeVerticalRGB -> subpixelBlendStateInfo
		GlyphSubpixelModeVerticalBGR -> subpixelBlendStateInfo

	ubs <- uniformBufferSlot 0
	uPositions <- uniformArray capacity ubs :: IO (Node [Vec4f])
	uTexcoords <- uniformArray capacity ubs :: IO (Node [Vec4f])
	uColors <- uniformArray capacity ubs :: IO (Node [Vec4f])
	program <- book bk $ createProgram device $ do
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

	buffer <- VSM.new $ capacity * 3

	return (GlyphRenderer
		{ glyphRendererVertexBuffer = vb
		, glyphRendererIndexBuffer = ib
		, glyphRendererUniformBuffer = ub
		, glyphRendererBlendState = bs
		, glyphRendererProgram = program
		, glyphRendererCapacity = capacity
		, glyphRendererBuffer = buffer
		}, freeBook bk)

-- | Runtime data about glyph of particular font.
data RenderableGlyph = RenderableGlyph
	{ renderableGlyphUV :: !Vec4f -- ^ Left-bottom + right-top UV coordinates.
	, renderableGlyphOffset :: !Vec4f -- ^ Offset from pen point to left-bottom + right-top corner, in pixels.
	}

-- | Runtime data about particular font.
data RenderableFont d = RenderableFont
	{ renderableFontTexture :: !(TextureId d)
	, renderableFontGlyphs :: !(V.Vector RenderableGlyph)
	-- | Maximum glyph box (left, top, right, bottom values relative to pen point, i.e. left-baseline).
	, renderableFontMaxGlyphBox :: !Vec4f
	}

createRenderableFont :: Device d => d -> Glyphs -> IO (RenderableFont d, IO ())
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
	let samplerStateInfo = defaultSamplerStateInfo
		{ samplerMinFilter = SamplerLinearFilter
		, samplerMipFilter = SamplerPointFilter
		, samplerMagFilter = SamplerLinearFilter
		, samplerWrapU = SamplerWrapClamp
		, samplerWrapV = SamplerWrapClamp
		, samplerWrapW = SamplerWrapClamp
		, samplerMaxLod = 0
		}
	(textureId, destroy) <- VS.unsafeWith pixels $ \pixelsPtr -> do
		pixelsBytes <- B.unsafePackCStringLen (castPtr pixelsPtr, VS.length pixels)
		createStaticTexture device TextureInfo
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
			} samplerStateInfo pixelsBytes

	-- create glyphs
	let invSize = xyxy__ $ Vec2 (1 / fromIntegral width) (1 / fromIntegral height)
	let invScale = xyxy__ $ Vec2 (1 / fromIntegral scaleX) (1 / fromIntegral scaleY)
	let createRenderableGlyph GlyphInfo
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
	let glyphs = V.map createRenderableGlyph infos

	-- calculate max glyph box
	let foldGlyphBox GlyphInfo
		{ glyphWidth = gw
		, glyphHeight = gh
		, glyphOffsetX = gox
		, glyphOffsetY = goy
		} (Vec4 left top right bottom) = Vec4
		(min left $ fromIntegral gox)
		(min top $ fromIntegral goy)
		(max right $ fromIntegral (gw + gox))
		(max bottom $ fromIntegral (gh + goy))
	let maxGlyphBox = foldr foldGlyphBox (Vec4 1e8 1e8 (-1e8) (-1e8)) infos

	return (RenderableFont
		{ renderableFontTexture = textureId
		, renderableFontGlyphs = glyphs
		, renderableFontMaxGlyphBox = maxGlyphBox
		}, destroy)

data GlyphToRender = GlyphToRender
	{ glyphToRenderPosition :: !Vec2f
	, glyphToRenderIndex :: !Int
	, glyphToRenderColor :: !Vec4f
	}

data RenderGlyphsState c d = RenderGlyphsState
	{ renderGlyphsStateAddGlyph :: !(GlyphToRender -> Render c ())
	, renderGlyphsStateRenderableFont :: RenderableFont d
	}

type RenderGlyphsM c d = ReaderT (RenderGlyphsState c d) (Render c)

-- | Draw glyphs.
renderGlyphs :: Context c d => GlyphRenderer d -> RenderableFont d -> RenderGlyphsM c d a -> Render c a
renderGlyphs GlyphRenderer
	{ glyphRendererVertexBuffer = vb
	, glyphRendererIndexBuffer = ib
	, glyphRendererUniformBuffer = ub
	, glyphRendererBlendState = bs
	, glyphRendererProgram = program
	, glyphRendererCapacity = capacity
	, glyphRendererBuffer = buffer
	} renderableFont@RenderableFont
	{ renderableFontTexture = textureId
	, renderableFontGlyphs = glyphs
	} m = renderScope $ do

	bufferIndexRef <- liftIO $ newIORef 0

	-- setup stuff
	renderVertexBuffer 0 vb
	renderIndexBuffer ib
	renderUniformBuffer 0 ub
	renderSampler 0 textureId nullSamplerState
	renderBlendState bs
	renderProgram program

	let flush = do
		count <- liftIO $ readIORef bufferIndexRef
		if count > 0 then do
			-- upload data to uniform buffer
			let (foreignPtr, len) = VSM.unsafeToForeignPtr0 buffer
			bytes <- liftIO $ B.unsafePackCStringLen (castPtr $ unsafeForeignPtrToPtr foreignPtr, len * sizeOf (undefined :: Vec4f))
			renderUploadUniformBuffer ub bytes
			liftIO $ touchForeignPtr foreignPtr
			-- render batch
			renderDrawInstanced count 6
		else return ()
		liftIO $ writeIORef bufferIndexRef 0

	Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
	let viewportScale = xyxy__ $ Vec2 (2 / fromIntegral (viewportRight - viewportLeft)) (2 / fromIntegral (viewportTop - viewportBottom))
	let viewportOffset = Vec4 (-1) 1 (-1) 1

	let addGlyph GlyphToRender
		{ glyphToRenderPosition = position
		, glyphToRenderIndex = index
		, glyphToRenderColor = color
		} = do
		do
			bufferIndex <- liftIO $ readIORef bufferIndexRef
			when (bufferIndex >= capacity) flush
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

	result <- runReaderT m RenderGlyphsState
		{ renderGlyphsStateAddGlyph = addGlyph
		, renderGlyphsStateRenderableFont = renderableFont
		}

	flush

	return result

data RenderTextCursorX = RenderTextCursorLeft | RenderTextCursorCenter | RenderTextCursorRight

data RenderTextCursorY = RenderTextCursorBaseline | RenderTextCursorTop | RenderTextCursorMiddle | RenderTextCursorBottom

-- | Shape multiple text runs and output it in RenderGlyphsM monad.
renderTextRuns :: FontShaper s => s -> [(T.Text, Vec4f)] -> FontScript -> Vec2f -> RenderTextCursorX -> RenderTextCursorY -> RenderGlyphsM c d ()
renderTextRuns shaper textsWithColors script (Vec2 px py) cursorX cursorY = do
	(runsPositionsAndIndices, Vec2 ax _ay) <- liftIO $ shapeText shaper (map fst textsWithColors) script
	RenderGlyphsState
		{ renderGlyphsStateAddGlyph = addGlyph
		, renderGlyphsStateRenderableFont = RenderableFont
			{ renderableFontMaxGlyphBox = Vec4 _boxLeft boxTop _boxRight boxBottom
			}
		} <- ask
	let x = case cursorX of
		RenderTextCursorLeft -> px
		RenderTextCursorCenter -> px - ax * 0.5
		RenderTextCursorRight -> px - ax
	let y = case cursorY of
		RenderTextCursorBaseline -> py
		RenderTextCursorTop -> py - boxTop
		RenderTextCursorMiddle -> py - (boxTop + boxBottom) * 0.5
		RenderTextCursorBottom -> py - boxBottom
	let position = Vec2 x y
	forM_ (zip runsPositionsAndIndices $ map snd textsWithColors) $ \(positionsAndIndices, color) -> forM_ positionsAndIndices $ \(glyphPosition, glyphIndex) -> do
		lift $ addGlyph GlyphToRender
			{ glyphToRenderPosition = position + glyphPosition
			, glyphToRenderIndex = glyphIndex
			, glyphToRenderColor = color
			}

-- | Simpler method for rendering text without context.
renderTextWithScript :: FontShaper s => s -> T.Text -> FontScript -> Vec2f -> RenderTextCursorX -> RenderTextCursorY -> Vec4f -> RenderGlyphsM c d ()
renderTextWithScript shaper text script position cursorX cursorY color = renderTextRuns shaper [(text, color)] script position cursorX cursorY

-- | Simpler method for rendering text with unspecified script.
renderText :: FontShaper s => s -> T.Text -> Vec2f -> RenderTextCursorX -> RenderTextCursorY -> Vec4f -> RenderGlyphsM c d ()
renderText shaper text position cursorX cursorY color = renderTextWithScript shaper text fontScriptUnknown position cursorX cursorY color
