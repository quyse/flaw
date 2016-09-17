{-|
Module: Flaw.Visual.Pipeline.Shadow
Description: Shadow mapping.
License: MIT
-}

module Flaw.Visual.Pipeline.Shadow
	( ShadowPipeline(..)
	, newShadowPipeline
	, renderShadowPipelineShadowPass
	, shadowPipelineInput
	, ShadowBlurPipeline(..)
	, newShadowBlurPipeline
	, ShadowBlurerESM(..)
	, newShadowBlurerESM
	, renderShadowBlurerESM
	, shadowBlurerESMInput
	) where

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Visual
import Flaw.Visual.ScreenQuad

-- | Sampler state used for sampling shadow values.
-- Uses border for getting "shadowed" values outside of shadow map.
shadowSamplerStateInfo :: Float -> SamplerStateInfo
shadowSamplerStateInfo nearDepth = defaultSamplerStateInfo
	{ samplerWrapU = SamplerWrapBorder
	, samplerWrapV = SamplerWrapBorder
	, samplerWrapW = SamplerWrapBorder
	, samplerBorderColor = vecFromScalar nearDepth
	}

-- | Pipeline for casting shadows.
data ShadowPipeline d = ShadowPipeline
	{ shadowPipelineWidth :: {-# UNPACK #-} !Int
	, shadowPipelineHeight :: {-# UNPACK #-} !Int
	, shadowPipelineShadowFB :: !(FrameBufferId d)
	, shadowPipelineShadowDSTT :: !(TextureId d)
	}

newShadowPipeline :: Device d => d -> Int -> Int -> Float -> IO (ShadowPipeline d, IO ())
newShadowPipeline device width height nearDepth = withSpecialBook $ \bk -> do
	-- depth-stencil target
	(shadowDST, shadowDSTT) <- book bk $ createReadableDepthStencilTarget device width height $ shadowSamplerStateInfo nearDepth
	-- framebuffer
	shadowFB <- book bk $ createFrameBuffer device [] shadowDST

	return ShadowPipeline
		{ shadowPipelineWidth = width
		, shadowPipelineHeight = height
		, shadowPipelineShadowFB = shadowFB
		, shadowPipelineShadowDSTT = shadowDSTT
		}

renderShadowPipelineShadowPass :: Context c d => ShadowPipeline d -> Render c ()
renderShadowPipelineShadowPass ShadowPipeline
	{ shadowPipelineWidth = width
	, shadowPipelineHeight = height
	, shadowPipelineShadowFB = frameBuffer
	} = do
	renderViewport $ Vec4 0 0 width height
	renderFrameBuffer frameBuffer
	renderDepthWrite True
	renderBlendState nullBlendState

-- | Sample shadow depth from sampler and return (depth from shadow map, light-space position z)
shadowPipelineInput
	:: Node Float4x4 -- ^ Transform from eye view space to shadow proj space.
	-> Node Float3 -- ^ View-space position.
	-> Int -- ^ Shadow map sampler index.
	-> Program (Node Float, Node Float)
shadowPipelineInput shadowTransform viewPosition samplerIndex = do
	-- get depth from shadow map
	shadowCoordH <- temp $ shadowTransform `mul` cvec31 viewPosition 1
	shadowCoord <- temp $ xyz__ shadowCoordH / www__ shadowCoordH
	shadowDepth <- temp $ normalizeSampledDepth $ sample (sampler2Df samplerIndex) (screenToTexture $ xy__ shadowCoord)
	return (shadowDepth, z_ shadowCoord)

-- | Pipeline for blurring shadows. Includes two 1-float rendertargets.
data ShadowBlurPipeline d = ShadowBlurPipeline
	{
	  shadowBlurPipelineFB1 :: !(FrameBufferId d)
	, shadowBlurPipelineRTT1 :: !(TextureId d)
	, shadowBlurPipelineFB2 :: !(FrameBufferId d)
	, shadowBlurPipelineRTT2 :: !(TextureId d)
	}

newShadowBlurPipeline :: Device d => d -> Int -> Int -> IO (ShadowBlurPipeline d, IO ())
newShadowBlurPipeline device width height = withSpecialBook $ \bk -> do
	let format = UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelFloat
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = LinearColorSpace
		}

	(rt1, rtt1) <- book bk $ createReadableRenderTarget device width height format $ shadowSamplerStateInfo 0
	(rt2, rtt2) <- book bk $ createReadableRenderTarget device width height format $ (shadowSamplerStateInfo 0)
		{ samplerMinFilter = SamplerLinearFilter
		, samplerMagFilter = SamplerLinearFilter
		}
	fb1 <- book bk $ createFrameBuffer device [rt1] nullDepthStencilTarget
	fb2 <- book bk $ createFrameBuffer device [rt2] nullDepthStencilTarget

	return ShadowBlurPipeline
		{ shadowBlurPipelineFB1 = fb1
		, shadowBlurPipelineRTT1 = rtt1
		, shadowBlurPipelineFB2 = fb2
		, shadowBlurPipelineRTT2 = rtt2
		}

-- | Helper object doing blurring using Exponential Shadow Maps method.
data ShadowBlurerESM d = ShadowBlurerESM
	{ shadowBlurerESMProgram1 :: !(ProgramId d)
	, shadowBlurerESMProgram2 :: !(ProgramId d)
	}

newShadowBlurerESM :: Device d
	=> d -- ^ Device.
	-> Int -- ^ Half count of gaussian taps.
	-> (Node Float -> Node Float) -- ^ Function to convert homogeneous depth to linear depth.
	-> IO (ShadowBlurerESM d, IO ())
newShadowBlurerESM device tapsHalfCount homogeneousToLinear = withSpecialBook $ \bk -> do
	let weights = gaussianWeights tapsHalfCount

	-- first pass converts homogeneous depths to linear and blurs them horizontally
	program1 <- book bk $ createProgram device $ screenQuadProgram $ \screenPositionTexcoord -> do
		let tap weight i =
			exp (homogeneousToLinear $ normalizeSampledDepth $ sampleOffset (sampler2Df 0) (zw__ screenPositionTexcoord) (cvec11 (fromIntegral $ i - tapsHalfCount) 0))
			* (constf weight)
		blurred <- temp $ log $ foldl1 (+) $ zipWith tap weights [0..]
		colorTarget 0 $ vecFromScalar blurred

	-- second pass blurs vertically
	program2 <- book bk $ createProgram device $ screenQuadProgram $ \screenPositionTexcoord -> do
		let tap weight i =
			exp (sampleOffset (sampler2Df 0) (zw__ screenPositionTexcoord) (cvec11 0 (fromIntegral $ i - tapsHalfCount)))
			* (constf weight)
		blurred <- temp $ log $ foldl1 (+) $ zipWith tap weights [0..]
		colorTarget 0 $ vecFromScalar blurred

	return ShadowBlurerESM
		{ shadowBlurerESMProgram1 = program1
		, shadowBlurerESMProgram2 = program2
		}

renderShadowBlurerESM :: Context c d => ShadowPipeline d -> ShadowBlurPipeline d -> ShadowBlurerESM d -> ScreenQuadRenderer d -> Render c ()
renderShadowBlurerESM ShadowPipeline
	{ shadowPipelineWidth = width
	, shadowPipelineHeight = height
	, shadowPipelineShadowDSTT = shadowDSTT
	} ShadowBlurPipeline
	{ shadowBlurPipelineFB1 = fb1
	, shadowBlurPipelineRTT1 = rtt1
	, shadowBlurPipelineFB2 = fb2
	} ShadowBlurerESM
	{ shadowBlurerESMProgram1 = program1
	, shadowBlurerESMProgram2 = program2
	} screenQuadRenderer = renderScope $ do
	renderViewport $ Vec4 0 0 width height
	renderDepthTestFunc DepthTestFuncAlways
	renderDepthWrite False
	renderBlendState nullBlendState
	-- horizontal pass
	renderScope $ do
		renderFrameBuffer fb1
		renderSampler 0 shadowDSTT nullSamplerState
		renderProgram program1
		renderScreenQuad screenQuadRenderer
	-- vertical pass
	renderScope $ do
		renderFrameBuffer fb2
		renderSampler 0 rtt1 nullSamplerState
		renderProgram program2
		renderScreenQuad screenQuadRenderer

shadowBlurerESMInput
	:: Node Float4x4 -- ^ Transform from eye view space to shadow proj space.
	-> Node Float3 -- ^ View-space position.
	-> Int -- ^ Shadow map sampler index.
	-> Program (Node Float)
shadowBlurerESMInput shadowTransform viewPosition samplerIndex = do
	shadowCoordH <- temp $ shadowTransform `mul` cvec31 viewPosition 1
	shadowCoord <- temp $ xyz__ shadowCoordH / www__ shadowCoordH
	shadowDepth <- temp $ sample (sampler2Df samplerIndex) (screenToTexture $ xy__ shadowCoord)
	temp $ max_ 0 $ min_ 1 $ exp ((negate (w_ shadowCoordH) - shadowDepth) * constf 4)
