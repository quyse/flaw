{-|
Module: Flaw.Visual.Pipeline.Deferred
Description: Deferred rendering pipeline.
License: MIT
-}

module Flaw.Visual.Pipeline.Deferred
	(
-- * Deferred pipeline with hardcoded shading model.
-- -| List of passes:
{-|
Opaque pass renders opaque geometry into couple of RTs and depth target:

* diffuse color (RT0.RGB)
* occlusion factor (RT0.A)
* specular color (RT1.RGB)
* glossiness (RT1.A)
* world normal (RT2.RGB)
* depth

Emission pass re-renders opaque geometry with non-zero emission into light RT.

Light pass renders light volumes (or fullscreen quads), calculate lighting and output resulting color.
-}

	  DeferredPipeline(..)
	, newDeferredPipeline
	, outputDeferredPipelineOpaquePass
	, renderDeferredPipelineOpaquePass
	, renderDeferredPipelineLightPass
	, deferredPipelineLightPassInput
	, deferredPipelineLightPassProgram
	) where

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Visual
import Flaw.Visual.ScreenQuad

data DeferredPipeline d = DeferredPipeline
	{ deferredPipelineOpaquePassFrameBuffer :: !(FrameBufferId d)
	, deferredPipelineLightPassFrameBuffer :: !(FrameBufferId d)
	, deferredPipelineDiffuseOcclusionRTT :: !(TextureId d)
	, deferredPipelineSpecularGlossinessRTT :: !(TextureId d)
	, deferredPipelineNormalsRTT :: !(TextureId d)
	, deferredPipelineOpaqueDSTT :: !(TextureId d)
	, deferredPipelineColorRTT :: !(TextureId d)
	, deferredPipelineLightPassBlendState :: !(BlendStateId d)
	}

newDeferredPipeline :: Device d => d -> Int -> Int -> IO (DeferredPipeline d, IO ())
newDeferredPipeline device width height = withSpecialBook $ \bk -> do
	-- diffuse-occlusion RT
	(diffuseOcclusionRT, diffuseOcclusionRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = LinearColorSpace
		} defaultSamplerStateInfo

	-- specular-glossiness RT
	(specularGlossinessRT, specularGlossinessRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = LinearColorSpace
		} defaultSamplerStateInfo

	-- normals RT
	(normalsRT, normalsRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
		{ textureFormatComponents = PixelRGB
		, textureFormatValueType = PixelFloat
		, textureFormatPixelSize = Pixel96bit
		, textureFormatColorSpace = LinearColorSpace
		} defaultSamplerStateInfo

	-- depth-stencil target
	(opaqueDST, opaqueDSTT) <- book bk $ createReadableDepthStencilTarget device width height

	-- result color RT
	(colorRT, colorRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
		{ textureFormatComponents = PixelRGB
		, textureFormatValueType = PixelFloat
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = LinearColorSpace
		} defaultSamplerStateInfo

	-- framebuffer for opaque pass
	opaquePassFrameBuffer <- book bk $ createFrameBuffer device [diffuseOcclusionRT, specularGlossinessRT, normalsRT] opaqueDST
	-- framebuffer for lighting pass
	lightPassFrameBuffer <- book bk $ createFrameBuffer device [colorRT] nullDepthStencilTarget

	-- light pass blend state (additive blending)
	lightPassBlendState <- book bk $ createBlendState device BlendStateInfo
		{ blendSourceColor = ColorSourceOne
		, blendDestColor = ColorSourceOne
		, blendColorOperation = BlendOperationAdd
		, blendSourceAlpha = AlphaSourceOne
		, blendDestAlpha = AlphaSourceOne
		, blendAlphaOperation = BlendOperationAdd
		}

	return DeferredPipeline
		{ deferredPipelineOpaquePassFrameBuffer = opaquePassFrameBuffer
		, deferredPipelineLightPassFrameBuffer = lightPassFrameBuffer
		, deferredPipelineDiffuseOcclusionRTT = diffuseOcclusionRTT
		, deferredPipelineSpecularGlossinessRTT = specularGlossinessRTT
		, deferredPipelineNormalsRTT = normalsRTT
		, deferredPipelineOpaqueDSTT = opaqueDSTT
		, deferredPipelineColorRTT = colorRTT
		, deferredPipelineLightPassBlendState = lightPassBlendState
		}

-- | Output result for opaque pass.
outputDeferredPipelineOpaquePass
	:: Node Float4 -- ^ Diffuse + occlusion.
	-> Node Float4 -- ^ Specular + glossiness.
	-> Node Float3 -- ^ World normal
	-> Program ()
outputDeferredPipelineOpaquePass diffuseOcclusion specularGlossiness normal = do
	colorTarget 0 diffuseOcclusion
	colorTarget 1 specularGlossiness
	colorTarget 2 $ cvec31 normal 1 * vecFromScalar 0.5 + vecFromScalar 0.5

-- | Set up resources for opaque pass.
renderDeferredPipelineOpaquePass :: Context c d => DeferredPipeline d -> Render c ()
renderDeferredPipelineOpaquePass DeferredPipeline
	{ deferredPipelineOpaquePassFrameBuffer = opaquePassFrameBuffer
	} = do
	renderFrameBuffer opaquePassFrameBuffer
	renderDepthWrite True
	renderBlendState nullBlendState

-- | Set up resources for light pass.
renderDeferredPipelineLightPass :: Context c d => DeferredPipeline d -> Render c ()
renderDeferredPipelineLightPass DeferredPipeline
	{ deferredPipelineLightPassFrameBuffer = lightPassFrameBuffer
	, deferredPipelineDiffuseOcclusionRTT = diffuseOcclusionRTT
	, deferredPipelineSpecularGlossinessRTT = specularGlossinessRTT
	, deferredPipelineNormalsRTT = normalsRTT
	, deferredPipelineOpaqueDSTT = opaqueDSTT
	, deferredPipelineLightPassBlendState = lightPassBlendState
	} = do
	renderFrameBuffer lightPassFrameBuffer
	renderSampler 4 diffuseOcclusionRTT nullSamplerState
	renderSampler 5 specularGlossinessRTT nullSamplerState
	renderSampler 6 normalsRTT nullSamplerState
	renderSampler 7 opaqueDSTT nullSamplerState
	renderDepthWrite False
	renderBlendState lightPassBlendState

-- | Shader resources for light pass.
deferredPipelineLightPassInput :: Node Float2 -> Program (Node Float4, Node Float4, Node Float3, Node Float)
deferredPipelineLightPassInput texcoord = do
	diffuseOcclusion <- temp $ sample (sampler2D4f 4) texcoord
	specularGlossiness <- temp $ sample (sampler2D4f 5) texcoord
	normal <- temp $ sample (sampler2D3f 6) texcoord * vecFromScalar 2 - vecFromScalar 1
	depth <- temp $ sample (sampler2Df 7) texcoord * 2 - 1
	return (diffuseOcclusion, specularGlossiness, normal, depth)

-- | Light pass program.
deferredPipelineLightPassProgram
	:: Node Float3
	-> Node Float4x4 -- ^ Inverse view-projection matrix.
	-> (Node Float3 -> Program (Node Float3, Node Float3)) -- ^ Light program, getting world position and returning direction to light and light color
	-> Program ()
deferredPipelineLightPassProgram eyePosition invViewProj lightProgram = screenQuadProgram $ \screenPositionTexcoord -> do
	-- fetch data from opaque pass
	(diffuseOcclusion, specularGlossiness, worldNormal, depth) <- deferredPipelineLightPassInput $ zw__ screenPositionTexcoord
	let diffuse = xyz__ diffuseOcclusion
	let specular = xyz__ specularGlossiness
	let glossiness = w_ specularGlossiness

	-- restore world position
	worldPositionH <- temp $ invViewProj `mul` (cvec211 (xy__ screenPositionTexcoord) depth 1)
	worldPosition <- temp $ xyz__ worldPositionH / www__ worldPositionH

	-- get direction to light and color
	(toLightDirection, lightColor) <- lightProgram worldPosition

	-- to eye direction and half vector
	toEyeDirection <- temp $ normalize $ eyePosition - worldPosition
	toEyeLightHalfDirection <- temp $ normalize $ toLightDirection + toEyeDirection

	-- reflectance
	diffuseReflectance <- temp =<< (max_ 0) <$> lambertReflectance worldNormal toLightDirection
	specularReflectance <- temp =<< (diffuseReflectance *) <$> schulerSpecularReflectance worldNormal toEyeLightHalfDirection toLightDirection glossiness

	-- resulting color
	color <- temp $ lightColor * (diffuse * vecFromScalar diffuseReflectance + specular * vecFromScalar specularReflectance)

	colorTarget 0 $ cvec31 color 0
