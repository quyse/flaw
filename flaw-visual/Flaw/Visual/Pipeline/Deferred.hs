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

* albedo (RT0.RGB)
* occlusion factor (RT0.A)
* diffuse factor (RT1.R)
* specular factor (RT1.G)
* metalness factor (RT1.B)
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
	, deferredPipelineAmbientLightPassProgram
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
	, deferredPipelineAlbedoOcclusionRTT :: !(TextureId d)
	, deferredPipelineMaterialRTT :: !(TextureId d)
	, deferredPipelineNormalsRTT :: !(TextureId d)
	, deferredPipelineOpaqueDSTT :: !(TextureId d)
	, deferredPipelineColorRTT :: !(TextureId d)
	, deferredPipelineLightPassBlendState :: !(BlendStateId d)
	}

newDeferredPipeline :: Device d => d -> Int -> Int -> IO (DeferredPipeline d, IO ())
newDeferredPipeline device width height = withSpecialBook $ \bk -> do
	-- albedo-occlusion RT
	(albedoOcclusionRT, albedoOcclusionRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = StandardColorSpace
		} defaultSamplerStateInfo

	-- material RT
	(materialRT, materialRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
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
	opaquePassFrameBuffer <- book bk $ createFrameBuffer device [albedoOcclusionRT, materialRT, normalsRT] opaqueDST
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
		, deferredPipelineAlbedoOcclusionRTT = albedoOcclusionRTT
		, deferredPipelineMaterialRTT = materialRTT
		, deferredPipelineNormalsRTT = normalsRTT
		, deferredPipelineOpaqueDSTT = opaqueDSTT
		, deferredPipelineColorRTT = colorRTT
		, deferredPipelineLightPassBlendState = lightPassBlendState
		}

-- | Output result for opaque pass.
outputDeferredPipelineOpaquePass
	:: Node Float4 -- ^ Albedo + occlusion
	-> Node Float4 -- ^ Material
	-> Node Float3 -- ^ World normal
	-> Program ()
outputDeferredPipelineOpaquePass albedoOcclusion material normal = do
	colorTarget 0 albedoOcclusion
	colorTarget 1 material
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
	, deferredPipelineAlbedoOcclusionRTT = albedoOcclusionRTT
	, deferredPipelineMaterialRTT = materialRTT
	, deferredPipelineNormalsRTT = normalsRTT
	, deferredPipelineOpaqueDSTT = opaqueDSTT
	, deferredPipelineLightPassBlendState = lightPassBlendState
	} = do
	renderFrameBuffer lightPassFrameBuffer
	renderSampler 4 albedoOcclusionRTT nullSamplerState
	renderSampler 5 materialRTT nullSamplerState
	renderSampler 6 normalsRTT nullSamplerState
	renderSampler 7 opaqueDSTT nullSamplerState
	renderDepthWrite False
	renderBlendState lightPassBlendState

-- | Shader resources for light pass.
deferredPipelineLightPassInput :: Node Float2 -> Program (Node Float4, Node Float4, Node Float3, Node Float)
deferredPipelineLightPassInput texcoord = do
	albedoOcclusion <- temp $ sample (sampler2D4f 4) texcoord
	material <- temp $ sample (sampler2D4f 5) texcoord
	normal <- temp $ sample (sampler2D3f 6) texcoord * vecFromScalar 2 - vecFromScalar 1
	depth <- temp $ sample (sampler2Df 7) texcoord * 2 - 1
	return (albedoOcclusion, material, normal, depth)

-- | Light pass program.
deferredPipelineLightPassProgram
	:: Node Float3 -- ^ Eye position.
	-> Node Float4x4 -- ^ Inverse view-projection matrix.
	-> (Node Float3 -> Program (Node Float3, Node Float3)) -- ^ Light program, getting world position and returning direction to light and light color
	-> Program ()
deferredPipelineLightPassProgram eyePosition invViewProj lightProgram = screenQuadProgram $ \screenPositionTexcoord -> do
	-- fetch data from opaque pass
	(albedoOcclusion, material, worldNormal, depth) <- deferredPipelineLightPassInput $ zw__ screenPositionTexcoord
	let albedo = xyz__ albedoOcclusion
	let diffuse = x_ material
	let specular = y_ material
	let metalness = zzz__ material
	let glossiness = w_ material

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
	color <- temp $ lightColor * (albedo * vecFromScalar (diffuse * diffuseReflectance) + (lerp (vecFromScalar 1) albedo metalness) * vecFromScalar (specular * specularReflectance))

	colorTarget 0 $ cvec31 color 0

-- | Ambient light pass program.
deferredPipelineAmbientLightPassProgram
	:: Node Float3 -- ^ Eye position.
	-> Node Float4x4 -- ^ Inverse view-projection matrix.
	-> Node Float3 -- ^ Ambient light color.
	-> Program ()
deferredPipelineAmbientLightPassProgram eyePosition invViewProj ambientColor = screenQuadProgram $ \screenPositionTexcoord -> do
	-- fetch data from opaque pass
	(albedoOcclusion, material, worldNormal, depth) <- deferredPipelineLightPassInput $ zw__ screenPositionTexcoord
	let albedo = xyz__ albedoOcclusion
	let diffuse = x_ material
	let specular = y_ material
	let metalness = zzz__ material
	let glossiness = w_ material

	-- restore world position
	worldPositionH <- temp $ invViewProj `mul` (cvec211 (xy__ screenPositionTexcoord) depth 1)
	worldPosition <- temp $ xyz__ worldPositionH / www__ worldPositionH

	-- get direction to eye
	toEyeDirection <- temp $ normalize $ eyePosition - worldPosition

	-- reflectance
	specularReflectance <- schulerAmbientReflectance worldNormal toEyeDirection specular glossiness

	-- resulting color
	color <- temp $ ambientColor * (albedo * vecFromScalar diffuse + (lerp (vecFromScalar 1) albedo metalness) * vecFromScalar (specular * specularReflectance))

	colorTarget 0 $ cvec31 color 0
