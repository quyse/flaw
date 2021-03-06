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
* view-space normal (RT2.RGB)
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

import Data.Default

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
  , deferredPipelineOpaqueDST :: !(DepthStencilTargetId d)
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
  let
    samplerStateInfo = def
      { samplerWrapU = SamplerWrapClamp
      , samplerWrapV = SamplerWrapClamp
      , samplerWrapW = SamplerWrapClamp
      }

  -- albedo-occlusion RT
  (albedoOcclusionRT, albedoOcclusionRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
    { textureFormatComponents = PixelRGBA
    , textureFormatValueType = PixelUint
    , textureFormatPixelSize = Pixel32bit
    , textureFormatColorSpace = StandardColorSpace
    } samplerStateInfo

  -- material RT
  (materialRT, materialRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
    { textureFormatComponents = PixelRGBA
    , textureFormatValueType = PixelUint
    , textureFormatPixelSize = Pixel32bit
    , textureFormatColorSpace = LinearColorSpace
    } samplerStateInfo

  -- normals RT
  (normalsRT, normalsRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
    { textureFormatComponents = PixelRG
    , textureFormatValueType = PixelUint
    , textureFormatPixelSize = Pixel32bit
    , textureFormatColorSpace = LinearColorSpace
    } samplerStateInfo

  -- depth-stencil target
  (opaqueDST, opaqueDSTT) <- book bk $ createReadableDepthStencilTarget device width height samplerStateInfo

  -- result color RT
  (colorRT, colorRTT) <- book bk $ createReadableRenderTarget device width height UncompressedTextureFormat
    { textureFormatComponents = PixelRGB
    , textureFormatValueType = PixelFloat
    , textureFormatPixelSize = Pixel32bit
    , textureFormatColorSpace = LinearColorSpace
    } samplerStateInfo

  -- framebuffer for opaque pass
  opaquePassFrameBuffer <- book bk $ createFrameBuffer device [colorRT, albedoOcclusionRT, materialRT, normalsRT] opaqueDST
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
    , deferredPipelineOpaqueDST = opaqueDST
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
  :: Node Float3 -- ^ Result color
  -> Node Float4 -- ^ Albedo + occlusion
  -> Node Float4 -- ^ Material
  -> Node Float3 -- ^ View-space normal
  -> Program ()
outputDeferredPipelineOpaquePass color albedoOcclusion material normal = do
  colorTarget 0 $ cvec31 color 0
  colorTarget 1 albedoOcclusion
  colorTarget 2 material
  encodedNormal <- encodeLambertAzimuthalEqualArea normal
  colorTarget 3 $ cvec211 encodedNormal 0 0

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
  normal <- decodeLambertAzimuthalEqualArea =<< temp (sample (sampler2D2f 6) texcoord)
  depth <- temp $ normalizeSampledDepth $ sample (sampler2Df 7) texcoord
  return (albedoOcclusion, material, normal, depth)

-- | Light pass program.
deferredPipelineLightPassProgram
  :: Node Float4x4 -- ^ Inverse projection matrix.
  -> (Node Float3 -> Program (Node Float3, Node Float3)) -- ^ Light program, getting view-space position and returning direction to light and light color
  -> Program ()
deferredPipelineLightPassProgram invProj lightProgram = screenQuadProgram $ \screenPositionTexcoord -> do
  -- fetch data from opaque pass
  (albedoOcclusion, material, viewNormal, depth) <- deferredPipelineLightPassInput $ zw__ screenPositionTexcoord
  let
    albedo = xyz__ albedoOcclusion
    diffuse = x_ material
    specular = y_ material
    metalness = zzz__ material
    glossiness = w_ material

  -- restore view-space position
  viewPositionH <- temp $ invProj `mul` cvec211 (xy__ screenPositionTexcoord) depth 1
  viewPosition <- temp $ xyz__ viewPositionH / www__ viewPositionH

  -- get direction to light and color
  (toLightDirection, lightColor) <- lightProgram viewPosition

  -- half vector
  toEyeLightHalfDirection <- temp $ normalize $ toLightDirection - normalize viewPosition

  -- reflectance
  diffuseReflectance <- temp =<< max_ 0 <$> lambertReflectance viewNormal toLightDirection
  specularReflectance <- temp =<< (diffuseReflectance *) <$> schulerSpecularReflectance viewNormal toEyeLightHalfDirection toLightDirection glossiness

  -- resulting color
  color <- temp $ lightColor * (albedo * vecFromScalar (diffuse * diffuseReflectance) + lerp (vecFromScalar 1) albedo metalness * vecFromScalar (specular * specularReflectance))

  colorTarget 0 $ cvec31 color 0

-- | Ambient light pass program.
deferredPipelineAmbientLightPassProgram
  :: Node Float4x4 -- ^ Inverse projection matrix.
  -> Node Float3 -- ^ Ambient light color.
  -> Program ()
deferredPipelineAmbientLightPassProgram invProj ambientColor = screenQuadProgram $ \screenPositionTexcoord -> do
  -- fetch data from opaque pass
  (albedoOcclusion, material, viewNormal, depth) <- deferredPipelineLightPassInput $ zw__ screenPositionTexcoord
  let
    albedo = xyz__ albedoOcclusion
    diffuse = x_ material
    specular = y_ material
    metalness = zzz__ material
    glossiness = w_ material

  -- restore view-space position
  viewPositionH <- temp $ invProj `mul` cvec211 (xy__ screenPositionTexcoord) depth 1
  viewPosition <- temp $ xyz__ viewPositionH / www__ viewPositionH

  -- reflectance
  specularReflectance <- schulerAmbientReflectance viewNormal (negate $ normalize viewPosition) specular glossiness

  -- resulting color
  color <- temp $ ambientColor * (albedo * vecFromScalar diffuse + lerp (vecFromScalar 1) albedo metalness * vecFromScalar (specular * specularReflectance))

  colorTarget 0 $ cvec31 color 0
