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
	) where

import Flaw.Book
import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Math

data ShadowPipeline d = ShadowPipeline
	{ shadowPipelineFrameBuffer :: !(FrameBufferId d)
	, shadowPipelineShadowDSTT :: !(TextureId d)
	}

newShadowPipeline :: Device d => d -> Int -> Int -> Float -> IO (ShadowPipeline d, IO ())
newShadowPipeline device width height nearDepth = withSpecialBook $ \bk -> do
	-- depth-stencil target
	(shadowDST, shadowDSTT) <- book bk $ createReadableDepthStencilTarget device width height defaultSamplerStateInfo
		{ samplerWrapU = SamplerWrapBorder
		, samplerWrapV = SamplerWrapBorder
		, samplerWrapW = SamplerWrapBorder
		, samplerBorderColor = vecFromScalar nearDepth
		}

	-- shadow framebuffer
	frameBuffer <- book bk $ createFrameBuffer device [] shadowDST

	return ShadowPipeline
		{ shadowPipelineFrameBuffer = frameBuffer
		, shadowPipelineShadowDSTT = shadowDSTT
		}

renderShadowPipelineShadowPass :: Context c d => ShadowPipeline d -> Render c ()
renderShadowPipelineShadowPass ShadowPipeline
	{ shadowPipelineFrameBuffer = frameBuffer
	} = do
	renderFrameBuffer frameBuffer
	renderDepthWrite True
	renderBlendState nullBlendState

shadowPipelineInput
	:: Node Float4x4 -- ^ Transform from eye view space to shadow proj space.
	-> Node Float3 -- ^ View-space position.
	-> Int -- ^ Shadow map sampler index.
	-> (Node Float -> Node Float -> Node Bool) -- ^ Comparison operator.
	-> Program (Node Float)
shadowPipelineInput shadowTransform viewPosition samplerIndex compareOp = do
	-- get depth from shadow map
	shadowCoordH <- temp $ shadowTransform `mul` cvec31 viewPosition 1
	shadowCoord <- temp $ xyz__ shadowCoordH / www__ shadowCoordH
	shadowDepth <- temp $ normalizeSampledDepth $ sample (sampler2Df samplerIndex) (screenToTexture $ xy__ shadowCoord)
	temp $ cast $ compareOp shadowDepth $ z_ shadowCoord
