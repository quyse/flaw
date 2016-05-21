{-|
Module: Flaw.Graphics
Description: Graphics abstraction.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}

module Flaw.Graphics
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	, DepthTestFunc(..)
	, Render
	, renderScope
	, renderFrameBuffer
	, renderViewport
	, renderGetViewport
	, renderScissor
	, renderGetScissor
	, renderIntersectScissor
	, renderVertexBuffer
	, renderIndexBuffer
	, renderUniformBuffer
	, renderSampler
	, renderBlendState
	, renderDepthTestFunc
	, renderDepthWrite
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderUploadUniformBuffer
	, renderUploadVertexBuffer
	, renderDraw
	, renderDrawInstanced
	, renderPlay
	, render
	, present
	) where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.Text as T

import Flaw.Exception
import Flaw.Graphics.Blend
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Stack

-- | Class of graphics system.
{- Initialization of graphics system depends on implementation.
There're just a little number of some general routines.
-}
class System s where
	-- | Type for id of graphics device.
	data DeviceId s :: *
	-- | Type for id of display.
	data DisplayId s :: *
	-- | Type for id of display mode.
	data DisplayModeId s :: *
	-- | Get list of graphics devices installed in system.
	getInstalledDevices :: s -> IO ([(DeviceId s, DeviceInfo s)], IO ())
	-- | Create custom display mode (with specified width and height) for specified display.
	createDisplayMode
		:: s
		-> DisplayId s -- ^ Display id.
		-> Int -- ^ Width.
		-> Int -- ^ Height.
		-> IO ((DisplayModeId s, DisplayModeInfo), IO ())

-- | Class of graphics device.
-- Graphics device performs managing of resources.
-- Also it works as a primary context for the device.
class Device d where
	-- | Type for deferred contexts.
	type DeferredContext d :: *
	-- | Type for texture id.
	data TextureId d :: *
	-- | Type for sampler state id.
	data SamplerStateId d :: *
	-- | Type for blend state id.
	data BlendStateId d :: *
	-- | Type for render target id.
	data RenderTargetId d :: *
	-- | Type for depth stencil target id.
	data DepthStencilTargetId d :: *
	-- | Type for framebuffer id.
	data FrameBufferId d :: *
	-- | Type for vertex buffer id.
	data VertexBufferId d :: *
	-- | Type for index buffer id.
	data IndexBufferId d :: *
	-- | Type for program id.
	data ProgramId d :: *
	-- | Type for uniform buffer id.
	data UniformBufferId d :: *

	-- | Null texture.
	nullTexture :: TextureId d
	-- | Null sampler state.
	nullSamplerState :: SamplerStateId d
	-- | Null blend state.
	nullBlendState :: BlendStateId d
	-- | Null depth stencil target.
	nullDepthStencilTarget :: DepthStencilTargetId d
	-- | Null index buffer.
	nullIndexBuffer :: IndexBufferId d
	-- | Null uniform buffer.
	nullUniformBuffer :: UniformBufferId d

	-- | Create deferred context.
	createDeferredContext :: Context (DeferredContext d) d => d -> IO (DeferredContext d, IO ())
	createDeferredContext _ = throwIO $ DescribeFirstException "creating deferred context is not supported"
	-- | Create static texture.
	createStaticTexture :: d -> TextureInfo -> SamplerStateInfo -> B.ByteString -> IO (TextureId d, IO ())
	-- | Create texture from image packed in any format natively supported by device.
	createNativeTexture :: d -> SamplerStateInfo -> B.ByteString -> IO (TextureId d, IO ())
	createNativeTexture _ _ _ = throwIO $ DescribeFirstException "creating native texture is not supported"
	-- | Create sampler state.
	createSamplerState :: d -> SamplerStateInfo -> IO (SamplerStateId d, IO ())
	-- | Create blend state.
	createBlendState :: d -> BlendStateInfo -> IO (BlendStateId d, IO ())
	-- | Create readable render target.
	createReadableRenderTarget :: d -> Int -> Int -> TextureFormat -> SamplerStateInfo -> IO ((RenderTargetId d, TextureId d), IO ())
	-- | Create depth stencil target.
	createDepthStencilTarget :: d -> Int -> Int -> IO (DepthStencilTargetId d, IO ())
	-- | Create readable depth stencil target.
	createReadableDepthStencilTarget :: d -> Int -> Int -> IO ((DepthStencilTargetId d, TextureId d), IO ())
	-- | Create framebuffer.
	createFrameBuffer :: d -> [RenderTargetId d] -> DepthStencilTargetId d -> IO (FrameBufferId d, IO ())
	-- | Create static vertex buffer.
	createStaticVertexBuffer
		:: d -- ^ Device.
		-> B.ByteString -- ^ Buffer.
		-> Int -- ^ Stride in bytes.
		-> IO (VertexBufferId d, IO ())
	-- | Create dynamic vertex buffer.
	createDynamicVertexBuffer
		:: d -- ^ Device.
		-> Int -- ^ Size in bytes.
		-> Int -- ^ Stride in bytes.
		-> IO (VertexBufferId d, IO ())
	-- | Create index buffer.
	createStaticIndexBuffer :: d -> B.ByteString -> Bool -> IO (IndexBufferId d, IO ())
	-- | Create program.
	createProgram
		:: d -- ^ Device.
		-> Program () -- ^ Program contents.
		-> IO (ProgramId d, IO ())
	-- | Create uniform buffer.
	createUniformBuffer :: d -> Int -> IO (UniformBufferId d, IO ())

-- | Class of graphics context.
-- Performs actual render operations.
class Device d => Context c d | c -> d where
	------- Immediate commands.
	-- | Clear render target.
	contextClearColor :: c -> Int -> Float4 -> IO ()
	-- | Clear depth.
	contextClearDepth :: c -> Float -> IO ()
	-- | Clear stencil.
	contextClearStencil :: c -> Int -> IO ()
	-- | Clear depth and stencil.
	contextClearDepthStencil :: c -> Float -> Int -> IO ()
	-- | Upload data to uniform buffer.
	contextUploadUniformBuffer :: c -> UniformBufferId d -> B.ByteString -> IO ()
	-- | Upload data to dynamic vertex buffer.
	contextUploadVertexBuffer :: c -> VertexBufferId d -> B.ByteString -> IO ()
	-- | Draw (instanced).
	contextDraw :: c
		-> Int -- ^ Instances count (1 for non-instanced).
		-> Int -- ^ Indices count.
		-> IO ()
	-- | Replay deferred context on immediate context.
	contextPlay :: Context dc d => c -> dc -> IO ()
	contextPlay _ _ = throwIO $ DescribeFirstException "playing deferred context is not supported"
	-- | Perform offscreen rendering. Initial state is context's default state.
	contextRender :: c -> IO a -> IO a
	------- Setup commands.
	-- | Set framebuffer.
	contextSetFrameBuffer :: c -> FrameBufferId d -> IO a -> IO a
	-- | Set viewport (left, top, right, bottom).
	contextSetViewport :: c -> Int4 -> IO a -> IO a
	-- | Get current viewport.
	contextGetViewport :: c -> IO Int4
	-- | Set scissor (left, top, right, bottom).
	contextSetScissor :: c -> Maybe Int4 -> IO a -> IO a
	-- | Get current scissor.
	contextGetScissor :: c -> IO (Maybe Int4)
	-- | Set vertex buffer.
	contextSetVertexBuffer :: c -> Int -> VertexBufferId d -> IO a -> IO a
	-- | Set index buffer.
	contextSetIndexBuffer :: c -> IndexBufferId d -> IO a -> IO a
	-- | Set uniform buffer.
	contextSetUniformBuffer :: c -> Int -> UniformBufferId d -> IO a -> IO a
	-- | Set sampler.
	contextSetSampler :: c -> Int -> TextureId d -> SamplerStateId d -> IO a -> IO a
	-- | Set blend state.
	contextSetBlendState :: c -> BlendStateId d -> IO a -> IO a
	-- | Set depth-test function.
	contextSetDepthTestFunc :: c -> DepthTestFunc -> IO a -> IO a
	-- | Set depth write flag.
	contextSetDepthWrite :: c -> Bool -> IO a -> IO a
	-- | Set program.
	contextSetProgram :: c -> ProgramId d -> IO a -> IO a

-- | Presenter class.
class (System s, Context c d) => Presenter p s c d | p -> s c d where
	setPresenterMode :: p -> Maybe (DisplayModeId s) -> IO ()
	-- | Perform rendering on presenter's surface.
	-- Presenter's framebuffer, viewport, etc will be automatically set
	-- as an initial state.
	presenterRender :: p -> c -> IO a -> IO a

-- | Device information structure.
data DeviceInfo device = DeviceInfo
	{ deviceName :: !T.Text
	, deviceDisplays :: [(DisplayId device, DisplayInfo device)]
	}

-- | Display information structure.
data DisplayInfo device = DisplayInfo
	{ displayName :: !T.Text
	, displayModes :: [(DisplayModeId device, DisplayModeInfo)]
	}

-- | Display mode information structure.
data DisplayModeInfo = DisplayModeInfo
	{ displayModeName :: !T.Text
	, displayModeWidth :: !Int
	, displayModeHeight :: !Int
	, displayModeRefreshRate :: !Rational
	} deriving Show

-- | Depth test function.
data DepthTestFunc
	= DepthTestFuncNever
	| DepthTestFuncLess
	| DepthTestFuncLessOrEqual
	| DepthTestFuncEqual
	| DepthTestFuncNonEqual
	| DepthTestFuncGreaterOrEqual
	| DepthTestFuncGreater
	| DepthTestFuncAlways
	deriving Eq

-- | Rendering monad.
type Render c = StackT (ReaderT c IO)

renderSetup :: (forall a. c -> IO a -> IO a) -> Render c ()
renderSetup setup = StackT $ \q -> do
	c <- ask
	mapReaderT (setup c) $ q ()

renderAction :: (c -> IO a) -> Render c a
renderAction action = StackT $ \q -> do
	c <- ask
	lift (action c) >>= q

-- | Scope for rendering state.
-- Context state will be restored after the scope.
renderScope :: Render c a -> Render c a
renderScope = scope

-- | Set current framebuffer.
renderFrameBuffer :: Context c d => FrameBufferId d -> Render c ()
renderFrameBuffer fb = renderSetup $ \c q -> contextSetFrameBuffer c fb q

-- | Set current viewport (vector with left, top, right, bottom).
renderViewport :: Context c d => Int4 -> Render c ()
renderViewport viewport = renderSetup $ \c q -> contextSetViewport c viewport q

-- | Get current viewport.
renderGetViewport :: Context c d => Render c Int4
renderGetViewport = renderAction contextGetViewport

-- | Set current scissor (vector with left, top, right, bottom).
renderScissor :: Context c d => Maybe Int4 -> Render c ()
renderScissor scissor = renderSetup $ \c q -> contextSetScissor c scissor q

-- | Get current scissor.
renderGetScissor :: Context c d => Render c (Maybe Int4)
renderGetScissor = renderAction contextGetScissor

-- | Set intersection between specified and current scissor as scissor.
renderIntersectScissor :: Context c d => Int4 -> Render c ()
renderIntersectScissor scissor@(Vec4 left top right bottom) = do
	currentScissor <- renderGetScissor
	renderScissor $ Just $ case currentScissor of
		Just (Vec4 currentLeft currentTop currentRight currentBottom) ->
			Vec4 (max left currentLeft) (max top currentTop) (min right currentRight) (min bottom currentBottom)
		Nothing -> scissor

-- | Set vertex buffer.
renderVertexBuffer :: Context c d => Int -> VertexBufferId d -> Render c ()
renderVertexBuffer i vb = renderSetup $ \c q -> contextSetVertexBuffer c i vb q

-- | Set current index buffer.
renderIndexBuffer :: Context c d => IndexBufferId d -> Render c ()
renderIndexBuffer ib = renderSetup $ \c q -> contextSetIndexBuffer c ib q

-- | Set uniform buffer.
renderUniformBuffer :: Context c d => Int -> UniformBufferId d -> Render c ()
renderUniformBuffer i ub = renderSetup $ \c q -> contextSetUniformBuffer c i ub q

-- | Set sampler.
renderSampler :: Context c d => Int -> TextureId d -> SamplerStateId d -> Render c ()
renderSampler i t s = renderSetup $ \c q -> contextSetSampler c i t s q

-- | Set blend state.
renderBlendState :: Context c d => BlendStateId d -> Render c ()
renderBlendState b = renderSetup $ \c q -> contextSetBlendState c b q

-- | Set depth test function.
renderDepthTestFunc :: Context c d => DepthTestFunc -> Render c ()
renderDepthTestFunc f = renderSetup $ \c q -> contextSetDepthTestFunc c f q

-- | Set depth write flag.
renderDepthWrite :: Context c d => Bool -> Render c ()
renderDepthWrite f = renderSetup $ \c q -> contextSetDepthWrite c f q

-- | Set current program.
renderProgram :: Context c d => ProgramId d -> Render c ()
renderProgram p = renderSetup $ \c q -> contextSetProgram c p q

-- | Clear render target.
renderClearColor :: Context c d => Int -> Float4 -> Render c ()
renderClearColor i color = renderAction $ \c -> contextClearColor c i color

-- | Clear depth.
renderClearDepth :: Context c d => Float -> Render c ()
renderClearDepth depth = renderAction $ \c -> contextClearDepth c depth

-- | Clear stencil.
renderClearStencil :: Context c d => Int -> Render c ()
renderClearStencil stencil = renderAction $ \c -> contextClearStencil c stencil

-- | Clear depth and stencil.
renderClearDepthStencil :: Context c d => Float -> Int -> Render c ()
renderClearDepthStencil depth stencil = renderAction $ \c -> contextClearDepthStencil c depth stencil

-- | Upload data to uniform buffer.
renderUploadUniformBuffer :: Context c d => UniformBufferId d -> B.ByteString -> Render c ()
renderUploadUniformBuffer ub bytes = renderAction $ \c -> contextUploadUniformBuffer c ub bytes

-- | Upload data to dynamic vertex buffer.
renderUploadVertexBuffer :: Context c d => VertexBufferId d -> B.ByteString -> Render c ()
renderUploadVertexBuffer vb bytes = renderAction $ \c -> contextUploadVertexBuffer c vb bytes

-- | Draw.
renderDraw :: Context c d => Int -> Render c ()
renderDraw = renderDrawInstanced 1

-- | Draw instanced.
renderDrawInstanced :: Context c d => Int -> Int -> Render c ()
renderDrawInstanced instancesCount indicesCount = renderAction $ \c -> contextDraw c instancesCount indicesCount

-- | Play deferred context on immediate context.
renderPlay :: (Context c d, Context dc d) => dc -> Render c ()
renderPlay deferredContext = renderAction $ \c -> contextPlay c deferredContext

-- | Perform offscreen rendering.
render :: Context c d => c -> Render c a -> IO a
render c f = contextRender c $ runReaderT (runStackT f) c

-- | Perform rendering on presenter.
present :: Presenter p s c d => p -> Render c a -> Render c a
present p f = renderAction $ \c -> presenterRender p c $ runReaderT (runStackT f) c
