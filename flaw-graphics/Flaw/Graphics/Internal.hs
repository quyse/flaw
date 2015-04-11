{-|
Module: Flaw.Graphics.Internal
Description: General types for graphics.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}

module Flaw.Graphics.Internal
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	, Render
	, renderScope
	, renderFrameBuffer
	, renderViewport
	, renderGetViewport
	, renderVertexBuffer
	, renderIndexBuffer
	, renderUniformBuffer
	, renderSampler
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderUploadUniformBuffer
	, renderDraw
	, renderPlay
	, render
	, present
	) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Foreign.Ptr

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
	getInstalledDevices :: (MonadResource m, MonadBaseControl IO m) => s -> m (ReleaseKey, [(DeviceId s, DeviceInfo s)])
	-- | Create custom display mode (with specified width and height) for specified display.
	createDisplayMode :: (MonadResource m, MonadBaseControl IO m)
		=> s
		-> DisplayId s -- ^ Display id.
		-> Int -- ^ Width.
		-> Int -- ^ Height.
		-> m (ReleaseKey, (DisplayModeId s, DisplayModeInfo))

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
	-- | Null depth stencil target.
	nullDepthStencilTarget :: DepthStencilTargetId d
	-- | Null index buffer.
	nullIndexBuffer :: IndexBufferId d
	-- | Null uniform buffer.
	nullUniformBuffer :: UniformBufferId d

	-- | Create deferred context.
	createDeferredContext :: (MonadResource m, MonadBaseControl IO m, Context (DeferredContext d) d) => d -> m (ReleaseKey, DeferredContext d)
	-- | Create static texture.
	createStaticTexture :: (MonadResource m, MonadBaseControl IO m) => d -> TextureInfo -> BS.ByteString -> m (ReleaseKey, TextureId d)
	-- | Create sampler state.
	createSamplerState :: (MonadResource m, MonadBaseControl IO m) => d -> SamplerStateInfo -> m (ReleaseKey, SamplerStateId d)
	-- | Create readable render target.
	createReadableRenderTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> TextureFormat -> m (ReleaseKey, RenderTargetId d, TextureId d)
	-- | Create depth stencil target.
	createDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d)
	-- | Create readable depth stencil target.
	createReadableDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d, TextureId d)
	-- | Create framebuffer.
	createFrameBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> [RenderTargetId d] -> DepthStencilTargetId d -> m (ReleaseKey, FrameBufferId d)
	-- | Create vertex buffer.
	createStaticVertexBuffer :: (MonadResource m, MonadBaseControl IO m)
		=> d -- ^ Device.
		-> BS.ByteString -- ^ Buffer
		-> Int -- ^ Stride in bytes.
		-> m (ReleaseKey, VertexBufferId d)
	-- | Create index buffer.
	createStaticIndexBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> Bool -> m (ReleaseKey, IndexBufferId d)
	-- | Create program.
	createProgram :: (MonadResource m, MonadBaseControl IO m)
		=> d -- ^ Device.
		-> Program () -- ^ Program contents.
		-> m (ReleaseKey, ProgramId d)
	-- | Create uniform buffer.
	createUniformBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> m (ReleaseKey, UniformBufferId d)

-- | Class of graphics context.
-- Performs actual render operations.
class Device d => Context c d | c -> d where
	------- Immediate commands.
	-- | Clear render target.
	contextClearColor :: c -> Int -> Vec4f -> IO ()
	-- | Clear depth.
	contextClearDepth :: c -> Float -> IO ()
	-- | Clear stencil.
	contextClearStencil :: c -> Int -> IO ()
	-- | Clear depth and stencil.
	contextClearDepthStencil :: c -> Float -> Int -> IO ()
	-- | Upload data to uniform buffer.
	contextUploadUniformBuffer :: c -> UniformBufferId d -> Ptr () -> Int -> IO ()
	-- | Draw.
	contextDraw :: c -> Int -> IO ()
	-- | Replay deferred context on immediate context.
	contextPlay :: Context dc d => c -> dc -> IO ()
	-- | Perform offscreen rendering. Initial state is context's default state.
	contextRender :: c -> IO a -> IO a
	------- Setup commands.
	-- | Set framebuffer.
	contextSetFrameBuffer :: c -> FrameBufferId d -> IO a -> IO a
	-- | Set viewport.
	contextSetViewport :: c -> Int -> Int -> IO a -> IO a
	-- | Get current viewport.
	contextGetViewport :: c -> IO (Int, Int)
	-- | Set vertex buffer.
	contextSetVertexBuffer :: c -> Int -> VertexBufferId d -> IO a -> IO a
	-- | Set index buffer.
	contextSetIndexBuffer :: c -> IndexBufferId d -> IO a -> IO a
	-- | Set uniform buffer.
	contextSetUniformBuffer :: c -> Int -> UniformBufferId d -> IO a -> IO a
	-- | Set sampler.
	contextSetSampler :: c -> Int -> TextureId d -> SamplerStateId d -> IO a -> IO a
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
	{ deviceName :: T.Text
	, deviceDisplays :: [(DisplayId device, DisplayInfo device)]
	}

-- | Display information structure.
data DisplayInfo device = DisplayInfo
	{ displayName :: T.Text
	, displayModes :: [(DisplayModeId device, DisplayModeInfo)]
	}

-- | Display mode information structure.
data DisplayModeInfo = DisplayModeInfo
	{ displayModeName :: T.Text
	, displayModeWidth :: Int
	, displayModeHeight :: Int
	, displayModeRefreshRate :: Rational
	} deriving Show

-- | Rendering monad.
type Render c a = StackT (ReaderT c IO) a

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

renderViewport :: Context c d => Int -> Int -> Render c ()
renderViewport width height = renderSetup $ \c q -> contextSetViewport c width height q

renderGetViewport :: Context c d => Render c (Int, Int)
renderGetViewport = renderAction contextGetViewport

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

-- | Set current program.
renderProgram :: Context c d => ProgramId d -> Render c ()
renderProgram p = renderSetup $ \c q -> contextSetProgram c p q

-- | Clear render target.
renderClearColor :: Context c d => Int -> Vec4f -> Render c ()
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
renderUploadUniformBuffer :: Context c d => UniformBufferId d -> ((Ptr () -> Int -> IO ()) -> IO ()) -> Render c ()
renderUploadUniformBuffer ub f = renderAction $ \c -> f $ contextUploadUniformBuffer c ub

-- | Draw.
renderDraw :: Context c d => Int -> Render c ()
renderDraw indicesCount = renderAction $ \c -> contextDraw c indicesCount

-- | Play deferred context on immediate context.
renderPlay :: (Context c d, Context dc d) => dc -> Render c ()
renderPlay deferredContext = renderAction $ \c -> contextPlay c deferredContext

-- | Perform offscreen rendering.
render :: Context c d => c -> Render c a -> IO a
render c f = contextRender c $ runReaderT (runStackT f) c

-- | Perform rendering on presenter.
present :: Presenter p s c d => p -> Render c a -> Render c a
present p f = renderAction $ \c -> presenterRender p c $ runReaderT (runStackT f) c
