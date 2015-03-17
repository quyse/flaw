{-|
Module: Flaw.Graphics.Internal
Description: General types for graphics.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.Internal
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	, Render(..)
	, RenderState(..)
	, renderScope
	, renderDesire
	, renderFrameBuffer
	, renderViewport
	, renderVertexBuffers
	, renderIndexBuffer
	, renderUniformBuffers
	, renderSamplers
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderDraw
	, renderPlay
	, render
	, present
	) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Foreign.Ptr

import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math

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
	-- | Clear render target.
	contextClearColor :: c -> RenderState d -> Int -> Vec4f -> IO ()
	-- | Clear depth.
	contextClearDepth :: c -> RenderState d -> Float -> IO ()
	-- | Clear stencil.
	contextClearStencil :: c -> RenderState d -> Int -> IO ()
	-- | Clear depth and stencil.
	contextClearDepthStencil :: c -> RenderState d -> Float -> Int -> IO ()
	-- | Upload contents of uniform buffer.
	contextUploadUniformBuffer :: c -> UniformBufferId d -> Ptr () -> Int -> IO ()
	-- | Draw.
	contextDraw :: c -> RenderState d -> Int -> IO ()
	-- | Replay deferred context on immediate context.
	contextPlay :: Context dc d => c -> RenderState d -> dc -> IO (RenderState d)
	-- | Perform offscreen rendering. Initial state is context's default state.
	contextRender :: c -> (RenderState d -> IO ()) -> IO ()

-- | Presenter class.
class (System s, Context c d) => Presenter p s c d | p -> s c d where
	setPresenterMode :: p -> Maybe (DisplayModeId s) -> IO ()
	-- | Perform rendering on presenter's surface.
	-- Presenter's framebuffer, viewport, etc will be automatically set
	-- as an initial state.
	presenterRender :: p -> c -> (RenderState d -> IO ()) -> IO ()

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
newtype Render c d a = Render (c -> RenderState d -> IO (RenderState d, a))

instance Functor (Render c d) where
	fmap f (Render h) = Render $ \c s0 -> do
		(s1, r) <- h c s0
		return (s1, f r)

instance Applicative (Render c d) where
	pure a = Render $ \_c s -> return (s, a)
	(Render f) <*> (Render h) = Render $ \c s0 -> do
		(s1, r1) <- h c s0
		(s2, r2) <- f c s1
		return (s2, r2 r1)

instance Monad (Render c d) where
	return a = Render $ \_c s -> return (s, a)
	(Render h) >>= f = Render $ \c s0 -> do
		(s1, r1) <- h c s0
		let Render q = f r1
		q c s1

instance MonadIO (Render c d) where
	liftIO io = Render $ \_c s -> do
		r <- io
		return (s, r)

data RenderState d = RenderState
	{ renderStateFrameBuffer :: FrameBufferId d
	, renderStateViewport :: (Int, Int)
	, renderStateVertexBuffers :: [VertexBufferId d]
	, renderStateIndexBuffer :: IndexBufferId d
	, renderStateUniformBuffers :: HashMap.HashMap Int (UniformBufferId d)
	, renderStateSamplers :: [(TextureId d, SamplerStateId d)]
	, renderStateProgram :: ProgramId d
	}

-- | Make a render scope, i.e. save context state and restore it after.
renderScope :: Render c d a -> Render c d a
renderScope (Render r) = Render $ \context oldState -> do
	(_newState, result) <- r context oldState
	return (oldState, result)

-- | Set new desired state.
renderDesire :: (RenderState d -> RenderState d) -> Render c d ()
renderDesire f = Render $ \_context renderState -> return (f renderState, ())

-- | Set current framebuffer.
renderFrameBuffer :: FrameBufferId d -> Render c d ()
renderFrameBuffer frameBuffer = renderDesire $ \s -> s
	{ renderStateFrameBuffer = frameBuffer
	}

renderViewport :: Int -> Int -> Render c d ()
renderViewport width height = renderDesire $ \s -> s
	{ renderStateViewport = (width, height)
	}

-- | Set current vertex buffers.
renderVertexBuffers :: [VertexBufferId d] -> Render c d ()
renderVertexBuffers vertexBuffers = renderDesire $ \s -> s
	{ renderStateVertexBuffers = vertexBuffers
	}

-- | Set current index buffer.
renderIndexBuffer ::  IndexBufferId d -> Render c d ()
renderIndexBuffer indexBuffer = renderDesire $ \s -> s
	{ renderStateIndexBuffer = indexBuffer
	}

-- | Set uniform buffers.
renderUniformBuffers :: HashMap.HashMap Int (UniformBufferId d) -> Render c d ()
renderUniformBuffers uniformBuffers = renderDesire $ \s -> s
	{ renderStateUniformBuffers = uniformBuffers
	}

-- | Set samplers.
renderSamplers :: [(TextureId d, SamplerStateId d)] -> Render c d ()
renderSamplers samplers = renderDesire $ \s -> s
	{ renderStateSamplers = samplers
	}

-- | Set current program.
renderProgram :: ProgramId d -> Render c d ()
renderProgram program = renderDesire $ \s -> s
	{ renderStateProgram = program
	}

-- | Clear render target.
renderClearColor :: Context c d => Int -> Vec4f -> Render c d ()
renderClearColor targetIndex color = Render $ \context renderState -> do
	contextClearColor context renderState targetIndex color
	return (renderState, ())

-- | Clear depth.
renderClearDepth :: Context c d => Float -> Render c d ()
renderClearDepth depth = Render $ \context renderState -> do
	contextClearDepth context renderState depth
	return (renderState, ())

-- | Clear stencil.
renderClearStencil :: Context c d => Int -> Render c d ()
renderClearStencil stencil = Render $ \context renderState -> do
	contextClearStencil context renderState stencil
	return (renderState, ())

-- | Clear depth and stencil.
renderClearDepthStencil :: Context c d => Float -> Int -> Render c d ()
renderClearDepthStencil depth stencil = Render $ \context renderState -> do
	contextClearDepthStencil context renderState depth stencil
	return (renderState, ())

-- | Draw.
renderDraw :: Context c d => Int -> Render c d ()
renderDraw indicesCount = Render $ \context renderState -> do
	contextDraw context renderState indicesCount
	return (renderState, ())

-- | Play deferred context on immediate context.
renderPlay :: (Context c d, Context dc d) => dc -> Render c d ()
renderPlay deferredContext = Render $ \context renderState -> do
	newRenderState <- contextPlay context renderState deferredContext
	return (newRenderState, ())

-- | Perform offscreen rendering.
render :: Context c d => c -> Render c d () -> IO ()
render context (Render f) = contextRender context $ \state -> do
	_ <- f context state
	return ()

-- | Perform rendering on presenter.
present :: Presenter p s c d => p -> Render c d () -> Render c d ()
present presenter (Render f) = Render $ \context state1 -> do
	presenterRender presenter context $ \state2 -> do
		_ <- f context state2
		return ()
	return (state1, ())
