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
	, renderFrameBuffer
	, renderVertexBuffers
	, renderIndexBuffer
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderDraw
	, renderPlay
	) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Flaw.Graphics.Abstract
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
	getInstalledDevices :: (MonadResource m, MonadBaseControl IO m) => m (ReleaseKey, [(DeviceId s, DeviceInfo s)])
	-- | Create custom display mode (with specified width and height) for specified display.
	createDisplayMode :: (MonadResource m, MonadBaseControl IO m)
		=> DisplayId s -- ^ Display id.
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
	-- | Type for sampler id.
	data SamplerId d :: *
	-- | Type for render target id.
	data RenderTargetId d :: *
	-- | Type for depth stencil target id.
	data DepthStencilTargetId d :: *
	-- | Type for framebuffer id.
	data FrameBufferId d :: *
	-- | Type for vertex layout id.
	data VertexLayoutId d :: *
	-- | Type for vertex buffer id.
	data VertexBufferId d :: *
	-- | Type for index buffer id.
	data IndexBufferId d :: *
	-- | Type for vertex shader id.
	data VertexShaderId d :: *
	-- | Type for pixel shader id.
	data PixelShaderId d :: *
	-- | Type for program id.
	data ProgramId d :: *
	-- | Create deferred context.
	createDeferredContext :: (MonadResource m, MonadBaseControl IO m, Context (DeferredContext d) d) => d -> m (ReleaseKey, DeferredContext d)
	-- | Create static texture.
	createStaticTexture :: (MonadResource m, MonadBaseControl IO m) => d -> TextureInfo -> BS.ByteString -> m (ReleaseKey, TextureId d)
	-- | Create sampler.
	createSampler :: (MonadResource m, MonadBaseControl IO m) => d -> SamplerInfo -> m (ReleaseKey, SamplerId d)
	-- | Create readable render target.
	createReadableRenderTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> TextureFormat -> m (ReleaseKey, RenderTargetId d, TextureId d)
	-- | Create depth stencil target.
	createDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d)
	-- | Create readable depth stencil target.
	createReadableDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d, TextureId d)
	-- | Create framebuffer.
	createFrameBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> [RenderTargetId d] -> Maybe (DepthStencilTargetId d) -> m (ReleaseKey, FrameBufferId d)
	-- | Create vertex layout.
	createVertexLayout :: MonadIO m => d -> VertexLayoutInfo -> m (VertexLayoutId d)
	-- | Create vertex buffer.
	createStaticVertexBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, VertexBufferId d)
	-- | Create index buffer.
	createStaticIndexBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, IndexBufferId d)
	-- | Create vertex shader.
	createVertexShader :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, VertexShaderId d)
	-- | Create pixel shader.
	createPixelShader :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, PixelShaderId d)
	-- | Create program.
	createProgram :: (MonadResource m, MonadBaseControl IO m) => d -> VertexShaderId d -> PixelShaderId d -> m (ReleaseKey, ProgramId d)

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
	-- | Draw something.
	contextDraw :: c -> RenderState d -> IO ()
	-- | Replay deferred context on immediate context.
	contextPlay :: Context dc d => c -> RenderState d -> dc -> IO (RenderState d)

-- | Presenter class.
class Context c d => Presenter p c d | p -> c d where
	-- | Present whatever needed.
	present :: p -> c -> Render c d ()

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

data RenderState d = RenderState
	{ renderStateFrameBuffer :: FrameBufferId d
	, renderStateVertexBuffers :: [VertexBufferId d]
	, renderStateIndexBuffer :: IndexBufferId d
	, renderStateSamplers :: [(TextureId d, SamplerId d)]
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
	contextDraw context renderState
	return (renderState, ())

-- | Play deferred context on immediate context.
renderPlay :: (Context c d, Context dc d) => dc -> Render c d ()
renderPlay deferredContext = Render $ \context renderState -> do
	newRenderState <- contextPlay context renderState deferredContext
	return (newRenderState, ())
