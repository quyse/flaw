{-|
Module: Flaw.Graphics
Description: General types for graphics.
License: MIT
-}

-- {-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	, Render(..)
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Flaw.Graphics.Abstract
import Flaw.Graphics.Texture

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
	-- | Create readable render target.
	createReadableRenderTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> TextureFormat -> m (ReleaseKey, RenderTargetId d, TextureId d)
	-- | Create depth stencil target.
	createDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d)
	-- | Create readable depth stencil target.
	createReadableDepthStencilTarget :: (MonadResource m, MonadBaseControl IO m) => d -> Int -> Int -> m (ReleaseKey, DepthStencilTargetId d, TextureId d)
	-- | Create vertex layout.
	createVertexLayout :: (MonadResource m, MonadBaseControl IO m) => d -> VertexLayoutInfo -> m (ReleaseKey, VertexLayoutId d)
	-- | Create vertex buffer.
	createVertexBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> VertexLayoutId d -> BS.ByteString -> m (ReleaseKey, VertexBufferId d)
	-- | Create vertex shader.
	createVertexShader :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, VertexShaderId d)
	-- | Create pixel shader.
	createPixelShader :: (MonadResource m, MonadBaseControl IO m) => d -> BS.ByteString -> m (ReleaseKey, PixelShaderId d)
	-- | Create program.
	createProgram :: (MonadResource m, MonadBaseControl IO m) => d -> VertexShaderId d -> PixelShaderId d -> m (ReleaseKey, ProgramId d)

-- | Class of graphics context.
-- Performs actual render operations.
class Device d => Context c d | c -> d where
	-- | Render something.
	render :: c -> Render c d a -> IO a
	-- | Replay deferred context on this context.
	-- Probably will work only with immediate context!
	playContext :: Context dc d => c -> dc -> IO ()
	-- | Set current framebuffer.
	setContextFrameBuffer :: c -> FrameBufferId d -> IO ()
	-- | Set current program.
	setContextProgram :: c -> ProgramId d -> IO ()
	-- | Draw something.
	draw :: c -> IO ()

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
newtype Render c d a = Render (c -> IO a)

instance Functor (Render c d) where
	fmap f (Render h) = Render $ \c -> liftM f $ h c

instance Applicative (Render c d) where
	pure a = Render $ \_c -> return a
	(Render f) <*> (Render h) = Render $ \c -> f c <*> h c

instance Monad (Render c d) where
	return a = Render $ \_c -> return a
	(Render h) >>= f = Render $ \c -> do
		r <- h c
		let Render q = f r
		q c

instance MonadBase IO (Render c d) where
	liftBase io = Render $ \_ -> io

instance Context c d => MonadBaseControl IO (Render c d) where
	type StM (Render c d) a = c -> IO a
	liftBaseWith f = Render $ \_c -> f $ \(Render q) -> return q
	restoreM = Render
