{-|
Module: Flaw.Graphics.Internal
Description: Internal methods of graphics.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.Internal
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, Render(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
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
	-- | Type for vertex layout id.
	data VertexLayoutId d :: *
	-- | Type for vertex buffer id.
	data VertexBufferId d :: *
	-- | Create deferred context.
	createDeferredContext :: (MonadResource m, MonadBaseControl IO m, Context (DeferredContext d)) => d -> m (ReleaseKey, DeferredContext d)
	-- | Create static texture.
	createStaticTexture :: (MonadResource m, MonadBaseControl IO m) => d -> TextureInfo -> BS.ByteString -> m (ReleaseKey, TextureId d)
	-- | Create render target.
	createRenderTarget :: (MonadResource m, MonadBaseControl IO m) => d -> TextureInfo -> m (ReleaseKey, RenderTargetId d)
	-- | Create vertex layout.
	createVertexLayout :: (MonadResource m, MonadBaseControl IO m) => d -> VertexLayoutInfo -> m (ReleaseKey, VertexLayoutId d)
	-- | Create vertex buffer.
	createVertexBuffer :: (MonadResource m, MonadBaseControl IO m) => d -> VertexLayoutId d -> BS.ByteString -> m (ReleaseKey, VertexBufferId d)

-- | Class of graphics context.
-- Performs actual render operations.
class Context c where
	-- | Render something.
	render :: c -> Render c a -> IO a
	-- | Replay deferred context on this context.
	playContext :: Context dc => c -> dc -> IO ()

-- | Presenter class.
class Context c => Presenter p c | p -> c where
	-- | Present whatever needed.
	present :: c -> p -> Render c ()

-- | Rendering monad.
newtype Render c a = Render (c -> IO a)

instance Functor (Render c) where
	fmap f (Render h) = Render $ \c -> liftM f $ h c

instance Applicative (Render c) where
	pure a = Render $ \_c -> return a
	(Render f) <*> (Render h) = Render $ \c -> f c <*> h c

instance Monad (Render c) where
	return a = Render $ \_c -> return a
	(Render h) >>= f = Render $ \c -> do
		r <- h c
		let Render q = f r
		q c

instance MonadBase IO (Render c) where
	liftBase io = Render $ \_ -> io

instance Context c => MonadBaseControl IO (Render c) where
	type StM (Render c) a = c -> IO a
	liftBaseWith f = Render $ \_c -> f $ \(Render q) -> return q
	restoreM = Render

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
