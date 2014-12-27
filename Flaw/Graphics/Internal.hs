{-|
Module: Flaw.Graphics.Internal
Description: Internal methods of graphics.
License: MIT
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Flaw.Graphics.Internal
	( Device(..)
	, Context(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	) where

import Control.Monad.Trans.Resource
import qualified Data.Text as T

-- | Class of graphics device.
class Device d where
	-- | Type for id of device.
	data DeviceId d :: *
	-- | Type for id of display.
	data DisplayId d :: *
	-- | Type for id of display mode.
	data DisplayModeId d :: *
	-- | Type for device context.
	type DeviceContext d :: *
	-- | Get devices installed in system.
	getDevices :: (MonadResource m, MonadBaseControl IO m) => m (ReleaseKey, [(DeviceId d, DeviceInfo d)])
	-- | Try to create custom display mode.
	tryCreateDisplayMode :: MonadResource m => DisplayModeInfo -> m (Maybe (ReleaseKey, (DisplayModeId d, DisplayModeInfo)))
	-- | Create device.
	createDevice :: MonadResource m => DeviceId d -> m (ReleaseKey, d)

-- | Class of graphics context.
class Context c where

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
