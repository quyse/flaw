{-|
Module: Flaw.App
Description: Abstract from platform for app initialization.
License: MIT
-}

{-# LANGUAGE CPP, FlexibleContexts, RecursiveDo #-}

module Flaw.App
	( initApp
	, runApp
	, exitApp
	, AppWindow
	, AppGraphicsDevice
	, AppGraphicsContext
	, AppGraphicsPresenter
	, AppInputManager
	) where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time
import Data.Typeable

import Flaw.Resource

#if defined(ghcjs_HOST_OS)

import qualified Flaw.Window.Web.Canvas as Web
import Flaw.Graphics.WebGL
import Flaw.Input.Web

type AppWindow = Web.Canvas
type AppGraphicsDevice = WebGLDevice
type AppGraphicsContext = WebGLContext
type AppGraphicsPresenter = WebGLPresenter
type AppInputManager = WebInputManager

#else

#if defined(mingw32_HOST_OS)

import Flaw.Graphics
import Flaw.Graphics.DirectX11
import Flaw.Graphics.DXGI
import Flaw.Input.Win32
import Flaw.Window.Win32

type AppWindow = Win32Window
type AppGraphicsDevice = Dx11Device
type AppGraphicsContext = Dx11Context
type AppGraphicsPresenter = Dx11Presenter
type AppInputManager = Win32InputManager

#endif

#endif


initApp :: ResourceIO m => T.Text -> Int -> Int -> Bool
	-> m
		( ReleaseKey
		, (AppWindow, AppGraphicsDevice, AppGraphicsContext, AppGraphicsPresenter, AppInputManager)
		)
initApp title width height needDepth = do
#if defined(ghcjs_HOST_OS)

	window@Web.Canvas
		{ Web.canvasElement = domCanvas
		} <- liftIO $ Web.initCanvas title

	inputManager <- liftIO $ initWebInput window

	(releaseKey, graphicsDevice, graphicsContext, presenter) <- webglInit domCanvas needDepth

#else

#if defined(mingw32_HOST_OS)

	(windowSystemReleaseKey, windowSystem) <- initWin32WindowSystem
	(windowReleaseKey, window) <- createWin32Window windowSystem title 0 0 width height

	inputManager <- liftIO $ initWin32Input window

	(graphicsSystemReleaseKey, graphicsSystem) <- dxgiCreateSystem
	(graphicsDevicesReleaseKey, graphicsDevices) <- getInstalledDevices graphicsSystem
	(graphicsDeviceReleaseKey, graphicsDevice, graphicsContext) <- dx11CreateDevice $ fst $ head graphicsDevices
	(presenterReleaseKey, presenter) <- dx11CreatePresenter graphicsDevice window Nothing needDepth

	releaseKey <- registerRelease $ do
		release windowSystemReleaseKey
		release windowReleaseKey
		release graphicsSystemReleaseKey
		release graphicsDevicesReleaseKey
		release graphicsDeviceReleaseKey
		release presenterReleaseKey

#endif

#endif

	return (releaseKey, (window, graphicsDevice, graphicsContext, presenter, inputManager))

-- | Run app loop.
-- To exit loop, call `exitApp`.
runApp :: a -> (Float -> a -> IO a) -> IO ()
runApp firstState step = mdo
	let f lastTime state = do
		currentTime <- getCurrentTime
		let frameTime = fromRational $ toRational $ diffUTCTime currentTime lastTime
		newState <- step frameTime state
		f currentTime newState
	veryFirstTime <- getCurrentTime
	catch (f veryFirstTime firstState) $ \ExitAppException -> return ()

exitApp :: IO ()
exitApp = throwIO ExitAppException

data ExitAppException = ExitAppException deriving (Show, Typeable)

instance Exception ExitAppException
