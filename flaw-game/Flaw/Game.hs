{-|
Module: Flaw.Game
Description: Abstract from platform for game initialization.
License: MIT
-}

{-# LANGUAGE CPP, FlexibleContexts, RecursiveDo #-}

module Flaw.Game
	( initGame
	, runGame
	, exitGame
	, GameWindow
	, GameGraphicsDevice
	, GameGraphicsContext
	, GameGraphicsPresenter
	, GameInputManager
	) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Data.Time
import Data.Typeable


#if defined(ghcjs_HOST_OS)

import qualified Flaw.Window.Web.Canvas as Web
import Flaw.Graphics.WebGL
import Flaw.Input.Web

type GameWindow = Web.Canvas
type GameGraphicsDevice = WebGLDevice
type GameGraphicsContext = WebGLContext
type GameGraphicsPresenter = WebGLPresenter
type GameInputManager = WebInputManager

#else

#if defined(mingw32_HOST_OS)

import Flaw.Graphics
import Flaw.Graphics.DirectX11
import Flaw.Graphics.DXGI
import Flaw.Input.Win32
import Flaw.Window.Win32

type GameWindow = Win32Window
type GameGraphicsDevice = Dx11Device
type GameGraphicsContext = Dx11Context
type GameGraphicsPresenter = Dx11Presenter
type GameInputManager = Win32InputManager

#endif

#endif


initGame :: (MonadResource m, MonadBaseControl IO m) => T.Text -> Int -> Int -> Bool
	-> m
		( ReleaseKey
		, (GameWindow, GameGraphicsDevice, GameGraphicsContext, GameGraphicsPresenter, GameInputManager)
		)
initGame title width height needDepth = do
#if defined(ghcjs_HOST_OS)

	window@(Web.Canvas domCanvas) <- liftIO $ Web.initCanvas title

	inputManager <- liftIO $ initWebInputManager window

	(releaseKey, graphicsDevice, graphicsContext, presenter) <- webglInit domCanvas needDepth

#else

#if defined(mingw32_HOST_OS)

	(windowSystemReleaseKey, windowSystem) <- initWin32WindowSystem
	(windowReleaseKey, window) <- createWin32Window windowSystem title 0 0 width height

	inputManager <- liftIO $ initWin32InputManager window

	(graphicsSystemReleaseKey, graphicsSystem) <- dxgiCreateSystem
	(graphicsDevicesReleaseKey, graphicsDevices) <- getInstalledDevices graphicsSystem
	(graphicsDeviceReleaseKey, graphicsDevice, graphicsContext) <- dx11CreateDevice $ fst $ head graphicsDevices
	(presenterReleaseKey, presenter) <- dx11CreatePresenter graphicsDevice window Nothing needDepth

	releaseKey <- register $ do
		release windowSystemReleaseKey
		release windowReleaseKey
		release graphicsSystemReleaseKey
		release graphicsDevicesReleaseKey
		release graphicsDeviceReleaseKey
		release presenterReleaseKey

#endif

#endif

	return (releaseKey, (window, graphicsDevice, graphicsContext, presenter, inputManager))

-- | Run game loop.
-- To exit loop, call `exitGame`.
runGame :: a -> (Float -> a -> IO a) -> IO ()
runGame firstState step = mdo
	let f lastTime state = do
		currentTime <- getCurrentTime
		let frameTime = fromRational $ toRational $ diffUTCTime currentTime lastTime
		newState <- step frameTime state
		f currentTime newState
	veryFirstTime <- getCurrentTime
	catch (f veryFirstTime firstState) $ \ExitGameException -> return ()

exitGame :: IO ()
exitGame = throwIO ExitGameException

data ExitGameException = ExitGameException deriving (Show, Typeable)

instance Exception ExitGameException
