{-|
Module: Flaw.App
Description: Abstract from platform for app initialization.
License: MIT
-}

{-# LANGUAGE CPP, GADTs, OverloadedStrings #-}

module Flaw.App
	( withApp
	, runApp
	, exitApp
	, appConfig
	, AppConfig(..)
	, AppWindow
	, AppGraphicsDevice
	, AppGraphicsContext
	, AppGraphicsPresenter
	, AppInputManager
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Data.Time
import Data.Typeable

import Flaw.BinaryCache
import Flaw.Book

#if defined(ghcjs_HOST_OS)

import Flaw.Graphics.WebGL
import Flaw.Input.Web
import Flaw.Js
import qualified Flaw.Window.Web.Canvas as Web

type AppWindow = Web.Canvas
type AppGraphicsDevice = WebGLDevice
type AppGraphicsContext = WebGLContext
type AppGraphicsPresenter = WebGLPresenter
type AppInputManager = WebInputManager

#else

#if defined(mingw32_HOST_OS)

import Flaw.BinaryCache
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

#else

import Flaw.Graphics
import Flaw.Graphics.OpenGL.Sdl
import Flaw.Input.Sdl
import Flaw.Window.Sdl

type AppWindow = SdlWindow
type AppGraphicsDevice = OpenGLSdlDevice
type AppGraphicsContext = OpenGLSdlContext
type AppGraphicsPresenter = OpenGLSdlPresenter
type AppInputManager = SdlInputManager

#endif

#endif

data AppConfig where
	AppConfig :: BinaryCache c =>
		{ appConfigTitle :: !T.Text
		, appConfigWindowPosition :: !(Maybe (Int, Int))
		, appConfigWindowSize :: !(Maybe (Int, Int))
		, appConfigNeedDepthBuffer :: !Bool
		, appConfigBinaryCache :: !c
		, appConfigDebug :: !Bool
		} -> AppConfig

appConfig :: AppConfig
appConfig = AppConfig
	{ appConfigTitle = "flaw app"
	, appConfigWindowPosition = Nothing
	, appConfigWindowSize = Nothing
	, appConfigNeedDepthBuffer = False
	, appConfigBinaryCache = NullBinaryCache
	, appConfigDebug = False
	}

-- | Init application.
-- This function must be called from main thread, and it doesn't return until windowing system
-- is shut down. It forks a new lightweight thread and calls the callback provided in it.
-- That complexity comes from demands of various windowing systems.
withApp :: AppConfig
	-> (AppWindow -> AppGraphicsDevice -> AppGraphicsContext -> AppGraphicsPresenter -> AppInputManager -> IO ()) -> IO ()
withApp AppConfig
	{ appConfigTitle = title
	, appConfigWindowPosition = maybeWindowPosition
	, appConfigWindowSize = maybeWindowSize
	, appConfigNeedDepthBuffer = needDepthBuffer
	, appConfigBinaryCache = binaryCache
	, appConfigDebug = debug
	} callback = do

#if defined(ghcjs_HOST_OS)
	initJs
#endif

	windowSystemVar <- newEmptyMVar
	void $ forkIO $ withBook $ \bk -> do
		windowSystem <- book bk $ takeMVar windowSystemVar

#if defined(ghcjs_HOST_OS)
		window <- Web.initCanvas windowSystem title
		inputManager <- initWebInput window
		let _ = (maybeWindowPosition, maybeWindowSize, binaryCache, debug)
		(graphicsDevice, graphicsContext, presenter) <- book bk $ webglInit window needDepthBuffer
#else
#if defined(mingw32_HOST_OS)
		window <- book bk $ createWin32Window windowSystem title maybeWindowPosition maybeWindowSize
		inputManager <- initWin32Input window
		graphicsSystem <- book bk $ dxgiCreateSystem
		graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
		(graphicsDevice, graphicsContext) <- book bk $ dx11CreateDevice (fst $ head graphicsDevices) binaryCache debug
		presenter <- book bk $ dx11CreatePresenter graphicsDevice window Nothing needDepthBuffer
#else
		window <- book bk $ createSdlWindow windowSystem title maybeWindowPosition maybeWindowSize needDepthBuffer
		inputManager <- initSdlInput window
		graphicsSystem <- book bk createOpenGLSdlSystem
		graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
		(graphicsContext, presenter) <- book bk $ createOpenGLSdlPresenter (fst $ head graphicsDevices) window binaryCache debug
		let graphicsDevice = graphicsContext
#endif
#endif

		callback window graphicsDevice graphicsContext presenter inputManager

	-- run window system
#if defined(ghcjs_HOST_OS)
	Web.runWebWindowSystem windowSystemVar
#else
#if defined(mingw32_HOST_OS)
	runWin32WindowSystem windowSystemVar
#else
	runSdlWindowSystem debug windowSystemVar
#endif
#endif

-- | Run app loop.
-- To exit loop, call `exitApp`.
{-# INLINE runApp #-}
runApp :: (Float -> IO ()) -> IO ()
runApp step = do
	let f lastTime = do
		currentTime <- getCurrentTime
		let frameTime = fromRational $ toRational $ diffUTCTime currentTime lastTime
		step frameTime
		f currentTime
	veryFirstTime <- getCurrentTime
	catch (f veryFirstTime) $ \ExitAppException -> return ()

{-# INLINE exitApp #-}
exitApp :: IO ()
exitApp = throwIO ExitAppException

data ExitAppException = ExitAppException deriving (Show, Typeable)

instance Exception ExitAppException
