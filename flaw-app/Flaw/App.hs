{-|
Module: Flaw.App
Description: Abstract from platform for app initialization.
License: MIT
-}

{-# LANGUAGE CPP, GADTs, RankNTypes #-}

module Flaw.App
	( withApp
	, runApp
	, exitApp
	, appConfig
	, AppGraphicsSystemId(..)
	, AppConfig(..)
	, AppWindow
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
import Flaw.Exception
import Flaw.Graphics


-- platform-specific imports

#if defined(ghcjs_HOST_OS)

import Flaw.Graphics.WebGL
import Flaw.Input.Web
import Flaw.Js
import qualified Flaw.Window.Web.Canvas as Web

#else

#if defined(mingw32_HOST_OS)

import Flaw.Graphics.DirectX11
import Flaw.Graphics.DXGI
import Flaw.Graphics.OpenGL.Win32
import Flaw.Input.Win32
import Flaw.Window.Win32

#else

import Flaw.Graphics.OpenGL.Sdl
import Flaw.Input.Sdl
import Flaw.Window.Sdl

#endif

#endif


-- platform-specific type synonyms
#if defined(ghcjs_HOST_OS)
type AppWindow = Web.Canvas
type AppInputManager = WebInputManager
#else
#if defined(mingw32_HOST_OS)
type AppWindow = Win32Window
type AppInputManager = Win32InputManager
#else
type AppWindow = SdlWindow
type AppInputManager = SdlInputManager
#endif
#endif


-- | Supported graphics systems.
data AppGraphicsSystemId
	= AppGraphicsSystemDirectX11
	| AppGraphicsSystemOpenGL
	| AppGraphicsSystemWebGL

data AppConfig where
	AppConfig :: BinaryCache c =>
		{ appConfigTitle :: !T.Text
		, appConfigWindowPosition :: !(Maybe (Int, Int))
		, appConfigWindowSize :: !(Maybe (Int, Int))
		, appConfigNeedDepthBuffer :: !Bool
		, appConfigBinaryCache :: !c
		, appConfigDebug :: !Bool
		-- | Graphics systems in order to try.
		, appConfigGraphicsSystems :: ![AppGraphicsSystemId]
		} -> AppConfig

appConfig :: AppConfig
appConfig = AppConfig
	{ appConfigTitle = T.pack "flaw app"
	, appConfigWindowPosition = Nothing
	, appConfigWindowSize = Nothing
	, appConfigNeedDepthBuffer = False
	, appConfigBinaryCache = NullBinaryCache
	, appConfigDebug = False
	, appConfigGraphicsSystems =
#if defined(mingw32_HOST_OS)
		AppGraphicsSystemDirectX11 :
#endif
#if defined(ghcjs_HOST_OS)
		AppGraphicsSystemWebGL :
#else
		AppGraphicsSystemOpenGL :
#endif
		[]
	}

-- | Initialized graphics system.
data GraphicsSystem where
	GraphicsSystem :: Presenter p s c d => s -> d -> c -> p -> GraphicsSystem

-- | Application callback.
type AppCallback = forall p s c d. Presenter p s c d => AppWindow -> d -> c -> p -> AppInputManager -> IO ()

-- | Init application.
-- This function must be called from main thread, and it doesn't return until windowing system
-- is shut down. It forks a new lightweight thread and calls the callback provided in it.
-- That complexity comes from demands of various windowing systems.
withApp :: AppConfig -> AppCallback -> IO ()
withApp AppConfig
	{ appConfigTitle = title
	, appConfigWindowPosition = maybeWindowPosition
	, appConfigWindowSize = maybeWindowSize
	, appConfigNeedDepthBuffer = needDepthBuffer
	, appConfigBinaryCache = binaryCache
	, appConfigDebug = debug
	, appConfigGraphicsSystems = graphicsSystemsIds
	} callback = do

#if defined(ghcjs_HOST_OS)
	initJs
#endif

	windowSystemVar <- newEmptyMVar
	void $ forkIO $ withBook $ \bk -> do
		windowSystem <- book bk $ takeMVar windowSystemVar

		-- initialize input manager
#if defined(ghcjs_HOST_OS)
		window <- Web.initCanvas windowSystem title
		inputManager <- initWebInput window
#else
#if defined(mingw32_HOST_OS)
		window <- book bk $ createWin32Window windowSystem title maybeWindowPosition maybeWindowSize
		inputManager <- initWin32Input window
#else
		window <- book bk $ createSdlWindow windowSystem title maybeWindowPosition maybeWindowSize needDepthBuffer
		inputManager <- initSdlInput window
#endif
#endif

		-- initialize specified graphics system
		let initGraphics graphicsSystemId = case graphicsSystemId of
#if defined(ghcjs_HOST_OS)
			AppGraphicsSystemWebGL -> do
				let _ = (maybeWindowPosition, maybeWindowSize, binaryCache, debug)
				(graphicsSystem, graphicsDevice, graphicsContext, presenter) <- book bk $ webglInit window needDepthBuffer
				return $ GraphicsSystem graphicsSystem graphicsDevice graphicsContext presenter
#else
#if defined(mingw32_HOST_OS)
			AppGraphicsSystemDirectX11 -> do
				graphicsSystem <- book bk $ dxgiCreateSystem
				graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
				(graphicsDevice, graphicsContext) <- book bk $ dx11CreateDevice (fst $ head graphicsDevices) binaryCache debug
				presenter <- book bk $ dx11CreatePresenter graphicsDevice window Nothing needDepthBuffer
				return $ GraphicsSystem graphicsSystem graphicsDevice graphicsContext presenter
#endif
			AppGraphicsSystemOpenGL -> do
#if defined(mingw32_HOST_OS)
				graphicsSystem <- book bk createOpenGLWin32System
				graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
				(graphicsContext, presenter) <- book bk $ createOpenGLWin32Presenter (fst $ head graphicsDevices) window binaryCache debug
#else
				graphicsSystem <- book bk createOpenGLSdlSystem
				graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
				(graphicsContext, presenter) <- book bk $ createOpenGLSdlPresenter (fst $ head graphicsDevices) window binaryCache debug
#endif
				return $ GraphicsSystem graphicsSystem graphicsContext graphicsContext presenter
#endif
			_ -> throwIO $ DescribeFirstException "unsupported graphics system"

		-- try to initialize graphics systems in order
		let
			tryInitGraphics (graphicsSystemId : restGraphicsSystemsIds) =
				catch (initGraphics graphicsSystemId) $ \SomeException {} -> tryInitGraphics restGraphicsSystemsIds
			tryInitGraphics [] = throwIO $ DescribeFirstException "no graphics system can be initialized"

		GraphicsSystem _graphicsSystem graphicsDevice graphicsContext presenter <- tryInitGraphics graphicsSystemsIds
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
