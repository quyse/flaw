{-|
Module: Flaw.App
Description: Abstract from platform for app initialization.
License: MIT
-}

{-# LANGUAGE CPP, RecursiveDo #-}

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
import qualified Data.Text as T
import Data.Time
import Data.Typeable

import Flaw.Book

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
import Flaw.Graphics.OpenGL
import Flaw.Input.Sdl
import Flaw.Window.Sdl

type AppWindow = SdlWindow
type AppGraphicsDevice = GlDevice
type AppGraphicsContext = GlContext
type AppGraphicsPresenter = GlPresenter
type AppInputManager = SdlInputManager

#endif

#endif


initApp
	:: BinaryCache c
	=> c -- ^ Binary cache.
	-> T.Text -- ^ Window title.
	-> Int -- ^ Window width.
	-> Int -- ^ Window height.
	-> Bool -- ^ Create depth buffer.
	-> Bool -- ^ Enable debug features.
	-> IO
		( (AppWindow, AppGraphicsDevice, AppGraphicsContext, AppGraphicsPresenter, AppInputManager)
		, IO ()
		)
initApp binaryCache title width height needDepth debug = do

	bk <- newBook

#if defined(ghcjs_HOST_OS)

	window@Web.Canvas
		{ Web.canvasElement = domCanvas
		} <- Web.initCanvas title

	inputManager <- initWebInput window

	(graphicsDevice, graphicsContext, presenter) <- book bk $ webglInit domCanvas needDepth

#else

#if defined(mingw32_HOST_OS)

	windowSystem <- book bk $ initWin32WindowSystem
	window <- book bk $ createWin32Window windowSystem title 0 0 width height

	inputManager <- initWin32Input window

	graphicsSystem <- book bk $ dxgiCreateSystem
	graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
	(graphicsDevice, graphicsContext) <- book bk $ dx11CreateDevice (fst $ head graphicsDevices) binaryCache debug
	presenter <- book bk $ dx11CreatePresenter graphicsDevice window Nothing needDepth

#else

	windowSystem <- book bk $ initSdlWindowSystem debug
	window <- book bk $ createSdlWindow windowSystem title 0 0 width height needDepth

	inputManager <- initSdlInput window

	graphicsSystem <- book bk createGlSystem
	graphicsDevices <- book bk $ getInstalledDevices graphicsSystem
	graphicsContext <- book bk $ createGlContext (fst $ head graphicsDevices) window debug
	let graphicsDevice = graphicsContext
	let presenter = graphicsContext

#endif

#endif

	return ((window, graphicsDevice, graphicsContext, presenter, inputManager), freeBook bk)

-- | Run app loop.
-- To exit loop, call `exitApp`.
{-# INLINE runApp #-}
runApp :: (Float -> IO ()) -> IO ()
runApp step = mdo
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
