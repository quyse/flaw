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


initApp :: T.Text -> Int -> Int -> Bool
	-> IO
		( (AppWindow, AppGraphicsDevice, AppGraphicsContext, AppGraphicsPresenter, AppInputManager)
		, IO ()
		)
initApp title width height needDepth = do

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
	(graphicsDevice, graphicsContext) <- book bk $ dx11CreateDevice $ fst $ head graphicsDevices
	presenter <- book bk $ dx11CreatePresenter graphicsDevice window Nothing needDepth

#endif

#endif

	return ((window, graphicsDevice, graphicsContext, presenter, inputManager), freeBook bk)

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
