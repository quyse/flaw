{-|
Module: Flaw.Graphics.OpenGL.Sdl
Description: OpenGL graphics implementation, using SDL for low-level interaction with system.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.OpenGL.Sdl
	( OpenGLSdlSystem()
	, OpenGLSdlDevice
	, OpenGLSdlContext
	, OpenGLSdlPresenter()
	, createOpenGLSdlSystem
	, createOpenGLSdlPresenter
	) where

import Control.Monad
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified SDL.Raw.Types as SDL
import qualified SDL.Raw.Video as SDL

import Flaw.BinaryCache
import Flaw.Book
import Flaw.Exception
import Flaw.Flow
import Flaw.Graphics
import Flaw.Graphics.GlContext
import Flaw.Graphics.OpenGL
import Flaw.Math
import Flaw.Sdl
import Flaw.Window.Sdl

data OpenGLSdlSystem = OpenGLSdlSystem

instance System OpenGLSdlSystem where
	newtype DeviceId OpenGLSdlSystem = GlDeviceId Int
	newtype DisplayId OpenGLSdlSystem = GlDisplayId Int
	newtype DisplayModeId OpenGLSdlSystem = GlDisplayModeId SDL.DisplayMode

	getInstalledDevices OpenGLSdlSystem = do
		-- get video drivers
		driversCount <- SDL.getNumVideoDrivers
		drivers <- forM [0..(driversCount - 1)] $ \i -> do
			name <- fmap T.decodeUtf8 (B.unsafePackCString =<< SDL.getVideoDriver i)
			return (i, name)

		-- get displays
		displaysCount <- SDL.getNumVideoDisplays
		displays <- forM [0..(displaysCount - 1)] $ \i -> do
			name <- fmap T.decodeUtf8 (B.unsafePackCString =<< SDL.getDisplayName i)
			modesCount <- SDL.getNumDisplayModes i
			modes <- alloca $ \modePtr -> forM [0..(modesCount - 1)] $ \j -> do
				checkSdlError (== 0) $ SDL.getDisplayMode i j modePtr
				mode@SDL.DisplayMode
					{ SDL.displayModeW = width
					, SDL.displayModeH = height
					, SDL.displayModeRefreshRate = refreshRate
					} <- peek modePtr
				return (GlDisplayModeId mode, DisplayModeInfo
					{ displayModeName = T.pack $ "SDL " ++ show width ++ "x" ++ show height ++ ", " ++ show refreshRate ++ " Hz"
					, displayModeWidth = fromIntegral width
					, displayModeHeight = fromIntegral height
					, displayModeRefreshRate = fromIntegral refreshRate
					})
			return (GlDisplayId $ fromIntegral i, DisplayInfo
				{ displayName = name
				, displayModes = modes
				})

		return (flip map drivers $ \(i, name) -> (GlDeviceId $ fromIntegral i, DeviceInfo
			{ deviceName = name
			, deviceDisplays = displays
			}), return ())

	createDisplayMode _ _ _ _ = undefined

type OpenGLSdlDevice = GlDevice
type OpenGLSdlContext = GlContext

-- | OpenGL presenter.
data OpenGLSdlPresenter = OpenGLSdlPresenter
	{ openglPresenterWindow :: !SdlWindow
	}

instance Presenter OpenGLSdlPresenter OpenGLSdlSystem GlContext GlContext where
	-- TODO
	setPresenterMode _presenter _maybeMode = return ()

	presenterRender OpenGLSdlPresenter
		{ openglPresenterWindow = SdlWindow
			{ swSystem = ws
			, swHandle = windowHandle
			}
		} GlContext
		{ glContextDesiredState = desiredContextState@GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} f = invokeSdlWindowSystem ws $ do
		-- clear state
		glSetDefaultContextState desiredContextState

		-- get viewport size
		(width, height) <- alloca $ \widthPtr -> alloca $ \heightPtr -> do
			SDL.glGetDrawableSize windowHandle widthPtr heightPtr
			width <- fromIntegral <$> peek widthPtr
			height <- fromIntegral <$> peek heightPtr
			return (width, height)

		-- setup state
		writeIORef frameBufferRef GlFrameBufferId
			{ glFrameBufferName = 0
			, glFrameBufferWidth = width
			, glFrameBufferHeight = height
			}
		writeIORef viewportRef $ Vec4 0 0 width height

		-- perform render
		r <- f

		-- present
		SDL.glSwapWindow windowHandle

		return r

createOpenGLSdlSystem :: IO (OpenGLSdlSystem, IO ())
createOpenGLSdlSystem = return (OpenGLSdlSystem, return ())

createOpenGLSdlPresenter :: BinaryCache c => DeviceId OpenGLSdlSystem -> SdlWindow -> c -> Bool -> IO ((GlContext, OpenGLSdlPresenter), IO ())
createOpenGLSdlPresenter _deviceId window@SdlWindow
	{ swSystem = ws
	, swHandle = windowHandle
	} programCache debug = describeException "failed to create OpenGL SDL presenter" $ withSpecialBook $ \bk -> invokeSdlWindowSystem ws $ do
	-- spawn background flow
	backgroundFlow <- book bk newFlowOS
	-- create background context
	backgroundSdlContext <- checkSdlResult $ SDL.glCreateContext windowHandle
	book bk $ return ((), runInFlow backgroundFlow $ do
		void $ SDL.glMakeCurrent windowHandle nullPtr
		SDL.glDeleteContext backgroundSdlContext
		)
	-- create main context (has to be created while background context is current, in order to "share with current")
	mainSdlContext <- checkSdlResult $ SDL.glCreateContext windowHandle
	book bk $ return ((), invokeSdlWindowSystem ws $ do
		void $ SDL.glMakeCurrent windowHandle nullPtr
		SDL.glDeleteContext mainSdlContext
		)
	-- make background context current in background thread
	checkSdlError (== 0) $ runInFlow backgroundFlow $ SDL.glMakeCurrent windowHandle backgroundSdlContext
	-- make main context current in current thread
	checkSdlError (== 0) $ SDL.glMakeCurrent windowHandle mainSdlContext

	-- create context
	let presenter = OpenGLSdlPresenter
		{ openglPresenterWindow = window
		}
	context <- createOpenGLContext programCache (invokeSdlWindowSystem ws) (runInFlow backgroundFlow) debug

	-- set swap interval
	do
		-- try "late swap tearing"
		r <- SDL.glSetSwapInterval (-1)
		when (r /= 0) $
			-- didn't work, try usual vsync
			void $ SDL.glSetSwapInterval 1

	return (context, presenter)
