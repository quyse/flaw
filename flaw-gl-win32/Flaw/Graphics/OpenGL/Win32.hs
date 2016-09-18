{-|
Module: Flaw.Graphics.OpenGL.Win32
Description: OpenGL graphics implementation for Windows.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.OpenGL.Win32
	( OpenGLWin32System()
	, OpenGLWin32Device
	, OpenGLWin32Context
	, OpenGLWin32Presenter()
	, createOpenGLWin32System
	, createOpenGLWin32Presenter
	) where

import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Text as T
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.BinaryCache
import Flaw.Book
import Flaw.Exception
import Flaw.FFI.Win32
import Flaw.Graphics
import Flaw.Graphics.GlContext
import Flaw.Graphics.OpenGL
import Flaw.Math
import Flaw.Window.Win32

data OpenGLWin32System = OpenGLWin32System

instance System OpenGLWin32System where
	data DeviceId OpenGLWin32System = GlDeviceId
	data DisplayId OpenGLWin32System = GlDisplayId
	data DisplayModeId OpenGLWin32System = GlDisplayModeId

	getInstalledDevices OpenGLWin32System = return ([(GlDeviceId, DeviceInfo
		{ deviceName = T.pack "Win32 OpenGL Device"
		, deviceDisplays = []
		})], return ())

	createDisplayMode OpenGLWin32System _ _ _ = undefined

type OpenGLWin32Device = GlDevice
type OpenGLWin32Context = GlContext

data OpenGLWin32Presenter = OpenGLWin32Presenter
	{ openglPresenterWindow :: !Win32Window
	, openglPresenterHDC :: !HDC
	}

instance Presenter OpenGLWin32Presenter OpenGLWin32System GlContext GlContext where
	-- TODO
	setPresenterMode _presenter _maybeMode = return ()

	presenterRender OpenGLWin32Presenter
		{ openglPresenterWindow = window@Win32Window
			{ wWindowSystem = ws
			}
		, openglPresenterHDC = hdc
		} GlContext
		{ glContextDesiredState = desiredContextState@GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} f = invokeWin32WindowSystem ws $ do
		-- clear state
		glSetDefaultContextState desiredContextState

		-- get viewport size
		(width, height) <- getWin32WindowClientSize_unsafe window

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
		c_swapWin32OpenGLWindow hdc

		return r

createOpenGLWin32System :: IO (OpenGLWin32System, IO ())
createOpenGLWin32System = return (OpenGLWin32System, return ())

createOpenGLWin32Presenter :: BinaryCache c => DeviceId OpenGLWin32System -> Win32Window -> c -> Bool -> IO ((GlContext, OpenGLWin32Presenter), IO ())
createOpenGLWin32Presenter _deviceId window@Win32Window
	{ wWindowSystem = ws
	, wHandle = hwnd
	} programCache debug = describeException "failed to create OpenGL Win32 presenter" $ withSpecialBook $ \bk -> invokeWin32WindowSystem ws $ do

	-- create OpenGL context and make it current
	(hglrc, hdc) <- alloca $ \hdcPtr -> do
		hglrc <- c_initWin32OpenGLContext hwnd (if debug then 1 else 0) hdcPtr
		when (hglrc == nullPtr) $ throwIO $ DescribeFirstException "failed to init Win32 OpenGL context"
		hdc <- peek hdcPtr
		return (hglrc, hdc)
	book bk $ return ((), invokeWin32WindowSystem ws $ c_deinitWin32OpenGLContext hwnd hglrc)

	-- create context
	let presenter = OpenGLWin32Presenter
		{ openglPresenterWindow = window
		, openglPresenterHDC = hdc
		}
	context <- createOpenGLContext programCache (invokeWin32WindowSystem ws) (invokeWin32WindowSystem ws) debug

	-- TODO: set swap interval

	return (context, presenter)

type HGLRC = Ptr ()
type HDC = Ptr ()

foreign import ccall safe "initWin32OpenGLContext" c_initWin32OpenGLContext :: HWND -> CInt -> Ptr HDC -> IO HGLRC
foreign import ccall safe "deinitWin32OpenGLContext" c_deinitWin32OpenGLContext :: HWND -> HGLRC -> IO ()
foreign import ccall safe "swapWin32OpenGLWindow" c_swapWin32OpenGLWindow :: HDC -> IO ()
