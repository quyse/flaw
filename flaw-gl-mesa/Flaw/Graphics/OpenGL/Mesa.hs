{-|
Module: Flaw.Graphics.OpenGL.Mesa
Description: OpenGL graphics implementation using Mesa for off-screen (OS) context initialization.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms, TypeFamilies #-}

module Flaw.Graphics.OpenGL.Mesa
	( OpenGLOsMesaSystem()
	, OpenGLOsMesaDevice
	, OpenGLOsMesaContext
	, OpenGLOsMesaPresenter()
	, createOpenGLOsMesaPresenter
	) where

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL.Core33

import Flaw.BinaryCache
import Flaw.Book
import Flaw.Exception
import Flaw.Flow
import Flaw.Graphics
import Flaw.Graphics.GlContext
import Flaw.Graphics.OpenGL
import Flaw.Math

data OpenGLOsMesaSystem

instance System OpenGLOsMesaSystem where
	-- TODO: implement something
	data DeviceId OpenGLOsMesaSystem
	data DisplayId OpenGLOsMesaSystem
	data DisplayModeId OpenGLOsMesaSystem
	getInstalledDevices _ = return ([], return ())
	createDisplayMode _ _ _ _ = throwIO $ DescribeFirstException "display modes for OpenGL Mesa OS not implemented"

type OpenGLOsMesaDevice = GlContext
type OpenGLOsMesaContext = GlContext

data OpenGLOsMesaPresenter = OpenGLOsMesaPresenter
	{ openglPresenterFlow :: {-# UNPACK #-} !Flow
	, openglPresenterBufferPtr :: {-# UNPACK #-} !(Ptr ())
	, openglPresenterWidth :: {-# UNPACK #-} !Int
	, openglPresenterHeight :: {-# UNPACK #-} !Int
	}

instance Presenter OpenGLOsMesaPresenter OpenGLOsMesaSystem GlContext GlContext where
	-- TODO: allow to change presenter size dynamically
	setPresenterMode _presenter _maybeMode = return ()

	presenterRender OpenGLOsMesaPresenter
		{ openglPresenterFlow = flow
		, openglPresenterWidth = width
		, openglPresenterHeight = height
		} GlContext
		{ glContextDesiredState = desiredContextState@GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} f = runInFlow flow $ do
		-- clear state
		glSetDefaultContextState desiredContextState

		-- setup state
		writeIORef frameBufferRef GlFrameBufferId
			{ glFrameBufferName = 0
			, glFrameBufferWidth = width
			, glFrameBufferHeight = height
			}
		writeIORef viewportRef $ Vec4 0 0 width height

		-- perform render
		f

createOpenGLOsMesaPresenter :: Int -> Int -> Bool -> Bool -> IO ((GlContext, OpenGLOsMesaPresenter), IO ())
createOpenGLOsMesaPresenter width height needDepth debug = describeException "failed to create OpenGL OS MESA presenter" $ withSpecialBook $ \bk -> do
	flow <- book bk newFlowOS
	runInFlow flow $ do
		bufferPtr <- book bk $ do
			bufferPtr <- mallocBytes (width * height * 4)
			return (bufferPtr, free bufferPtr)
		contextPtr <- withArray
			[ OSMESA_FORMAT, OSMESA_RGBA
			, OSMESA_DEPTH_BITS, if needDepth then 24 else 0
			, OSMESA_STENCIL_BITS, if needDepth then 8 else 0
			, OSMESA_ACCUM_BITS, 0
			, OSMESA_PROFILE, OSMESA_CORE_PROFILE
			, OSMESA_CONTEXT_MAJOR_VERSION, 3
			, OSMESA_CONTEXT_MINOR_VERSION, 3
			] $ \attribsPtr -> c_OSMesaCreateContextAttribs (castPtr attribsPtr) nullPtr
		when (contextPtr == nullPtr) $ throwIO $ DescribeFirstException "failed to create context"
		book bk $ return ((), c_OSMesaDestroyContext contextPtr)

		r <- c_OSMesaMakeCurrent contextPtr bufferPtr GL_UNSIGNED_BYTE (fromIntegral width) (fromIntegral height)
		when (r == GL_FALSE) $ throwIO $ DescribeFirstException "failed to make context current"

		context <- createOpenGLContext NullBinaryCache (runInFlow flow) debug

		return (context, OpenGLOsMesaPresenter
			{ openglPresenterFlow = flow
			, openglPresenterBufferPtr = bufferPtr
			, openglPresenterWidth = width
			, openglPresenterHeight = height
			})

foreign import ccall safe "OSMesaCreateContextAttribs" c_OSMesaCreateContextAttribs :: Ptr CInt -> Ptr C_OSMesaContext -> IO (Ptr C_OSMesaContext)
foreign import ccall safe "OSMesaDestroyContext" c_OSMesaDestroyContext :: Ptr C_OSMesaContext -> IO ()
foreign import ccall safe "OSMesaMakeCurrent" c_OSMesaMakeCurrent :: Ptr C_OSMesaContext -> Ptr () -> GLenum -> GLsizei -> GLsizei -> IO GLboolean

pattern OSMESA_FORMAT                = 0x22
pattern OSMESA_DEPTH_BITS            = 0x30
pattern OSMESA_STENCIL_BITS          = 0x31
pattern OSMESA_ACCUM_BITS            = 0x32
pattern OSMESA_PROFILE               = 0x33
pattern OSMESA_CORE_PROFILE          = 0x34
pattern OSMESA_CONTEXT_MAJOR_VERSION = 0x36
pattern OSMESA_CONTEXT_MINOR_VERSION = 0x37

pattern OSMESA_RGBA                  = GL_RGBA

data C_OSMesaContext
