{-|
Module: Flaw.Graphics.OpenGL.SDL
Description: OpenGL graphics implementation, using SDL for low-level interaction with system.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.OpenGL.SDL
	( OpenGLSystem()
	, OpenGLDevice
	, OpenGLContext
	, OpenGLPresenter()
	, createOpenGLSystem
	, createOpenGLPresenter
	) where

import Control.Monad
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.ARB.DebugOutput
import Graphics.GL.Core33
import qualified SDL.Raw.Types as SDL
import qualified SDL.Raw.Video as SDL

import Flaw.BinaryCache
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.GlContext
import Flaw.Graphics.GLSL
import Flaw.Math
import Flaw.Sdl
import Flaw.Window.Sdl

data OpenGLSystem = OpenGLSystem

instance System OpenGLSystem where
	newtype DeviceId OpenGLSystem = GlDeviceId Int
	newtype DisplayId OpenGLSystem = GlDisplayId Int
	newtype DisplayModeId OpenGLSystem = GlDisplayModeId SDL.DisplayMode

	getInstalledDevices OpenGLSystem = do
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

type OpenGLDevice = GlDevice
type OpenGLContext = GlContext

-- | OpenGL presenter.
data OpenGLPresenter = OpenGLPresenter
	{ openglPresenterSdlContext :: !SDL.GLContext
	, openglPresenterWindow :: !SdlWindow
	}

instance Presenter OpenGLPresenter OpenGLSystem GlContext GlContext where
	-- TODO
	setPresenterMode _presenter _maybeMode = return ()

	presenterRender presenter@OpenGLPresenter
		{ openglPresenterWindow = SdlWindow
			{ swHandle = windowHandle
			}
		} GlContext
		{ glContextDesiredState = desiredContextState@GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} f = openglInvoke presenter $ do
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

createOpenGLSystem :: IO (OpenGLSystem, IO ())
createOpenGLSystem = return (OpenGLSystem, return ())

createOpenGLPresenter :: BinaryCache c => DeviceId OpenGLSystem -> SdlWindow -> c -> Bool -> IO ((GlContext, OpenGLPresenter), IO ())
createOpenGLPresenter _deviceId window@SdlWindow
	{ swSystem = ws
	, swHandle = windowHandle
	} programCache debug = describeException "failed to create OpenGL device" $ invokeSdlWindowSystem ws $ do
	-- create context
	sdlContext <- checkSdlResult $ SDL.glCreateContext windowHandle
	-- make it current
	checkSdlError (== 0) $ SDL.glMakeCurrent windowHandle sdlContext

	-- init capabilities
	capArbUniformBufferObject <- isOpenGLExtensionSupported "GL_ARB_uniform_buffer_object"
	capArbSamplerObjects <- isOpenGLExtensionSupported "GL_ARB_sampler_objects"
	capArbVertexAttribBinding <- isOpenGLExtensionSupported "GL_ARB_vertex_attrib_binding"
	capArbFramebufferObject <- isOpenGLExtensionSupported "GL_ARB_framebuffer_object"
	capArbTextureStorage <- isOpenGLExtensionSupported "GL_ARB_texture_storage"
	capArbInstancedArrays <- isOpenGLExtensionSupported "GL_ARB_instanced_arrays"
	capArbDebugOutput <- isOpenGLExtensionSupported "GL_ARB_debug_output"
	capArbGetProgramBinary <- isOpenGLExtensionSupported "GL_ARB_get_program_binary"

	-- create context
	let presenter = OpenGLPresenter
		{ openglPresenterSdlContext = sdlContext
		, openglPresenterWindow = window
		}
	context <- newGlContext (openglInvoke presenter) GlCaps
		{ glCapsArbUniformBufferObject = capArbUniformBufferObject
		, glCapsArbSamplerObjects = capArbSamplerObjects
		, glCapsArbVertexAttribBinding = capArbVertexAttribBinding
		, glCapsArbFramebufferObject = capArbFramebufferObject
		, glCapsArbTextureStorage = capArbTextureStorage
		, glCapsArbInstancedArrays = capArbInstancedArrays
		, glCapsClearBuffer = True
		, glCapsArbDebugOutput = capArbDebugOutput
		, glCapsArbGetProgramBinary = capArbGetProgramBinary
		} GlslConfig
		{ glslConfigVersion = Just 330
		, glslConfigForceFloatAttributes = False
		, glslConfigUnsignedUnsupported = False
		, glslConfigUniformBlocks = capArbUniformBufferObject
		, glslConfigInOutSyntax = True
		, glslConfigTextureSampleDimensionSpecifier = False
		} (SomeBinaryCache programCache)

	-- set swap interval
	do
		-- try "late swap tearing"
		r <- SDL.glSetSwapInterval (-1)
		when (r /= 0) $ do
			-- didn't work, try usual vsync
			checkSdlError (== 0) $ SDL.glSetSwapInterval 1

	-- set front face mode
	glFrontFace GL_CW
	-- set cull mode
	glEnable GL_CULL_FACE
	glCullFace GL_BACK
	-- enable SRGB framebuffer
	glEnable GL_FRAMEBUFFER_SRGB
	glCheckErrors1 "init state"
	-- enable depth test
	glEnable GL_DEPTH_TEST

	-- if debug mode requested, setup debug output
	when (debug && capArbDebugOutput) $ do
		-- set debug message callback
		callbackPtr <- wrapGlDebugMessageCallback $ \messageSource messageType messageId messageSeverity messageLength messagePtr _userParam -> do
			-- unfortunately we cannot pattern match against gl_* constants here (as they are not patterns)
			let messageSourceStr = case messageSource of
				0x8246 -> "GL_DEBUG_SOURCE_API"
				0x8247 -> "GL_DEBUG_SOURCE_WINDOW_SYSTEM"
				0x8248 -> "GL_DEBUG_SOURCE_SHADER_COMPILER"
				0x8249 -> "GL_DEBUG_SOURCE_THIRD_PARTY"
				0x824A -> "GL_DEBUG_SOURCE_APPLICATION"
				0x824B -> "GL_DEBUG_SOURCE_OTHER"
				_ -> show messageSource
			let messageTypeStr = case messageType of
				0x824C -> "GL_DEBUG_TYPE_ERROR"
				0x824D -> "GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR"
				0x824E -> "GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR"
				0x824F -> "GL_DEBUG_TYPE_PORTABILITY"
				0x8250 -> "GL_DEBUG_TYPE_PERFORMANCE"
				0x8268 -> "GL_DEBUG_TYPE_MARKER"
				0x8269 -> "GL_DEBUG_TYPE_PUSH_GROUP"
				0x826A -> "GL_DEBUG_TYPE_POP_GROUP"
				0x8251 -> "GL_DEBUG_TYPE_OTHER"
				_ -> show messageType
			let messageSeverityStr = case messageSeverity of
				0x9146 -> "GL_DEBUG_SEVERITY_HIGH"
				0x9147 -> "GL_DEBUG_SEVERITY_MEDIUM"
				0x9148 -> "GL_DEBUG_SEVERITY_LOW"
				0x826B -> "GL_DEBUG_SEVERITY_NOTIFICATION"
				_ -> show messageSeverity
			message <- T.decodeUtf8 <$> B.unsafePackCStringLen (messagePtr, fromIntegral messageLength)
			putStrLn $ "*** OpenGL debug message ***" ++
				"\n  source:    " ++ messageSourceStr ++
				"\n  type:      " ++ messageTypeStr ++
				"\n  id:        " ++ show messageId ++
				"\n  severity:  " ++ messageSeverityStr ++
				"\n  message:   " ++ T.unpack message ++
				"\n*** EOM ***"
		glDebugMessageCallbackARB callbackPtr nullPtr
		-- enable asynchronous calls to callback
		glDisable GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB
		-- enable all debug messages
		glDebugMessageControlARB GL_DONT_CARE GL_DONT_CARE GL_DONT_CARE 0 nullPtr 1
		glCheckErrors1 "setup debug output"

	return ((context, presenter), openglInvoke presenter $ SDL.glDeleteContext sdlContext)

type GlDebugMessageCallback = GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGlDebugMessageCallback :: GlDebugMessageCallback -> IO (FunPtr GlDebugMessageCallback)

-- | Run in a thread of SDL window system.
openglInvoke :: OpenGLPresenter -> IO a -> IO a
openglInvoke OpenGLPresenter
	{ openglPresenterWindow = SdlWindow
		{ swSystem = windowSystem
		}
	} io = invokeSdlWindowSystem windowSystem io

isOpenGLExtensionSupported :: String -> IO Bool
isOpenGLExtensionSupported name = withCString name SDL.glExtensionSupported
