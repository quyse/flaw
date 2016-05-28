{-|
Module: Flaw.Graphics.OpenGL
Description: OpenGL helper functions.
License: MIT
-}

{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Flaw.Graphics.OpenGL
	( createOpenGLContext
	) where

import Control.Monad
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.ARB.DebugOutput
import Graphics.GL.Core33

import Flaw.BinaryCache
import Flaw.Graphics.GlContext
import Flaw.Graphics.GLSL

-- | Create OpenGL context and perform basic initialization.
createOpenGLContext :: BinaryCache c => c -> (forall a. IO a -> IO a) -> Bool -> IO GlContext
createOpenGLContext programCache invoke debug = do
	-- init capabilities
	numExtensions <- alloca $ \numExtensionsPtr -> do
		glGetIntegerv GL_NUM_EXTENSIONS numExtensionsPtr
		peek numExtensionsPtr
	extensions <- let
		getExtensions i restExtensions
			| i >= 0 = getExtensions (i - 1) =<< (: restExtensions) <$> (B.unsafePackCString . castPtr =<< glGetStringi GL_EXTENSIONS (fromIntegral i))
			| otherwise = return restExtensions
		in getExtensions (numExtensions - 1) []

	let
		isExtensionSupported = flip elem extensions
		capUniformBufferObject = isExtensionSupported "GL_ARB_uniform_buffer_object"
		capSamplerObjects      = isExtensionSupported "GL_ARB_sampler_objects"
		capVertexAttribBinding = isExtensionSupported "GL_ARB_vertex_attrib_binding"
		capFramebufferObject   = isExtensionSupported "GL_ARB_framebuffer_object"
		capTextureStorage      = isExtensionSupported "GL_ARB_texture_storage"
		capInstancedArrays     = isExtensionSupported "GL_ARB_instanced_arrays"
		capDebugOutput         = isExtensionSupported "GL_ARB_debug_output"
		capGetProgramBinary    = isExtensionSupported "GL_ARB_get_program_binary"

	context <- newGlContext invoke GlCaps
		{ glCapsUniformBufferObject = capUniformBufferObject
		, glCapsSamplerObjects = capSamplerObjects
		, glCapsVertexAttribBinding = capVertexAttribBinding
		, glCapsFramebufferObject = capFramebufferObject
		, glCapsTextureStorage = capTextureStorage
		, glCapsInstancedArrays = capInstancedArrays
		, glCapsClearBuffer = True
		, glCapsDebugOutput = capDebugOutput
		, glCapsGetProgramBinary = capGetProgramBinary
		} GlslConfig
		{ glslConfigVersion = Just 330
		, glslConfigForceFloatAttributes = False
		, glslConfigUnsignedUnsupported = False
		, glslConfigUniformBlocks = capUniformBufferObject
		, glslConfigInOutSyntax = True
		, glslConfigTextureSampleDimensionSpecifier = False
		} (SomeBinaryCache programCache)

	-- set front face mode
	glFrontFace GL_CW
	-- set cull mode
	glEnable GL_CULL_FACE
	glCullFace GL_BACK
	-- enable SRGB framebuffer
	glEnable GL_FRAMEBUFFER_SRGB
	-- enable depth test
	glEnable GL_DEPTH_TEST
	glCheckErrors1 "init state"

	-- if debug mode requested, setup debug output
	when (debug && capDebugOutput) $ do
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

	return context

type GlDebugMessageCallback = GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGlDebugMessageCallback :: GlDebugMessageCallback -> IO (FunPtr GlDebugMessageCallback)
