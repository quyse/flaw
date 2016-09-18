{-|
Module: Flaw.Graphics.WebGL
Description: WebGL graphics implementation.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.WebGL
	( WebGLSystem()
	, WebGLDevice
	, WebGLContext
	, WebGLPresenter()
	, webglInit
	) where

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.String
import GHCJS.Foreign
import GHCJS.Foreign.Callback

import Flaw.BinaryCache
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.GlContext
import Flaw.Graphics.GLSL
import Flaw.Graphics.WebGL.FFI
import Flaw.Math
import Flaw.Window
import Flaw.Window.Web.Canvas

-- | Graphics system.
data WebGLSystem = WebGLSystem

instance System WebGLSystem where
	data DeviceId WebGLSystem
	data DisplayId WebGLSystem
	data DisplayModeId WebGLSystem
	getInstalledDevices _ = throwIO $ DescribeFirstException "not implemented"
	createDisplayMode _system _displayId _width _height = throwIO $ DescribeFirstException "not implemented"

type WebGLDevice = GlContext
type WebGLContext = GlContext

data WebGLPresenter = WebGLPresenter
	{ webglPresenterCanvas :: !Canvas
	, webglPresenterContext :: !JS_WebGLContext
	}

instance Presenter WebGLPresenter WebGLSystem GlContext GlContext where
	setPresenterMode _presenter _maybeDisplayMode = throwIO $ DescribeFirstException "not implemented"

	presenterRender WebGLPresenter
		{ webglPresenterCanvas = canvas
		, webglPresenterContext = context
		} GlContext
		{ glContextDesiredState = desiredContextState@GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} f = do
		-- create sync var
		syncVar <- newEmptyMVar

		-- create sync callback
		callback <- syncCallback ThrowWouldBlock $ do
			-- clear state
			glSetDefaultContextState desiredContextState

			-- get client size
			(width, height) <- getWindowClientSize canvas
			-- set current context
			js_setContext context
			-- setup state
			writeIORef frameBufferRef GlFrameBufferId
				{ glFrameBufferName = glNullFramebufferName
				, glFrameBufferWidth = width
				, glFrameBufferHeight = height
				}
			writeIORef viewportRef $ Vec4 0 0 width height

			-- perform render
			putMVar syncVar =<< f

		-- perform rendering
		js_requestAnimationFrame callback

		-- wait for the end
		r <- takeMVar syncVar

		-- release data associated with callback
		releaseCallback callback

		return r

webglInit :: Canvas -> Bool -> IO ((WebGLSystem, WebGLDevice, WebGLContext, WebGLPresenter), IO ())
webglInit canvas@Canvas
	{ canvasElement = jsCanvas
	} needDepth = do
	-- get context
	jsContext@(JS_WebGLContext jsContextVal) <- js_getCanvasContext jsCanvas needDepth
	when (isNull jsContextVal) $ throwIO $ DescribeFirstException "cannot get WebGL context"
	-- create context
	context <- newGlContext id id GlCaps
		{ glCapsUniformBufferObject = False
		, glCapsSamplerObjects = False
		, glCapsVertexArrayObject = False
		, glCapsVertexAttribBinding = False
		, glCapsFramebufferObject = True
		, glCapsTextureStorage = False
		, glCapsInstancedArrays = False
		, glCapsClearBuffer = False
		, glCapsDebugOutput = False
		, glCapsGetProgramBinary = False
		} GlslConfig
		{ glslConfigVersion = Nothing
		, glslConfigForceFloatAttributes = True
		, glslConfigUnsignedUnsupported = True
		, glslConfigUniformBlocks = False
		, glslConfigInOutSyntax = False
		, glslConfigTextureSampleDimensionSpecifier = True
		} (SomeBinaryCache NullBinaryCache)
	let device = context

	-- set context as current
	js_setContext jsContext

	-- set some defaults
	-- set front face mode
	glFrontFace GL_CW
	-- enable culling
	glEnable GL_CULL_FACE
	glCullFace GL_BACK
	-- enable depth test
	glEnable GL_DEPTH_TEST

	-- enable extensions
	let extensions =
		[ "ANGLE_instanced_arrays"
		, "EXT_color_buffer_half_float"
		, "EXT_frag_depth"
		, "EXT_shader_texture_lod"
		, "EXT_sRGB"
		, "EXT_texture_filter_anisotropic"
		, "OES_element_index_uint"
		, "OES_standard_derivatives"
		, "OES_texture_float"
		, "OES_texture_float_linear"
		, "OES_texture_half_float"
		, "OES_texture_half_float_linear"
		--, "OES_vertex_array_object" -- better to not enable it, as we don't really support it right now in WebGL
		, "WEBGL_color_buffer_float"
		, "WEBGL_compressed_texture_atc"
		, "WEBGL_compressed_texture_pvrtc"
		, "WEBGL_compressed_texture_s3tc"
		, "WEBGL_depth_texture"
		, "WEBGL_draw_buffers"
		, "WEBGL_shared_resources"
		]
	forM_ extensions $ glGetExtension . fromString

	-- create presenter
	let presenter = WebGLPresenter
		{ webglPresenterCanvas = canvas
		, webglPresenterContext = jsContext
		}

	-- return
	return ((WebGLSystem, device, context, presenter), return ())
