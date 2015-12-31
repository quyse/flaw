{-|
Module: Flaw.Graphics.WebGL
Description: WebGL graphics implementation.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.WebGL
	( WebGLSystem()
	, WebGLDevice()
	, WebGLContext()
	, WebGLPresenter()
	, webglInit
	, loadWebGLTexture2DFromURL
	) where

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Data.Array.IO as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.JSString.Text
import Data.IORef
import Data.String
import qualified Data.Text as T
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified GHCJS.Buffer
import qualified GHCJS.DOM.Element as DOM
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types

import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.GLSL
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.WebGL.FFI
import Flaw.Math

-- | Graphics system.
data WebGLSystem

instance System WebGLSystem where
	data DeviceId WebGLSystem
	data DisplayId WebGLSystem
	data DisplayModeId WebGLSystem
	getInstalledDevices _ = throwIO $ DescribeFirstException "not implemented"
	createDisplayMode _system _displayId _width _height = throwIO $ DescribeFirstException "not implemented"

-- | Device.
data WebGLDevice = WebGLDevice
	{ webglDeviceContext :: !JS_WebGLContext
	-- | Counter for ids for various resources.
	, webglDeviceCreateId :: !(IORef Int)
	}

data WebGLUniform = WebGLUniform
	{ webglUniformLocation :: !JS_WebGLUniformLocation
	, webglUniformInfo :: !Uniform
	}

instance Device WebGLDevice where
	type DeferredContext WebGLDevice = JSVal
	data TextureId WebGLDevice = WebGLTextureId
		{ webglTextureId :: !Int
		, webglTextureTexture :: !JS_WebGLTexture
		}
	data SamplerStateId WebGLDevice = WebGLSamplerStateId deriving Eq
	newtype RenderTargetId WebGLDevice = WebGLRenderTargetId JSVal
	newtype DepthStencilTargetId WebGLDevice = WebGLDepthStencilTargetId JSVal
	newtype FrameBufferId WebGLDevice = WebGLFrameBufferId JSVal
	data VertexBufferId WebGLDevice = WebGLVertexBufferId
		{ webglVertexBufferId :: !Int
		, webglVertexBufferBuffer :: !JS_WebGLBuffer
		, webglVertexBufferStride :: !Int
		}
	data IndexBufferId WebGLDevice = WebGLIndexBufferId
		{ webglIndexBufferId :: !Int
		, webglIndexBufferBuffer :: !JS_WebGLBuffer
		, webglIndexBufferMode :: !GLenum
		, webglIndexBufferFormat :: !GLenum
		}
	data ProgramId WebGLDevice = WebGLProgramId
		{ webglProgramId :: !Int
		, webglProgramProgram :: !JS_WebGLProgram
		, webglProgramAttributes :: ![Attribute]
		, webglProgramUniforms :: ![WebGLUniform]
		}
	data UniformBufferId WebGLDevice = WebGLUniformBufferId
		{ webglUniformBufferId :: !Int
		, webglUniformBufferPtr :: ForeignPtr ()
		}

	nullTexture = WebGLTextureId
		{ webglTextureId = 0
		, webglTextureTexture = JS_WebGLTexture nullRef
		}
	nullSamplerState = WebGLSamplerStateId
	nullDepthStencilTarget = WebGLDepthStencilTargetId nullRef
	nullIndexBuffer = WebGLIndexBufferId
		{ webglIndexBufferId = 0
		, webglIndexBufferBuffer = JS_WebGLBuffer nullRef
		, webglIndexBufferMode = webgl_TRIANGLES
		, webglIndexBufferFormat = webgl_UNSIGNED_SHORT
		}
	nullUniformBuffer = WebGLUniformBufferId
		{ webglUniformBufferId = 0
		, webglUniformBufferPtr = undefined
		}

	createDeferredContext _ = throwIO $ DescribeFirstException "not implemented"

	createStaticTexture _ _ _ _ = throwIO $ DescribeFirstException "not implemented"

	createSamplerState _device _samplerInfo = describeException "failed to create WebGL sampler state" $ do
		-- TODO
		return (WebGLSamplerStateId, undefined)

	createReadableRenderTarget _ _ _ _ _ = throwIO $ DescribeFirstException "not implemented"

	createDepthStencilTarget _ _ _ = throwIO $ DescribeFirstException "not implemented"

	createReadableDepthStencilTarget _ _ _ = throwIO $ DescribeFirstException "not implemented"

	createFrameBuffer _ _ _ = throwIO $ DescribeFirstException "not implemented"

	createStaticVertexBuffer device@WebGLDevice
		{ webglDeviceContext = jsContext
		} bytes stride = describeException "failed to create WebGL static vertex buffer" $ do
		bufferId <- webglAllocateId device
		jsBuffer <- js_createBuffer jsContext
		js_bindBuffer jsContext webgl_ARRAY_BUFFER jsBuffer
		jsDataBuffer <- byteStringToJsBuffer bytes
		js_bufferData jsContext webgl_ARRAY_BUFFER jsDataBuffer webgl_STATIC_DRAW
		return (WebGLVertexBufferId
			{ webglVertexBufferId = bufferId
			, webglVertexBufferBuffer = jsBuffer
			, webglVertexBufferStride = stride
			}, undefined)

	createStaticIndexBuffer device@WebGLDevice
		{ webglDeviceContext = jsContext
		} bytes is32Bit = describeException "failed to create WebGL static index buffer" $ do
		bufferId <- webglAllocateId device
		jsBuffer <- js_createBuffer jsContext
		js_bindBuffer jsContext webgl_ELEMENT_ARRAY_BUFFER jsBuffer
		jsDataBuffer <- byteStringToJsBuffer bytes
		js_bufferData jsContext webgl_ELEMENT_ARRAY_BUFFER jsDataBuffer webgl_STATIC_DRAW
		let format = if is32Bit then webgl_UNSIGNED_INT else webgl_UNSIGNED_SHORT
		return (WebGLIndexBufferId
			{ webglIndexBufferId = bufferId
			, webglIndexBufferBuffer = jsBuffer
			, webglIndexBufferMode = webgl_TRIANGLES
			, webglIndexBufferFormat = format
			}, undefined)

	createProgram device@WebGLDevice
		{ webglDeviceContext = jsContext
		} _binaryCache program = describeException "failed to create WebGL program" $ do

		let createShader source shaderType = describeException "failed to create WebGL shader" $ do
			jsShader <- js_createShader jsContext shaderType
			js_shaderSource jsContext jsShader $ textToJSString source
			js_compileShader jsContext jsShader
			jsStatus <- js_getShaderParameter jsContext jsShader webgl_COMPILE_STATUS
			if pFromJSVal jsStatus then return jsShader
			else do
				jsLog <- js_getShaderInfoLog jsContext jsShader
				putStrLn $ T.unpack source
				throwIO $ DescribeFirstException ("failed to compile shader", textFromJSString jsLog)

		-- generate GLSL
		GlslProgram
			{ glslProgramAttributes = attributes
			, glslProgramUniformBlocks = _uniformBlocks
			, glslProgramUniforms = uniforms
			, glslProgramSamplers = samplers
			, glslProgramFragmentTargets = _fragmentTargets
			, glslProgramShaders = shaders
			} <- liftM (glslGenerateProgram glslWebGLConfig) $ runProgram program

		-- create program
		jsProgram <- js_createProgram jsContext

		-- create and attach shaders
		forM_ shaders $ \(shaderStage, shaderSource) -> do
			shader <- createShader shaderSource $ case shaderStage of
				GlslVertexStage -> webgl_VERTEX_SHADER
				GlslFragmentStage -> webgl_FRAGMENT_SHADER
			js_attachShader jsContext jsProgram shader

		-- bind attributes
		forM_ (zip attributes [0..]) $ \(GlslAttribute
			{ glslAttributeName = name
			}, i) -> js_bindAttribLocation jsContext jsProgram i $ textToJSString name

		-- TODO: bind targets

		-- link program
		js_linkProgram jsContext jsProgram
		jsStatus <- js_getProgramParameter jsContext jsProgram webgl_LINK_STATUS
		if pFromJSVal jsStatus then return ()
		else throwIO $ DescribeFirstException "failed to link program"

		-- set as current
		js_useProgram jsContext jsProgram

		-- bind samplers
		forM_ (zip samplers [0..]) $ \(GlslSampler
			{ glslSamplerName = name
			}, i) -> do
			jsLocation <- js_getUniformLocation jsContext jsProgram $ textToJSString name
			js_uniform1i jsContext jsLocation i

		-- form uniforms
		programUniforms <- forM uniforms $ \GlslUniform
			{ glslUniformName = name
			, glslUniformInfo = info
			} -> do
			jsLocation <- js_getUniformLocation jsContext jsProgram $ textToJSString name
			return WebGLUniform
				{ webglUniformLocation = jsLocation
				, webglUniformInfo = info
				}

		programId <- webglAllocateId device
		return (WebGLProgramId
			{ webglProgramId = programId
			, webglProgramProgram = jsProgram
			, webglProgramAttributes = map glslAttributeInfo attributes
			, webglProgramUniforms = programUniforms
			}, undefined)

	createUniformBuffer device size = describeException "failed to create WebGL uniform buffer" $ do
		bufferId <- webglAllocateId device
		ptr <- mallocForeignPtrBytes size
		return (WebGLUniformBufferId
			{ webglUniformBufferId = bufferId
			, webglUniformBufferPtr = ptr
			}, undefined)

data WebGLContext = WebGLContext
	{ webglContextContext :: !JS_WebGLContext
	, webglContextActualState :: !WebGLContextState
	, webglContextDesiredState :: !WebGLContextState
	}

data WebGLContextState = WebGLContextState
	{ webglContextStateFrameBuffer :: !(IORef (FrameBufferId WebGLDevice))
	, webglContextStateViewport :: !(IORef Int4)
	, webglContextStateVertexBuffers :: !(IOArray Int (VertexBufferId WebGLDevice))
	, webglContextStateIndexBuffer :: !(IORef (IndexBufferId WebGLDevice))
	, webglContextStateUniformBuffers :: !(IOArray Int (UniformBufferId WebGLDevice))
	, webglContextStateSamplers :: !(IOArray Int (TextureId WebGLDevice, SamplerStateId WebGLDevice))
	, webglContextStateProgram :: !(IORef (ProgramId WebGLDevice))
	, webglContextStateAttributes :: !(IORef ([Attribute], VertexBufferId WebGLDevice))
	}

nullVertexBuffer :: VertexBufferId WebGLDevice
nullVertexBuffer = WebGLVertexBufferId
	{ webglVertexBufferId = 0
	, webglVertexBufferBuffer = JS_WebGLBuffer nullRef
	, webglVertexBufferStride = 0
	}

webglCreateContextState :: IO WebGLContextState
webglCreateContextState = do
	frameBuffer <- newIORef $ WebGLFrameBufferId nullRef
	viewport <- newIORef $ Int4 0 0 0 0
	vertexBuffers <- A.newArray (0, 7) nullVertexBuffer
	indexBuffer <- newIORef $ WebGLIndexBufferId
		{ webglIndexBufferId = 0
		, webglIndexBufferBuffer = JS_WebGLBuffer nullRef
		, webglIndexBufferMode = webgl_TRIANGLES
		, webglIndexBufferFormat = webgl_UNSIGNED_SHORT
		}
	uniformBuffers <- A.newArray (0, 7) $ WebGLUniformBufferId
		{ webglUniformBufferId = 0
		, webglUniformBufferPtr = undefined
		}
	samplers <- A.newArray (0, 7) (WebGLTextureId
		{ webglTextureId = 0
		, webglTextureTexture = JS_WebGLTexture nullRef
		}, WebGLSamplerStateId)
	program <- newIORef $ WebGLProgramId
		{ webglProgramId = 0
		, webglProgramProgram = JS_WebGLProgram nullRef
		, webglProgramAttributes = []
		, webglProgramUniforms = []
		}
	attributes <- newIORef ([], nullVertexBuffer)
	return WebGLContextState
		{ webglContextStateFrameBuffer = frameBuffer
		, webglContextStateViewport = viewport
		, webglContextStateVertexBuffers = vertexBuffers
		, webglContextStateIndexBuffer = indexBuffer
		, webglContextStateUniformBuffers = uniformBuffers
		, webglContextStateSamplers = samplers
		, webglContextStateProgram = program
		, webglContextStateAttributes = attributes
		}

webglSetDefaultContextState :: WebGLContextState -> IO ()
webglSetDefaultContextState WebGLContextState
	{ webglContextStateFrameBuffer = frameBufferRef
	, webglContextStateViewport = viewportRef
	, webglContextStateVertexBuffers = vertexBuffersArray
	, webglContextStateIndexBuffer = indexBufferRef
	, webglContextStateUniformBuffers = uniformBuffersArray
	, webglContextStateSamplers = samplersArray
	, webglContextStateProgram = programRef
	, webglContextStateAttributes = attributesRef
	} = do
	writeIORef frameBufferRef $ WebGLFrameBufferId nullRef
	writeIORef viewportRef $ Int4 0 0 0 0
	vertexBuffersBounds <- getBounds vertexBuffersArray
	forM_ (range vertexBuffersBounds) $ \i -> writeArray vertexBuffersArray i nullVertexBuffer
	writeIORef indexBufferRef $ WebGLIndexBufferId
		{ webglIndexBufferId = 0
		, webglIndexBufferBuffer = JS_WebGLBuffer nullRef
		, webglIndexBufferMode = webgl_TRIANGLES
		, webglIndexBufferFormat = webgl_UNSIGNED_SHORT
		}
	uniformBuffersBounds <- getBounds uniformBuffersArray
	forM_ (range uniformBuffersBounds) $ \i -> writeArray uniformBuffersArray i WebGLUniformBufferId
		{ webglUniformBufferId = 0
		, webglUniformBufferPtr = undefined
		}
	samplersBounds <- getBounds samplersArray
	forM_ (range samplersBounds) $ \i -> writeArray samplersArray i (WebGLTextureId
		{ webglTextureId = 0
		, webglTextureTexture = JS_WebGLTexture nullRef
		} , WebGLSamplerStateId)
	writeIORef programRef $ WebGLProgramId
		{ webglProgramId = 0
		, webglProgramProgram = JS_WebGLProgram nullRef
		, webglProgramAttributes = []
		, webglProgramUniforms = []
		}
	writeIORef attributesRef ([], nullVertexBuffer)

instance Context WebGLContext WebGLDevice where
	contextClearColor context@WebGLContext
		{ webglContextContext = jsContext
		} _targetIndex (Vec4 r g b a) = do
		webglUpdateFrameBuffer context
		js_clearColor jsContext r g b a
		js_clear jsContext webgl_COLOR_BUFFER_BIT

	contextClearDepth context@WebGLContext
		{ webglContextContext = jsContext
		} depth = do
		webglUpdateFrameBuffer context
		js_clearDepth jsContext depth
		js_clear jsContext webgl_DEPTH_BUFFER_BIT

	contextClearStencil context@WebGLContext
		{ webglContextContext = jsContext
		} stencil = do
		webglUpdateFrameBuffer context
		js_clearStencil jsContext stencil
		js_clear jsContext webgl_STENCIL_BUFFER_BIT

	contextClearDepthStencil context@WebGLContext
		{ webglContextContext = jsContext
		} depth stencil = do
		webglUpdateFrameBuffer context
		js_clearDepth jsContext depth
		js_clearStencil jsContext stencil
		js_clear jsContext $ webgl_DEPTH_BUFFER_BIT .|. webgl_STENCIL_BUFFER_BIT

	contextUploadUniformBuffer _context WebGLUniformBufferId
		{ webglUniformBufferPtr = foreignPtr
		} bytes = withForeignPtr foreignPtr $ \bufferPtr -> B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
		copyBytes bufferPtr (castPtr bytesPtr) bytesLen

	contextDraw context@WebGLContext
		{ webglContextContext = jsContext
		, webglContextDesiredState = WebGLContextState
			{ webglContextStateIndexBuffer = indexBufferRef
			}
		} instancesCount indicesCount = do
		webglUpdateContext context
		WebGLIndexBufferId
			{ webglIndexBufferBuffer = JS_WebGLBuffer jsBufferVal
			, webglIndexBufferMode = mode
			, webglIndexBufferFormat = format
			} <- readIORef indexBufferRef
		if instancesCount > 1 then
			if isNull jsBufferVal then js_drawArraysInstanced jsContext mode 0 indicesCount instancesCount
			else js_drawElementsInstanced jsContext mode indicesCount format 0 instancesCount
		else
			if isNull jsBufferVal then js_drawArrays jsContext mode 0 indicesCount
			else js_drawElements jsContext mode indicesCount format 0

	contextPlay = undefined

	contextRender WebGLContext
		{ webglContextDesiredState = desiredContextState
		} f = do
		webglSetDefaultContextState desiredContextState
		f

	contextSetFrameBuffer WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateFrameBuffer = frameBufferRef
			}
		} frameBuffer scope = do
		oldFrameBuffer <- readIORef frameBufferRef
		writeIORef frameBufferRef frameBuffer
		r <- scope
		writeIORef frameBufferRef oldFrameBuffer
		return r

	contextSetViewport WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateViewport = viewportRef
			}
		} viewport scope = do
		oldViewport <- readIORef viewportRef
		writeIORef viewportRef viewport
		r <- scope
		writeIORef viewportRef oldViewport
		return r

	contextGetViewport WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateViewport = viewportRef
			}
		} = readIORef viewportRef

	contextSetVertexBuffer WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateVertexBuffers = vertexBuffersArray
			, webglContextStateAttributes = attributesRef
			}
		} i vertexBuffer scope = do
		oldVertexBuffer <- readArray vertexBuffersArray i
		writeArray vertexBuffersArray i vertexBuffer
		oldAttributes@(as, _oldVertexBuffer)  <- readIORef attributesRef
		writeIORef attributesRef (as, vertexBuffer)
		r <- scope
		writeArray vertexBuffersArray i oldVertexBuffer
		writeIORef attributesRef oldAttributes
		return r

	contextSetIndexBuffer WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateIndexBuffer = indexBufferRef
			}
		} indexBuffer scope = do
		oldIndexBuffer <- readIORef indexBufferRef
		writeIORef indexBufferRef indexBuffer
		r <- scope
		writeIORef indexBufferRef oldIndexBuffer
		return r

	contextSetUniformBuffer WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateUniformBuffers = uniformBuffersArray
			}
		} i uniformBuffer scope = do
		oldUniformBuffer <- readArray uniformBuffersArray i
		writeArray uniformBuffersArray i uniformBuffer
		r <- scope
		writeArray uniformBuffersArray i oldUniformBuffer
		return r

	contextSetSampler WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateSamplers = samplersArray
			}
		} i texture samplerState scope = do
		oldSampler <- readArray samplersArray i
		writeArray samplersArray i (texture, samplerState)
		r <- scope
		writeArray samplersArray i oldSampler
		return r

	contextSetProgram WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateProgram = programRef
			, webglContextStateAttributes = attributesRef
			}
		} program scope = do
		oldProgram <- readIORef programRef
		writeIORef programRef program
		oldAttributes@(_oldAs, stride) <- readIORef attributesRef
		writeIORef attributesRef
			( case program of
				WebGLProgramId
					{ webglProgramAttributes = as
					} -> as
			, stride
			)
		r <- scope
		writeIORef programRef oldProgram
		writeIORef attributesRef oldAttributes
		return r

data WebGLPresenter = WebGLPresenter
	{ webglPresenterCanvas :: !DOM.Element
	}

instance Presenter WebGLPresenter WebGLSystem WebGLContext WebGLDevice where
	setPresenterMode _presenter _maybeDisplayMode = throwIO $ DescribeFirstException "not implemented"

	presenterRender WebGLPresenter
		{ webglPresenterCanvas = canvas
		} WebGLContext
		{ webglContextDesiredState = WebGLContextState
			{ webglContextStateFrameBuffer = frameBufferRef
			, webglContextStateViewport = viewportRef
			}
		} f = do
		-- create sync var
		syncVar <- newEmptyMVar

		-- create sync callback
		callback <- syncCallback ThrowWouldBlock $ do
			-- set framebuffer
			writeIORef frameBufferRef $ WebGLFrameBufferId nullRef
			-- get client size
			width <- DOM.getClientWidth canvas
			height <- DOM.getClientHeight canvas
			-- set viewport
			writeIORef viewportRef $ Int4 0 0 (floor width) (floor height)

			-- perform render
			putMVar syncVar =<< f

		-- perform rendering
		js_requestAnimationFrame callback

		-- wait for the end
		r <- takeMVar syncVar

		-- release data associated with callback
		releaseCallback callback

		return r

-- | Wrapper for window.requestAnimationFrame.
foreign import javascript unsafe "( \
	\ window.requestAnimationFrame || \
	\ window.mozRequestAnimationFrame || \
	\ window.webkitRequestAnimationFrame || \
	\ window.msRequestAnimationFrame \
	\ )($1);" js_requestAnimationFrame :: Callback (IO ()) -> IO ()

webglInit :: DOM.Element -> Bool -> IO ((WebGLDevice, WebGLContext, WebGLPresenter), IO ())
webglInit jsCanvas needDepth = do
	-- get context
	jsContext@(JS_WebGLContext jsContextVal) <- js_getWebGLContext jsCanvas needDepth
	if isNull jsContextVal then throwIO $ DescribeFirstException "cannot get WebGL context"
	else return ()
	-- create device
	createIdRef <- newIORef 1
	let device = WebGLDevice
		{ webglDeviceContext = jsContext
		, webglDeviceCreateId = createIdRef
		}
	-- create context
	actualContextState <- webglCreateContextState
	desiredContextState <- webglCreateContextState
	let context = WebGLContext
		{ webglContextContext = jsContext
		, webglContextActualState = actualContextState
		, webglContextDesiredState = desiredContextState
		}
	-- set front face mode
	js_frontFace jsContext webgl_CW
	-- enable culling
	js_enable jsContext webgl_CULL_FACE
	js_cullFace jsContext webgl_BACK
	-- enable depth test
	js_enable jsContext webgl_DEPTH_TEST

	-- enable extensions
	let extensions =
		[ "OES_texture_float"
		, "OES_texture_half_float"
		, "OES_standard_derivatives"
		, "OES_vertex_array_object"
		, "WEBGL_compressed_texture_s3tc"
		, "WEBGL_depth_texture"
		, "OES_element_index_uint"
		, "EXT_texture_filter_anisotropic"
		, "ANGLE_instanced_arrays"
		, "OES_texture_float_linear"
		, "OES_texture_half_float_linear"
		, "WEBGL_compressed_texture_atc"
		, "WEBGL_compressed_texture_pvrtc"
		, "EXT_color_buffer_half_float"
		, "WEBGL_color_buffer_float"
		, "EXT_frag_depth"
		, "EXT_sRGB"
		, "WEBGL_draw_buffers"
		, "WEBGL_shared_resources"
		, "EXT_shader_texture_lod"

		, "WEBGL_compressed_texture_s3tc"
		, "EXT_texture_filter_anisotropic"
		, "OES_texture_float"
		, "ANGLE_instanced_arrays"
		, "OES_vertex_array_object"
		, "WEBGL_draw_buffers"
		]
	forM_ extensions $ js_getExtension jsContext . fromString

	-- create presenter
	let presenter = WebGLPresenter
		{ webglPresenterCanvas = jsCanvas
		}

	-- return
	return ((device, context, presenter), return ())

webglAllocateId :: WebGLDevice -> IO Int
webglAllocateId WebGLDevice
	{ webglDeviceCreateId = createIdRef
	} = do
	newId <- readIORef createIdRef
	writeIORef createIdRef $ newId + 1
	return newId

webglUpdateFrameBuffer :: WebGLContext -> IO ()
webglUpdateFrameBuffer _ = do
	-- TODO: supporting only default framebuffer for now
	return ()

-- | Update context.
webglUpdateContext :: WebGLContext -> IO ()
webglUpdateContext WebGLContext
	{ webglContextContext = jsContext
	, webglContextActualState = WebGLContextState
		{ webglContextStateFrameBuffer = actualFrameBufferRef
		, webglContextStateViewport = actualViewportRef
		, webglContextStateVertexBuffers = actualVertexBuffersArray
		, webglContextStateIndexBuffer = actualIndexBufferRef
		, webglContextStateUniformBuffers = actualUniformBuffersArray
		, webglContextStateSamplers = actualSamplersArray
		, webglContextStateProgram = actualProgramRef
		, webglContextStateAttributes = actualAttributesRef
		}
	, webglContextDesiredState = WebGLContextState
		{ webglContextStateFrameBuffer = desiredFrameBufferRef
		, webglContextStateViewport = desiredViewportRef
		, webglContextStateVertexBuffers = desiredVertexBuffersArray
		, webglContextStateIndexBuffer = desiredIndexBufferRef
		, webglContextStateUniformBuffers = desiredUniformBuffersArray
		, webglContextStateSamplers = desiredSamplersArray
		, webglContextStateProgram = desiredProgramRef
		, webglContextStateAttributes = desiredAttributesRef
		}
	} = do

	let
		refSetup :: Eq a => IORef a -> IORef a -> (a -> IO ()) -> IO ()
		refSetup actualRef desiredRef setup = do
			actual <- readIORef actualRef
			desired <- readIORef desiredRef
			if actual /= desired then do
				setup desired
				writeIORef actualRef desired
			else return ()

	let
		arraySetup :: Eq a => IOArray Int a -> IOArray Int a -> ([a] -> IO ()) -> IO ()
		arraySetup actualArray desiredArray setup = do
			actual <- getElems actualArray
			desired <- getElems desiredArray
			if actual /= desired then do
				setup desired
				bounds <- getBounds desiredArray
				forM_ (range bounds) $ \i -> writeArray actualArray i =<< readArray desiredArray i
			else return ()

	-- framebuffer
	-- TODO: supporting only default framebuffer for now

	-- viewport
	refSetup actualViewportRef desiredViewportRef $ \(Int4 viewportLeft viewportTop viewportRight viewportBottom) -> do
		js_viewport jsContext viewportLeft viewportTop (viewportRight - viewportLeft) (viewportBottom - viewportTop)

	-- program
	refSetup actualProgramRef desiredProgramRef $ \desiredProgram -> do
		case desiredProgram of
			WebGLProgramId
				{ webglProgramProgram = jsProgram
				} -> do
				-- bind program
				js_useProgram jsContext jsProgram

	-- setup uniforms unconditionally
	WebGLProgramId
		{ webglProgramUniforms = uniforms
		} <- readIORef desiredProgramRef
	forM_ uniforms $ \WebGLUniform
		{ webglUniformLocation = jsLocation
		, webglUniformInfo = Uniform
			{ uniformSlot = slot
			, uniformOffset = offset
			, uniformSize = size
			, uniformType = t
			}
		} -> do
		WebGLUniformBufferId
			{ webglUniformBufferPtr = foreignPtr
			} <- readArray desiredUniformBuffersArray slot
		let count = if size > 0 then size else 1
		withForeignPtr foreignPtr $ \bufferPtr -> case t of
			ScalarValueType ScalarFloat -> do
				js_uniform1fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) count
			ScalarValueType ScalarInt -> do
				js_uniform1iv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) count
			VectorValueType Dimension1 ScalarFloat -> do
				js_uniform1fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) count
			VectorValueType Dimension2 ScalarFloat -> do
				js_uniform2fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) (count * 2)
			VectorValueType Dimension3 ScalarFloat -> do
				js_uniform3fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) (count * 3)
			VectorValueType Dimension4 ScalarFloat -> do
				js_uniform4fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) (count * 4)
			MatrixValueType Dimension4 Dimension4 ScalarFloat -> do
				js_uniformMatrix4fv jsContext jsLocation =<< js_ptrToFloat32Array (bufferPtr `plusPtr` offset) (count * 16)
			_ -> throwIO $ DescribeFirstException ("wrong WebGL uniform type", t)

	-- uniform buffers
	arraySetup actualUniformBuffersArray desiredUniformBuffersArray $ \desiredUniformBuffers -> do
		-- nothing to do (they are faked with uniforms)
		return ()

	-- samplers
	arraySetup actualSamplersArray desiredSamplersArray $ \desiredSamplers -> do
		forM_ (zip desiredSamplers [0..]) $ \((WebGLTextureId
			{ webglTextureId = textureId
			, webglTextureTexture = jsTexture
			}, _samplerState), i) -> do
			if textureId > 0 then do
				js_activeTexture jsContext $ webgl_TEXTURE0 + i
				js_bindTexture jsContext webgl_TEXTURE_2D jsTexture
			else return ()

	-- vertex buffers
	arraySetup actualVertexBuffersArray desiredVertexBuffersArray $ \desiredVertexBuffers -> do
		case head desiredVertexBuffers of
			WebGLVertexBufferId
				{ webglVertexBufferBuffer = jsBuffer
				} -> do
				js_bindBuffer jsContext webgl_ARRAY_BUFFER jsBuffer

	-- index buffer
	refSetup actualIndexBufferRef desiredIndexBufferRef $ \WebGLIndexBufferId
		{ webglIndexBufferBuffer = jsBuffer
		} -> do
		js_bindBuffer jsContext webgl_ELEMENT_ARRAY_BUFFER jsBuffer

	-- attributes
	refSetup actualAttributesRef desiredAttributesRef $ \(attributes, WebGLVertexBufferId
		{ webglVertexBufferStride = stride
		}) -> do
		forM_ (zip attributes [0..]) $ \(Attribute
			--{ attributeSlot = slot -- TODO multiple slots
			{ attributeOffset = offset
			--, attributeDivisor = divisor -- TODO attribute instancing
			, attributeType = at
			}, i) -> do
			js_enableVertexAttribArray jsContext i
			let normalized n = case n of
				NonNormalized -> False
				Normalized -> True
			let stn q = case q of
				ATFloat32 -> (1, webgl_FLOAT, False)
				ATInt32 n -> (1, webgl_INT, normalized n)
				ATInt16 n -> (1, webgl_SHORT, normalized n)
				ATInt8 n -> (1, webgl_BYTE, normalized n)
				ATUint32 n -> (1, webgl_UNSIGNED_INT, normalized n)
				ATUint16 n -> (1, webgl_UNSIGNED_SHORT, normalized n)
				ATUint8 n -> (1, webgl_UNSIGNED_BYTE, normalized n)
				ATVec1 a -> let (_, t, n) = stn a in (1, t, n)
				ATVec2 a -> let (_, t, n) = stn a in (2, t, n)
				ATVec3 a -> let (_, t, n) = stn a in (3, t, n)
				ATVec4 a -> let (_, t, n) = stn a in (4, t, n)
				_ -> error $ show ("wrong WebGL attribute type", at)
			let (aSize, aType, aNormalized) = stn at
			js_vertexAttribPointer jsContext i aSize aType aNormalized stride offset

loadWebGLTexture2DFromURL :: WebGLDevice -> T.Text -> IO (TextureId WebGLDevice, IO ())
loadWebGLTexture2DFromURL device@WebGLDevice
	{ webglDeviceContext = jsContext
	} url = describeException "failed to load WebGL texture from URL" $ do
	image <- js_loadImage $ textToJSString url
	jsTexture <- js_createTexture jsContext
	js_bindTexture jsContext webgl_TEXTURE_2D jsTexture
	js_texImage2D jsContext webgl_TEXTURE_2D 0 webgl_RGBA webgl_RGBA webgl_UNSIGNED_BYTE image
	js_texParameteri jsContext webgl_TEXTURE_2D webgl_TEXTURE_WRAP_S $ fromIntegral webgl_REPEAT
	js_texParameteri jsContext webgl_TEXTURE_2D webgl_TEXTURE_WRAP_T $ fromIntegral webgl_REPEAT
	js_texParameteri jsContext webgl_TEXTURE_2D webgl_TEXTURE_MIN_FILTER $ fromIntegral webgl_LINEAR
	js_texParameteri jsContext webgl_TEXTURE_2D webgl_TEXTURE_MAG_FILTER $ fromIntegral webgl_LINEAR
	textureId <- webglAllocateId device
	return (WebGLTextureId
		{ webglTextureId = textureId
		, webglTextureTexture = jsTexture
		}, return ())

byteStringToJsBuffer :: B.ByteString -> IO JSVal
byteStringToJsBuffer bytes = do
	let (buf, off, len) = GHCJS.Buffer.fromByteString bytes
	r <- js_unwrapBuf buf off len
	return r
foreign import javascript unsafe "$r = new Uint8Array($1.buf, $2, $3)" js_unwrapBuf :: GHCJS.Buffer.Buffer -> Int -> Int -> IO JSVal

foreign import javascript unsafe "$r = $1.f3.subarray($1_2, $1_2 + $2)" js_ptrToFloat32Array :: Ptr () -> Int -> IO JSVal

instance Eq (VertexBufferId WebGLDevice) where
	WebGLVertexBufferId { webglVertexBufferId = a } == WebGLVertexBufferId { webglVertexBufferId = b } = a == b

instance Eq (IndexBufferId WebGLDevice) where
	WebGLIndexBufferId { webglIndexBufferId = a } == WebGLIndexBufferId { webglIndexBufferId = b } = a == b

instance Eq (UniformBufferId WebGLDevice) where
	WebGLUniformBufferId { webglUniformBufferId = a } == WebGLUniformBufferId { webglUniformBufferId = b } = a == b

instance Eq (ProgramId WebGLDevice) where
	WebGLProgramId { webglProgramId = a } == WebGLProgramId { webglProgramId = b } = a == b

instance Eq (TextureId WebGLDevice) where
	WebGLTextureId { webglTextureId = a } == WebGLTextureId { webglTextureId = b } = a == b
