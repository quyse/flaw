{-|
Module: Flaw.Graphics.OpenGL
Description: OpenGL graphics implementation.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.OpenGL
	( GlSystem()
	, GlDevice()
	, GlContext()
	, GlPresenter()
	, createGlSystem
	, createGlContext
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Coerce
import Data.List
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.ARB.TextureStorage
import Graphics.Rendering.OpenGL.Raw.ARB.UniformBufferObject
import Graphics.Rendering.OpenGL.Raw.Core43
import Graphics.Rendering.OpenGL.Raw.EXT.TextureCompressionS3TC
import Graphics.Rendering.OpenGL.Raw.EXT.TextureSRGB
import qualified SDL.Raw.Types as SDL
import qualified SDL.Raw.Video as SDL

import Flaw.Book
import Flaw.Exception
import Flaw.Graphics.Blend
import Flaw.Graphics.GLSL
import Flaw.Graphics.Internal
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.Texture
import Flaw.Sdl
import Flaw.Window.Sdl

data GlSystem = GlSystem

instance System GlSystem where
	newtype DeviceId GlSystem = GlDeviceId Int
	newtype DisplayId GlSystem = GlDisplayId Int
	newtype DisplayModeId GlSystem = GlDisplayModeId SDL.DisplayMode

	getInstalledDevices GlSystem = do
		-- get video drivers
		driversCount <- SDL.getNumVideoDrivers
		drivers <- forM [0..(driversCount - 1)] $ \i -> do
			name <- liftM T.decodeUtf8 (B.unsafePackCString =<< SDL.getVideoDriver i)
			return (i, name)

		-- get displays
		displaysCount <- SDL.getNumVideoDisplays
		displays <- forM [0..(displaysCount - 1)] $ \i -> do
			name <- liftM T.decodeUtf8 (B.unsafePackCString =<< SDL.getDisplayName i)
			modesCount <- SDL.getNumDisplayModes i
			modes <- alloca $ \modePtr -> forM [0..(modesCount - 1)] $ \j -> do
				checkSdlError (== 0) $ SDL.getDisplayMode i j modePtr
				mode@SDL.DisplayMode
					{ SDL.displayModeW = width
					, SDL.displayModeH = height
					, SDL.displayModeRefreshRate = refreshRate
					} <- peek modePtr
				return (GlDisplayModeId mode, DisplayModeInfo
					{ displayModeName = T.pack $ "SDL " ++ (show width) ++ "x" ++ (show height) ++ ", " ++ (show refreshRate) ++ " Hz"
					, displayModeWidth = fromIntegral width
					, displayModeHeight = fromIntegral height
					, displayModeRefreshRate = fromIntegral refreshRate
					})
			return (GlDisplayId $ fromIntegral i, DisplayInfo
				{ displayName = name
				, displayModes = modes
				})

		return ((flip map) drivers $ \(i, name) -> (GlDeviceId $ fromIntegral i, DeviceInfo
			{ deviceName = name
			, deviceDisplays = displays
			}), return ())

	createDisplayMode _ _ _ _ = undefined

-- | OpenGL context is a Device, Context and Presenter simultaneously.
data GlContext = GlContext
	{ glContextContext :: !SDL.GLContext
	, glContextCaps :: !GlCaps
	, glContextWindow :: SdlWindow
	, glContextActualState :: GlContextState
	, glContextDesiredState :: GlContextState
	-- | Number of manually bound attributes.
	, glContextBoundAttributesCount :: !(IORef Int)
	}

data GlContextState = GlContextState
	{ glContextStateFrameBuffer :: !(IORef (FrameBufferId GlDevice))
	, glContextStateViewport :: !(IORef (Int, Int))
	, glContextStateVertexBuffers :: !(VM.IOVector (VertexBufferId GlDevice))
	, glContextStateIndexBuffer :: !(IORef (IndexBufferId GlDevice))
	, glContextStateUniformBuffers :: !(VM.IOVector (UniformBufferId GlDevice))
	, glContextStateSamplers :: !(VM.IOVector (TextureId GlDevice, SamplerStateId GlDevice))
	, glContextStateProgram :: !(IORef (ProgramId GlDevice))
	, glContextStateDepthTestFunc :: !(IORef DepthTestFunc)
	, glContextStateDepthWrite :: !(IORef Bool)
	, glContextStateBlendState :: !(IORef (BlendStateId GlDevice))
	}

type GlDevice = GlContext

data GlAttributeSlot = GlAttributeSlot
	{ glAttributeSlotElements :: !(V.Vector GlAttribute)
	, glAttributeSlotDivisor :: !GLuint
	}

data GlAttribute = GlAttribute
	{
	-- | Generic attribute index.
	  glAttributeIndex :: !GLuint
	-- | Attribute size as specified to glVertexAttribPointer.
	, glAttributeSize :: !GLint
	-- | Attribute type as specified to glVertexAttribPointer.
	, glAttributeType :: !GLenum
	-- | Is attribute normalized.
	, glAttributeIsNormalized :: !GLboolean
	-- | Is attribute integer.
	, glAttributeIsInteger :: !Bool
	-- | Offset within vertex buffer.
	, glAttributeOffset :: !IntPtr
	}

data GlUniform = GlUniform
	{ glUniformLocation :: !GLint
	, glUniformOffset :: !Int
	, glUniformSize :: !GLint
	, glUniformType :: !ValueType
	}

instance Device GlContext where
	type DeferredContext GlContext = GlContext
	newtype TextureId GlContext = GlTextureId GLuint deriving Eq
	newtype SamplerStateId GlContext = GlSamplerStateId GLuint deriving Eq
	data BlendStateId GlContext
		= GlBlendStateId BlendStateInfo
		| GlNullBlendStateId
		deriving Eq
	newtype RenderTargetId GlContext = GlRenderTargetId GLuint
	newtype DepthStencilTargetId GlContext = GlDepthStencilTargetId GLuint
	newtype FrameBufferId GlContext = GlFrameBufferId GLuint deriving Eq
	data VertexBufferId GlContext = GlVertexBufferId !GLuint !GLsizei deriving Eq
	data IndexBufferId GlContext = GlIndexBufferId !GLuint !GLenum
	data ProgramId GlContext = GlProgramId
		{ glProgramName :: !GLuint
		, glProgramVertexArrayName :: !GLuint
		, glProgramAttributeSlots ::  !(V.Vector GlAttributeSlot)
		-- | Number of generic vertex attributes used (i.e. used attributes are from 0 to this number minus 1).
		-- Only for manual binding, i.e. zero when vertex array is used.
		, glProgramAttributesCount :: !Int
		-- | "Manual" uniforms by slot.
		, glProgramUniforms :: !(V.Vector (V.Vector GlUniform))
		}
	data UniformBufferId GlContext
		-- | Real uniform buffer: buffer name, size.
		= GlUniformBufferId !GLuint !Int
		-- | Emulated uniform buffer: bytestring ref.
		| GlUniformMemoryBufferId !(IORef B.ByteString)
		-- | Null uniform buffer.
		| GlNullUniformBufferId

	nullTexture = GlTextureId 0
	nullSamplerState = GlSamplerStateId 0
	nullBlendState = GlNullBlendStateId
	nullDepthStencilTarget = GlDepthStencilTargetId 0
	nullIndexBuffer = GlIndexBufferId 0 gl_UNSIGNED_SHORT
	nullUniformBuffer = GlNullUniformBufferId

	createDeferredContext = undefined

	createStaticTexture context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} textureInfo@TextureInfo
		{ textureFormat = format
		} bytes = glInvoke context $ describeException ("failed to create OpenGL static texture", textureInfo) $ do

		let
			width = fromIntegral $ textureWidth textureInfo
			height = fromIntegral $ textureHeight textureInfo
			depth = fromIntegral $ textureDepth textureInfo
			mips = fromIntegral $ textureMips textureInfo
			count = fromIntegral $ textureCount textureInfo

		-- arrays of 3D textures not supported
		if depth > 0 && count > 0 then throwIO $ DescribeFirstException "array of 3D textures is not supported"
		else return ()

		let TextureMetrics
			{ textureMipsMetrics = mipsMetrics
			} = calcTextureMetrics textureInfo

		-- allocate texture name
		textureName <- alloca $ \namePtr -> do
			glGenTextures 1 namePtr
			glCheckErrors 0 "gen texture"
			peek namePtr

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		glPixelStorei gl_UNPACK_ALIGNMENT 1
		glCheckErrors 0 "set unpack alignment"

		let pixelSize = fromIntegral $ pixelSizeByteSize $ textureFormatPixelSize format

		-- get texture type
		let textureType = if depth > 0 then Texture3D
			else if height > 0 then
				if count > 0 then Texture2DArray
				else Texture2D
			else if count > 0 then Texture1DArray
			else Texture1D

		-- get target
		let glTarget = case textureType of
			Texture3D      -> gl_TEXTURE_3D
			Texture2DArray -> gl_TEXTURE_2D_ARRAY
			Texture2D      -> gl_TEXTURE_2D
			Texture1DArray -> gl_TEXTURE_1D_ARRAY
			Texture1D      -> gl_TEXTURE_1D

		-- bind texture
		glBindTexture glTarget textureName
		glCheckErrors 0 "bind texture"

		-- allocate texture storage, if we use it
		if useTextureStorage then do
			case textureType of
				Texture3D      -> glTexStorage3D glTarget mips glInternalFormat width height depth
				Texture2DArray -> glTexStorage3D glTarget mips glInternalFormat width height count
				Texture2D      -> glTexStorage2D glTarget mips glInternalFormat width height
				Texture1DArray -> glTexStorage2D glTarget mips glInternalFormat width count
				Texture1D      -> glTexStorage1D glTarget mips glInternalFormat width
			glCheckErrors 0 "tex storage"
		else return ()

		-- gl[Compressed]TexImage* requires GLint, but glInternal format is GLenum (GLuint)
		let glInternalFormatS = fromIntegral glInternalFormat

		-- loop for mips
		B.unsafeUseAsCString bytes $ \textureData -> forM_ (zip [0..(mips - 1)] mipsMetrics) $ \(mip, mipMetrics) -> do
			let
				mipWidth = fromIntegral $ textureMipWidth mipMetrics
				mipHeight = fromIntegral $ textureMipHeight mipMetrics
				mipDepth = fromIntegral $ textureMipDepth mipMetrics
				mipLinePitch = fromIntegral $ textureMipLinePitch mipMetrics
				mipSlicePitch = fromIntegral $ textureMipSlicePitch mipMetrics
				mipOffset = fromIntegral $ textureMipOffset mipMetrics
				mipSize = fromIntegral $ textureMipSize mipMetrics

			-- set unpack image height if needed
			if textureType == Texture3D || textureType == Texture2DArray then do
				glPixelStorei gl_UNPACK_IMAGE_HEIGHT $ mipSlicePitch `div` pixelSize
				glCheckErrors 0 "set unpack image height"
			else return ()

			-- set unpack row length if needed
			if textureType == Texture3D || textureType == Texture2DArray || textureType == Texture2D || textureType == Texture1DArray then do
				glPixelStorei gl_UNPACK_ROW_LENGTH $ mipLinePitch `div` pixelSize
				glCheckErrors 0 "set unpack row length"
			else return ()

			-- get mip data
			let mipData = textureData `plusPtr` mipOffset

			-- upload data
			if useTextureStorage then
				if compressed then
					case textureType of
						Texture3D      -> glCompressedTexSubImage3D glTarget mip 0 0 0 mipWidth mipHeight mipDepth glFormat mipSize mipData
						Texture2DArray -> glCompressedTexSubImage3D glTarget mip 0 0 0 mipWidth mipHeight count    glFormat mipSize mipData
						Texture2D      -> glCompressedTexSubImage2D glTarget mip 0 0   mipWidth mipHeight          glFormat mipSize mipData
						Texture1DArray -> glCompressedTexSubImage2D glTarget mip 0 0   mipWidth count              glFormat mipSize mipData
						Texture1D      -> glCompressedTexSubImage1D glTarget mip 0     mipWidth                    glFormat mipSize mipData
				else
					case textureType of
						Texture3D      -> glTexSubImage3D glTarget mip 0 0 0 mipWidth mipHeight mipDepth glFormat glType mipData
						Texture2DArray -> glTexSubImage3D glTarget mip 0 0 0 mipWidth mipHeight count    glFormat glType mipData
						Texture2D      -> glTexSubImage2D glTarget mip 0 0   mipWidth mipHeight          glFormat glType mipData
						Texture1DArray -> glTexSubImage2D glTarget mip 0 0   mipWidth count              glFormat glType mipData
						Texture1D      -> glTexSubImage1D glTarget mip 0     mipWidth                    glFormat glType mipData
			else
				if compressed then
					case textureType of
						Texture3D      -> glCompressedTexImage3D glTarget mip glInternalFormat mipWidth mipHeight mipDepth 0 mipSize mipData
						Texture2DArray -> glCompressedTexImage3D glTarget mip glInternalFormat mipWidth mipHeight count    0 mipSize mipData
						Texture2D      -> glCompressedTexImage2D glTarget mip glInternalFormat mipWidth mipHeight          0 mipSize mipData
						Texture1DArray -> glCompressedTexImage2D glTarget mip glInternalFormat mipWidth count              0 mipSize mipData
						Texture1D      -> glCompressedTexImage1D glTarget mip glInternalFormat mipWidth                    0 mipSize mipData
				else
					case textureType of
						Texture3D      -> glTexImage3D glTarget mip glInternalFormatS mipWidth mipHeight mipDepth 0 glFormat glType mipData
						Texture2DArray -> glTexImage3D glTarget mip glInternalFormatS mipWidth mipHeight count    0 glFormat glType mipData
						Texture2D      -> glTexImage2D glTarget mip glInternalFormatS mipWidth mipHeight          0 glFormat glType mipData
						Texture1DArray -> glTexImage2D glTarget mip glInternalFormatS mipWidth count              0 glFormat glType mipData
						Texture1D      -> glTexImage1D glTarget mip glInternalFormatS mipWidth                    0 glFormat glType mipData
			glCheckErrors 0 "tex image"

		if useTextureStorage then return ()
		else do
			glTexParameteri glTarget gl_TEXTURE_BASE_LEVEL 0
			glTexParameteri glTarget gl_TEXTURE_MAX_LEVEL $ mips - 1
			glCheckErrors 0 "texture parameters"

		return (GlTextureId textureName, glInvoke context $ with textureName $ glDeleteTextures 1)

	createSamplerState _context _samplerStateInfo = describeException "failed to create OpenGL sampler state" $ do
		return (GlSamplerStateId 0, return ())

	createBlendState _context blendStateInfo = return (GlBlendStateId blendStateInfo, return ())

	createReadableRenderTarget context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} width height format = glInvoke context $ describeException "failed to create OpenGL readable render target" $ do
		-- allocate texture name
		textureName <- alloca $ \namePtr -> do
			glGenTextures 1 namePtr
			peek namePtr

		glBindTexture gl_TEXTURE_2D textureName
		glCheckErrors 0 "bind texture"

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		if compressed then throwIO $ DescribeFirstException "render target cannot use compressed format"
		else return ()

		if useTextureStorage then
			glTexStorage2D gl_TEXTURE_2D 1 glInternalFormat (fromIntegral width) (fromIntegral height)
		else
			glTexImage2D gl_TEXTURE_2D 0 (fromIntegral glInternalFormat) (fromIntegral width) (fromIntegral height) 0 glFormat glType nullPtr
		glCheckErrors 0 "texture storage"

		return ((GlRenderTargetId textureName, GlTextureId textureName), glInvoke context $ with textureName $ glDeleteTextures 1)

	createDepthStencilTarget context width height = glInvoke context $ describeException "failed to create OpenGL depth stencil target" $ do
		-- allocate texture name
		textureName <- alloca $ \namePtr -> do
			glGenTextures 1 namePtr
			peek namePtr

		glBindTexture gl_TEXTURE_2D textureName
		glCheckErrors 0 "bind texture"
		glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_DEPTH_STENCIL) (fromIntegral width) (fromIntegral height) 0 gl_DEPTH_STENCIL gl_UNSIGNED_INT_24_8 nullPtr
		glCheckErrors 0 "tex image"

		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_R $ fromIntegral gl_CLAMP_TO_EDGE
		glCheckErrors 0 "texture parameters"

		return (GlDepthStencilTargetId textureName, glInvoke context $ with textureName $ glDeleteTextures 1)

	createReadableDepthStencilTarget context width height = do
		(depthStencilTarget@(GlDepthStencilTargetId bufferName), destroy) <- createDepthStencilTarget context width height
		return ((depthStencilTarget, GlTextureId bufferName), destroy)

	createFrameBuffer context@GlContext
		{ glContextActualState = GlContextState
			{ glContextStateFrameBuffer = actualFrameBufferRef
			}
		} renderTargets (GlDepthStencilTargetId depthStencilName) = glInvoke context $ describeException "failed to create OpenGL framebuffer" $ do
		-- allocate framebuffer name
		framebufferName <- alloca $ \namePtr -> do
			glGenFramebuffers 1 namePtr
			glCheckErrors 0 "gen framebuffer"
			peek namePtr

		-- bind framebuffer
		glBindFramebuffer gl_FRAMEBUFFER framebufferName
		glCheckErrors 0 "bind framebuffer"

		-- bind render targets
		forM_ (zip [0..] renderTargets) $ \(i, GlRenderTargetId renderTargetName) -> do
			glFramebufferTexture2D gl_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + i) gl_TEXTURE_2D renderTargetName 0
			glCheckErrors 0 "bind framebuffer color buffer"

		-- bind depth-stencil target
		glFramebufferTexture2D gl_FRAMEBUFFER gl_DEPTH_STENCIL_ATTACHMENT gl_TEXTURE_2D depthStencilName 0
		glCheckErrors 0 "bind framebuffer depth-stencil buffer"

		let frameBufferId = GlFrameBufferId framebufferName

		writeIORef actualFrameBufferRef frameBufferId

		return (frameBufferId, glInvoke context $ with framebufferName $ glDeleteFramebuffers 1)

	createStaticVertexBuffer context bytes stride = glInvoke context $ describeException "failed to create OpenGL static vertex buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			glCheckErrors 0 "gen buffer"
			peek namePtr

		glBindBuffer gl_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			glBufferData gl_ARRAY_BUFFER (fromIntegral bytesLen) bytesPtr gl_STATIC_DRAW
		glCheckErrors 0 "buffer data"

		return (GlVertexBufferId bufferName (fromIntegral stride), glInvoke context $ with bufferName $ glDeleteBuffers 1)

	createDynamicVertexBuffer context size stride = glInvoke context $ describeException "failed to create OpenGL dynamic vertex buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			glCheckErrors 0 "gen buffer"
			peek namePtr

		glBindBuffer gl_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		glBufferData gl_ARRAY_BUFFER (fromIntegral size) nullPtr gl_DYNAMIC_DRAW
		glCheckErrors 0 "buffer data"

		return (GlVertexBufferId bufferName (fromIntegral stride), glInvoke context $ with bufferName $ glDeleteBuffers 1)

	createStaticIndexBuffer context@GlContext
		{ glContextActualState = GlContextState
			{ glContextStateIndexBuffer = actualIndexBufferRef
			}
		} bytes is32Bit = glInvoke context $ describeException "failed to create OpenGL static index buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			glCheckErrors 0 "gen buffer"
			peek namePtr

		glBindBuffer gl_ELEMENT_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			glBufferData gl_ELEMENT_ARRAY_BUFFER (fromIntegral bytesLen) bytesPtr gl_STATIC_DRAW
		glCheckErrors 0 "buffer data"

		let indexBufferId = GlIndexBufferId bufferName (if is32Bit then gl_UNSIGNED_INT else gl_UNSIGNED_SHORT)

		-- element array buffer is a part of VAO; unbind it in order to re-bind with actual VAO during drawing
		glBindBuffer gl_ELEMENT_ARRAY_BUFFER 0
		glCheckErrors 0 "unbind buffer"

		writeIORef actualIndexBufferRef $ GlIndexBufferId 0 gl_UNSIGNED_SHORT

		return (indexBufferId, glInvoke context $ with bufferName $ glDeleteBuffers 1)

	createProgram context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = useUniformBufferObject
			, glCapsArbVertexAttribBinding = capArbVertexAttribBinding
			}
		, glContextActualState = GlContextState
			{ glContextStateProgram = actualProgramRef
			}
		} program = glInvoke context $ describeException "failed to create OpenGL program" $ do

		-- reset current program in order to correctly rebind it later for drawing
		writeIORef actualProgramRef glNullProgram

		bk <- newBook

		-- generate GLSL
		let glslConfig = GlslConfig
			{ glslConfigForceFloatAttributes = False
			, glslConfigUnsignedUnsupported = False
			, glslConfigUniformBlocks = useUniformBufferObject
			, glslConfigInOutSyntax = True
			}
		GlslProgram
			{ glslProgramAttributes = attributes
			, glslProgramUniformBlocks = uniformBlocks
			, glslProgramUniforms = uniforms
			, glslProgramSamplers = samplers
			, glslProgramFragmentTargets = fragmentTargets
			, glslProgramShaders = shaders
			} <- liftM (glslGenerateProgram glslConfig) $ runProgram program

		-- create program
		programName <- book bk $ do
			p <- glCreateProgram
			return (p, glDeleteProgram p)

		-- create and attach shaders
		forM_ shaders $ \(shaderStage, shaderSource) -> describeException ("failed to create OpenGL shader", shaderStage) $ do

			-- create shader
			shaderName <- glCreateShader $ case shaderStage of
				GlslVertexStage -> gl_VERTEX_SHADER
				GlslFragmentStage -> gl_FRAGMENT_SHADER
			glCheckErrors 0 "create shader"
			_ <- book bk $ return (shaderName, glDeleteShader shaderName)

			-- set shader source
			B.unsafeUseAsCStringLen (T.encodeUtf8 shaderSource) $ \(sourcePtr, sourceLen) -> do
				with sourcePtr $ \sourcePtrPtr -> do
					with (fromIntegral sourceLen) $ \sourceLenPtr -> do
						glShaderSource shaderName 1 sourcePtrPtr sourceLenPtr
			glCheckErrors 0 "set shader source"

			-- compile shader
			glCompileShader shaderName
			glCheckErrors 0 "compile shader"
			-- check compilation status
			status <- alloca $ \statusPtr -> do
				glGetShaderiv shaderName gl_COMPILE_STATUS statusPtr
				glCheckErrors 0 "get shader compile status"
				peek statusPtr
			if status == 1 then do
				-- compilation succeeded, attach shader to program
				glAttachShader programName shaderName
				glCheckErrors 0 "attach shader"
			else do
				-- in case of error, get compilation log
				logLength <- alloca $ \logLengthPtr -> do
					poke logLengthPtr 0
					glGetShaderiv shaderName gl_INFO_LOG_LENGTH logLengthPtr
					glCheckErrors 0 "get shader log length"
					peek logLengthPtr
				logBytes <- allocaBytes (fromIntegral logLength) $ \logPtr -> do
					realLogLength <- alloca $ \logLengthPtr -> do
						glGetShaderInfoLog shaderName logLength logLengthPtr logPtr
						glCheckErrors 0 "get shader log"
						peek logLengthPtr
					B.packCStringLen (logPtr, fromIntegral realLogLength)
				glClearErrors
				putStrLn $ T.unpack shaderSource -- TEST
				throwIO $ DescribeFirstException ("failed to compile shader", T.decodeUtf8 logBytes)

		-- bind attributes
		forM_ (zip attributes [0..]) $ \(GlslAttribute
			{ glslAttributeName = attributeName
			}, i) -> do
			B.useAsCString (T.encodeUtf8 attributeName) $ glBindAttribLocation programName i
			glCheckErrors 0 "bind attribute location"

		-- bind fragment targets
		forM_ fragmentTargets $ \GlslFragmentTarget
			{ glslFragmentTargetName = targetName
			, glslFragmentTargetIndex = targetIndex
			} -> do
			B.useAsCString (T.encodeUtf8 targetName) $ glBindFragDataLocation programName (fromIntegral targetIndex)
			glCheckErrors 0 "bind frag data location"

		-- link program
		glLinkProgram programName
		glCheckErrors 0 "link program"
		-- check link status
		status <- alloca $ \statusPtr -> do
			glGetProgramiv programName gl_LINK_STATUS statusPtr
			glCheckErrors 0 "get program link status"
			peek statusPtr
		if status == 1 then return ()
		else do
			-- in case of error, get linking log
			logLength <- alloca $ \logLengthPtr -> do
				glGetProgramiv programName gl_INFO_LOG_LENGTH logLengthPtr
				glCheckErrors 0 "get program log length"
				peek logLengthPtr
			logBytes <- allocaBytes (fromIntegral logLength) $ \logPtr -> do
				realLogLength <- alloca $ \logLengthPtr -> do
					glGetProgramInfoLog programName logLength logLengthPtr logPtr
					glCheckErrors 0 "get program log"
					peek logLengthPtr
				B.packCStringLen (logPtr, fromIntegral realLogLength)
			glClearErrors
			throwIO $ DescribeFirstException ("failed to link program", T.decodeUtf8 logBytes)

		-- set as current
		glUseProgram programName
		glCheckErrors 0 "set program"

		-- bind uniform blocks
		forM_ uniformBlocks $ \GlslUniformBlock
			{ glslUniformBlockName = uniformBlockName
			, glslUniformBlockSlot = slot
			} -> do
			index <- B.useAsCString (T.encodeUtf8 uniformBlockName) $ glGetUniformBlockIndex programName
			glCheckErrors 0 "get uniform block index"
			glUniformBlockBinding programName index (fromIntegral slot)
			glCheckErrors 0 "uniform block binding"

		-- bind samplers
		forM_ samplers $ \GlslSampler
			{ glslSamplerName = samplerName
			, glslSamplerInfo = Sampler
				{ samplerSlot = slot
				}
			} -> do
			location <- B.useAsCString (T.encodeUtf8 samplerName) $ glGetUniformLocation programName
			glCheckErrors 0 "get sampler location"
			glUniform1i location (fromIntegral slot)
			glCheckErrors 0 "bind sampler to texture unit"

		-- get non-buffer uniform bindings
		uniformBindings <- do
			-- sort uniforms by slot
			let getSlot GlslUniform
				{ glslUniformInfo = Uniform
					{ uniformSlot = slot
					}
				} = slot
			let eqBySlot a b = getSlot a == getSlot b
			let compareBySlot a b = compare (getSlot a) (getSlot b)
			let uniformsBySlot = groupBy eqBySlot $ sortBy compareBySlot uniforms
			-- get maximum slot
			let slotsCount = if null uniformsBySlot then 0 else 1 + (maximum $ map (getSlot . head) uniformsBySlot)
			-- create slots
			slots <- VM.replicate slotsCount V.empty
			forM_ uniformsBySlot $ \us@(u : _) -> do
				let slot = getSlot u
				slotUniforms <- V.forM (V.fromList us) $ \GlslUniform
					{ glslUniformName = uniformName
					, glslUniformInfo = Uniform
						{ uniformOffset = offset
						, uniformSize = size
						, uniformType = t
						}
					} -> do
					-- get location
					location <- B.useAsCString (T.encodeUtf8 uniformName) $ glGetUniformLocation programName
					glCheckErrors 0 "get uniform location"

					return GlUniform
						{ glUniformLocation = location
						, glUniformOffset = offset
						, glUniformSize = fromIntegral size
						, glUniformType = t
						}
				VM.write slots slot slotUniforms
			V.unsafeFreeze slots

		-- create vertex array if supported
		(vertexArrayName, attributeSlots, attributesCount) <- if capArbVertexAttribBinding then do
			-- create vertex array
			vaName <- book bk $ do
				vaName <- alloca $ \vaNamePtr -> do
					glGenVertexArrays 1 vaNamePtr
					glCheckErrors 0 "gen vertex array"
					peek vaNamePtr
				return (vaName, with vaName $ glDeleteVertexArrays 1)

			-- bind vertex array
			glBindVertexArray vaName
			glCheckErrors 0 "bind vertex array"

			-- setup attributes for vertex array
			forM_ (zip attributes [0..]) $ \(GlslAttribute
				{ glslAttributeInfo = Attribute
					{ attributeSlot = slot
					, attributeOffset = offset
					, attributeDivisor = divisor
					, attributeType = aType
					}
				}, i) -> do
				-- enable attribute
				glEnableVertexAttribArray i
				glCheckErrors 0 "enable vertex attrib array"

				-- get attribute's info
				let (size, t, isNormalized, isInteger) = glGetAttributeSTNI aType

				-- set format
				if isInteger then glVertexAttribIFormat i size t (fromIntegral offset)
				else glVertexAttribFormat i size t isNormalized (fromIntegral offset)
				glCheckErrors 0 "vertex attrib format"

				-- set slot
				glVertexAttribBinding i (fromIntegral slot)
				glCheckErrors 0 "vertex attrib binding"
				-- set divisor for slot
				-- TODO: it's not optimal, as we will set the same divisor multiple times for the same slot
				glVertexBindingDivisor (fromIntegral slot) (fromIntegral divisor)
				glCheckErrors 0 "vertex binding divisor"

			-- unbind vertex array
			glBindVertexArray 0
			glCheckErrors 0 "unbind vertex array"

			return (vaName, V.empty, 0)
		-- else create "manual" attribute binding
		else do
			-- sort attributes by slot
			let getSlot GlslAttribute
				{ glslAttributeInfo = Attribute
					{ attributeSlot = slot
					}
				} = slot
			let eqBySlot a b = getSlot (fst a) == getSlot (fst b)
			let compareBySlot a b = compare (getSlot $ fst a) (getSlot $ fst b)
			let attributesBySlot = groupBy eqBySlot $ sortBy compareBySlot $ zip attributes [0..]
			-- get maximum slot
			let slotsCount = if null attributesBySlot then 0 else 1 + (maximum $ map (getSlot . fst . head) attributesBySlot)
			-- create attribute slots
			let attributeSlots = V.create $ do
				slots <- VM.replicate slotsCount GlAttributeSlot
					{ glAttributeSlotElements = V.empty
					, glAttributeSlotDivisor = 0
					}
				forM_ attributesBySlot $ \as@(a : _) -> do
					let slot = getSlot $ fst a
					elements <- V.forM (V.fromList as) $ \(GlslAttribute
						{ glslAttributeInfo = Attribute
							{ attributeOffset = offset
							, attributeType = aType
							}
						}, i) -> do
						let (size, t, isNormalized, isInteger) = glGetAttributeSTNI aType
						return GlAttribute
							{ glAttributeIndex = i
							, glAttributeSize = size
							, glAttributeType = t
							, glAttributeIsNormalized = isNormalized
							, glAttributeIsInteger = isInteger
							, glAttributeOffset = fromIntegral offset
							}
					VM.write slots slot GlAttributeSlot
						{ glAttributeSlotElements = elements
						, glAttributeSlotDivisor = fromIntegral $ attributeDivisor $ glslAttributeInfo $ fst a
						}
				return slots

			return (0, attributeSlots, length attributes)

		return (GlProgramId
			{ glProgramName = programName
			, glProgramVertexArrayName = vertexArrayName
			, glProgramAttributeSlots = attributeSlots
			, glProgramAttributesCount = attributesCount
			, glProgramUniforms = uniformBindings
			}, glInvoke context $ freeBook bk)

	createUniformBuffer context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = useUniformBufferObject
			}
		} size = glInvoke context $ describeException "failed to create OpenGL uniform buffer" $ do

		if useUniformBufferObject then do
			-- allocate buffer name
			bufferName <- alloca $ \namePtr -> do
				glGenBuffers 1 namePtr
				glCheckErrors 0 "gen buffer"
				peek namePtr

			glBindBuffer gl_UNIFORM_BUFFER bufferName
			glCheckErrors 0 "bind buffer"
			glBufferData gl_UNIFORM_BUFFER (fromIntegral size) nullPtr gl_DYNAMIC_DRAW
			glCheckErrors 0 "buffer data"
			
			return (GlUniformBufferId bufferName size, glInvoke context $ with bufferName $ glDeleteBuffers 1)
		else do
			bufferRef <- newIORef B.empty
			return (GlUniformMemoryBufferId bufferRef, return ())

instance Context GlContext GlContext where
	contextClearColor context targetIndex color = do
		glUpdateFrameBuffer context
		with color $ glClearBufferfv gl_COLOR (fromIntegral targetIndex) . castPtr
		glCheckErrors 1 "clear color"

	contextClearDepth context depth = do
		glUpdateFrameBuffer context
		glEnableDepthWriteForClearing context
		with (coerce depth) $ glClearBufferfv gl_DEPTH 0
		glCheckErrors 1 "clear depth"

	contextClearStencil context stencil = do
		glUpdateFrameBuffer context
		with (fromIntegral stencil) $ glClearBufferiv gl_STENCIL 0
		glCheckErrors 1 "clear stencil"

	contextClearDepthStencil context depth stencil = do
		glUpdateFrameBuffer context
		glEnableDepthWriteForClearing context
		glClearBufferfi gl_DEPTH_STENCIL 0 (coerce depth) (fromIntegral stencil)
		glCheckErrors 1 "clear depth stencil"

	contextUploadUniformBuffer _context uniformBuffer bytes = case uniformBuffer of
		GlUniformBufferId bufferName _bufferSize -> do
			glBindBuffer gl_UNIFORM_BUFFER bufferName
			B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
				glBufferData gl_UNIFORM_BUFFER (fromIntegral bytesLen) bytesPtr gl_DYNAMIC_DRAW
			glCheckErrors 1 "upload uniform buffer"
		GlUniformMemoryBufferId bufferRef -> do
			-- remember buffer data
			writeIORef bufferRef bytes
		GlNullUniformBufferId -> throwIO $ DescribeFirstException "uploading to null uniform buffer"

	contextUploadVertexBuffer _context (GlVertexBufferId bufferName _stride) bytes = do
		glBindBuffer gl_ARRAY_BUFFER bufferName
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			glBufferData gl_ARRAY_BUFFER (fromIntegral bytesLen) bytesPtr gl_DYNAMIC_DRAW
		glCheckErrors 1 "upload vertex buffer"

	contextDraw context@GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateIndexBuffer = indexBufferRef
			}
		} instancesCount indicesCount = do
		glUpdateContext context
		GlIndexBufferId indexBufferName indicesType <- readIORef indexBufferRef
		if indexBufferName > 0 then do
			if instancesCount > 1 then
				glDrawElementsInstanced gl_TRIANGLES (fromIntegral indicesCount) indicesType nullPtr (fromIntegral instancesCount)
			else
				glDrawElements gl_TRIANGLES (fromIntegral indicesCount) indicesType nullPtr
		else do
			if instancesCount > 1 then
				glDrawArraysInstanced gl_TRIANGLES 0 (fromIntegral indicesCount) (fromIntegral instancesCount)
			else
				glDrawArrays gl_TRIANGLES 0 (fromIntegral indicesCount)
		glCheckErrors 1 "draw"

	-- TODO
	contextPlay = undefined

	contextRender GlContext
		{ glContextDesiredState = desiredContextState
		} f = do
		glSetDefaultContextState desiredContextState
		f

	contextSetFrameBuffer GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			}
		} frameBuffer scope = do
		oldFrameBuffer <- readIORef frameBufferRef
		writeIORef frameBufferRef frameBuffer
		r <- scope
		writeIORef frameBufferRef oldFrameBuffer
		return r

	contextSetViewport GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateViewport = viewportRef
			}
		} width height scope = do
		oldViewport <- readIORef viewportRef
		writeIORef viewportRef (width, height)
		r <- scope
		writeIORef viewportRef oldViewport
		return r

	contextGetViewport GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateViewport = viewportRef
			}
		} = readIORef viewportRef

	contextSetVertexBuffer GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateVertexBuffers = vertexBuffersVector
			}
		} i vertexBuffer scope = do
		oldVertexBuffer <- VM.read vertexBuffersVector i
		VM.write vertexBuffersVector i vertexBuffer
		r <- scope
		VM.write vertexBuffersVector i oldVertexBuffer
		return r

	contextSetIndexBuffer GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateIndexBuffer = indexBufferRef
			}
		} indexBuffer scope = do
		oldIndexBuffer <- readIORef indexBufferRef
		writeIORef indexBufferRef indexBuffer
		r <- scope
		writeIORef indexBufferRef oldIndexBuffer
		return r

	contextSetUniformBuffer GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateUniformBuffers = uniformBuffersVector
			}
		} i uniformBuffer scope = do
		oldUniformBuffer <- VM.read uniformBuffersVector i
		VM.write uniformBuffersVector i uniformBuffer
		r <- scope
		VM.write uniformBuffersVector i oldUniformBuffer
		return r

	contextSetSampler GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateSamplers = samplersVector
			}
		} i texture samplerState scope = do
		oldSampler <- VM.read samplersVector i
		VM.write samplersVector i (texture, samplerState)
		r <- scope
		VM.write samplersVector i oldSampler
		return r

	contextSetBlendState GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateBlendState = blendStateRef
			}
		} blendState scope = do
		oldBlendState <- readIORef blendStateRef
		writeIORef blendStateRef blendState
		r <- scope
		writeIORef blendStateRef oldBlendState
		return r

	contextSetDepthTestFunc GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateDepthTestFunc = depthTestFuncRef
			}
		} depthTestFunc scope = do
		oldDepthTestFunc <- readIORef depthTestFuncRef
		writeIORef depthTestFuncRef depthTestFunc
		r <- scope
		writeIORef depthTestFuncRef oldDepthTestFunc
		return r

	contextSetDepthWrite GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateDepthWrite = depthWriteRef
			}
		} depthWrite scope = do
		oldDepthWrite <- readIORef depthWriteRef
		writeIORef depthWriteRef depthWrite
		r <- scope
		writeIORef depthWriteRef oldDepthWrite
		return r

	contextSetProgram GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateProgram = programRef
			}
		} program scope = do
		oldProgram <- readIORef programRef
		writeIORef programRef program
		r <- scope
		writeIORef programRef oldProgram
		return r

type GlPresenter = GlContext

instance Presenter GlContext GlSystem GlContext GlContext where
	-- TODO
	setPresenterMode _presenter _maybeMode = return ()

	presenterRender context@GlContext
		{ glContextWindow = SdlWindow
			{ swHandle = windowHandle
			}
		, glContextDesiredState = GlContextState
			{ glContextStateFrameBuffer = frameBufferRef
			, glContextStateViewport = viewportRef
			}
		} _context f = glInvoke context $ do
		-- get viewport size
		(width, height) <- alloca $ \widthPtr -> alloca $ \heightPtr -> do
			SDL.glGetDrawableSize windowHandle widthPtr heightPtr
			width <- peek widthPtr
			height <- peek heightPtr
			return (width, height)

		-- setup state
		writeIORef frameBufferRef $ GlFrameBufferId 0
		writeIORef viewportRef (fromIntegral width, fromIntegral height)

		-- perform render
		r <- f

		-- present
		SDL.glSwapWindow windowHandle

		return r

createGlSystem :: IO (GlSystem, IO ())
createGlSystem = return (GlSystem, return ())

data GlCaps = GlCaps
	{ glCapsArbUniformBufferObject :: !Bool
	, glCapsArbSamplerObjects :: !Bool
	, glCapsArbVertexAttribBinding :: !Bool
	, glCapsArbFramebufferObject :: !Bool
	, glCapsArbTextureStorage :: !Bool
	, glCapsArbInstancedArrays :: !Bool
	} deriving Show

createGlContext :: DeviceId GlSystem -> SdlWindow -> Bool -> IO (GlContext, IO ())
createGlContext _deviceId window@SdlWindow
	{ swSystem = ws
	, swHandle = windowHandle
	} debug = describeException "failed to create OpenGL device" $ invokeSdlWindowSystem ws $ do
	-- create context
	glContext <- checkSdlResult $ SDL.glCreateContext windowHandle
	-- make it current
	checkSdlError (== 0) $ SDL.glMakeCurrent windowHandle glContext

	-- init capabilities
	capArbUniformBufferObject <- isGlExtensionSupported "GL_ARB_uniform_buffer_object"
	capArbSamplerObjects <- isGlExtensionSupported "GL_ARB_sampler_objects"
	capArbVertexAttribBinding <- isGlExtensionSupported "GL_ARB_vertex_attrib_binding"
	capArbFramebufferObject <- isGlExtensionSupported "GL_ARB_framebuffer_object"
	capArbTextureStorage <- isGlExtensionSupported "GL_ARB_texture_storage"
	capArbInstancedArrays <- isGlExtensionSupported "GL_ARB_instanced_arrays"

	-- create context
	actualContextState <- glCreateContextState
	desiredContextState <- glCreateContextState
	boundAttributesCount <- newIORef 0
	let context = GlContext
		{ glContextContext = glContext
		, glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = capArbUniformBufferObject
			, glCapsArbSamplerObjects = capArbSamplerObjects
			, glCapsArbVertexAttribBinding = capArbVertexAttribBinding
			, glCapsArbFramebufferObject = capArbFramebufferObject
			, glCapsArbTextureStorage = capArbTextureStorage
			, glCapsArbInstancedArrays = capArbInstancedArrays
			}
		, glContextWindow = window
		, glContextActualState = actualContextState
		, glContextDesiredState = desiredContextState
		, glContextBoundAttributesCount = boundAttributesCount
		}

	-- set swap interval
	do
		-- try "late swap tearing"
		r <- SDL.glSetSwapInterval (-1)
		if r /= 0 then do
			-- didn't work, try usual vsync
			checkSdlError (== 0) $ SDL.glSetSwapInterval 1
		else return ()

	-- set front face mode
	glFrontFace gl_CW
	-- set cull mode
	glEnable gl_CULL_FACE
	glCullFace gl_BACK
	glCheckErrors 1 "init state"

	-- if debug mode requested, setup debug output
	if debug then do
		-- enable debug output
		glEnable gl_DEBUG_OUTPUT
		-- set debug message callback
		callbackPtr <- wrapGlDebugMessageCallback $ \messageSource messageType messageId messageSeverity messageLength messagePtr _userParam -> do
			let messageSourceStr = case messageSource of
				gl_DEBUG_SOURCE_API -> "DEBUG_SOURCE_API"
				gl_DEBUG_SOURCE_WINDOW_SYSTEM -> "DEBUG_SOURCE_WINDOW_SYSTEM"
				gl_DEBUG_SOURCE_SHADER_COMPILER -> "DEBUG_SOURCE_SHADER_COMPILER"
				gl_DEBUG_SOURCE_THIRD_PARTY -> "DEBUG_SOURCE_THIRD_PARTY"
				gl_DEBUG_SOURCE_APPLICATION -> "DEBUG_SOURCE_APPLICATION"
				gl_DEBUG_SOURCE_OTHER -> "DEBUG_SOURCE_OTHER"
				_ -> show messageSource
			let messageTypeStr = case messageType of
				gl_DEBUG_TYPE_ERROR -> "DEBUG_TYPE_ERROR"
				gl_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> "DEBUG_TYPE_DEPRECATED_BEHAVIOR"
				gl_DEBUG_TYPE_UNDEFINED_BEHAVIOR -> "DEBUG_TYPE_UNDEFINED_BEHAVIOR"
				gl_DEBUG_TYPE_PORTABILITY -> "DEBUG_TYPE_PORTABILITY"
				gl_DEBUG_TYPE_PERFORMANCE -> "DEBUG_TYPE_PERFORMANCE"
				gl_DEBUG_TYPE_MARKER -> "DEBUG_TYPE_MARKER"
				gl_DEBUG_TYPE_PUSH_GROUP -> "DEBUG_TYPE_PUSH_GROUP"
				gl_DEBUG_TYPE_POP_GROUP -> "DEBUG_TYPE_POP_GROUP"
				gl_DEBUG_TYPE_OTHER -> "DEBUG_TYPE_OTHER"
				_ -> show messageType
			let messageSeverityStr = case messageSeverity of
				gl_DEBUG_SEVERITY_HIGH -> "DEBUG_SEVERITY_HIGH"
				gl_DEBUG_SEVERITY_MEDIUM -> "DEBUG_SEVERITY_MEDIUM"
				gl_DEBUG_SEVERITY_LOW -> "DEBUG_SEVERITY_LOW"
				gl_DEBUG_SEVERITY_NOTIFICATION -> "DEBUG_SEVERITY_NOTIFICATION"
				_ -> show messageSeverity
			message <- liftM T.decodeUtf8 $ B.unsafePackCStringLen (messagePtr, fromIntegral messageLength)
			putStrLn $ "*** OpenGL debug message ***" ++
				"\n  source:    " ++ messageSourceStr ++
				"\n  type:      " ++ messageTypeStr ++
				"\n  id:        " ++ show messageId ++
				"\n  severity:  " ++ messageSeverityStr ++
				"\n  message:   " ++ T.unpack message ++
				"\n*** EOM ***"
		glDebugMessageCallback callbackPtr nullPtr
		-- enable asynchronous calls to callback
		glDisable gl_DEBUG_OUTPUT_SYNCHRONOUS
		-- enable all debug messages
		glDebugMessageControl gl_DONT_CARE gl_DONT_CARE gl_DONT_CARE 0 nullPtr 1
		glCheckErrors 1 "setup debug output"
	else return ()

	return (context, glInvoke context $ SDL.glDeleteContext glContext)

type GlDebugMessageCallback = GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGlDebugMessageCallback :: GlDebugMessageCallback -> IO (FunPtr GlDebugMessageCallback)

glNullProgram :: ProgramId GlDevice
glNullProgram = GlProgramId
	{ glProgramName = 0
	, glProgramVertexArrayName = 0
	, glProgramAttributeSlots = V.empty
	, glProgramAttributesCount = 0
	, glProgramUniforms = V.empty
	}

glCreateContextState :: IO GlContextState
glCreateContextState = do
	frameBuffer <- newIORef $ GlFrameBufferId 0
	viewport <- newIORef (0, 0)
	vertexBuffers <- VM.replicate 8 $ GlVertexBufferId 0 0
	indexBuffer <- newIORef $ GlIndexBufferId 0 gl_UNSIGNED_SHORT
	uniformBuffers <- VM.replicate 8 GlNullUniformBufferId
	samplers <- VM.replicate 8 (GlTextureId 0, GlSamplerStateId 0)
	program <- newIORef glNullProgram
	depthTestFunc <- newIORef DepthTestFuncLess
	depthWrite <- newIORef True
	blendState <- newIORef nullBlendState
	return GlContextState
		{ glContextStateFrameBuffer = frameBuffer
		, glContextStateViewport = viewport
		, glContextStateVertexBuffers = vertexBuffers
		, glContextStateIndexBuffer = indexBuffer
		, glContextStateUniformBuffers = uniformBuffers
		, glContextStateSamplers = samplers
		, glContextStateProgram = program
		, glContextStateDepthTestFunc = depthTestFunc
		, glContextStateDepthWrite = depthWrite
		, glContextStateBlendState = blendState
		}

glSetDefaultContextState :: GlContextState -> IO ()
glSetDefaultContextState GlContextState
	{ glContextStateFrameBuffer = frameBufferRef
	, glContextStateViewport = viewportRef
	, glContextStateVertexBuffers = vertexBuffersVector
	, glContextStateIndexBuffer = indexBufferRef
	, glContextStateUniformBuffers = uniformBuffersVector
	, glContextStateSamplers = samplersVector
	, glContextStateProgram = programRef
	, glContextStateDepthTestFunc = depthTestFuncRef
	, glContextStateDepthWrite = depthWriteRef
	, glContextStateBlendState = blendStateRef
	} = do
	writeIORef frameBufferRef $ GlFrameBufferId 0
	writeIORef viewportRef (0, 0)
	VM.set vertexBuffersVector $ GlVertexBufferId 0 0
	writeIORef indexBufferRef $ GlIndexBufferId 0 gl_UNSIGNED_SHORT
	VM.set uniformBuffersVector GlNullUniformBufferId
	VM.set samplersVector (GlTextureId 0, GlSamplerStateId 0)
	writeIORef programRef glNullProgram
	writeIORef depthTestFuncRef DepthTestFuncLess
	writeIORef depthWriteRef True
	writeIORef blendStateRef nullBlendState

-- | Run in a thread of SDL window system.
glInvoke :: GlContext -> IO a -> IO a
glInvoke GlContext
	{ glContextWindow = SdlWindow
		{ swSystem = windowSystem
		}
	} io = invokeSdlWindowSystem windowSystem io

isGlExtensionSupported :: String -> IO Bool
isGlExtensionSupported name = withCString name SDL.glExtensionSupported

-- | Returns (compressed, internal format, format, type)
glFormatFromTextureFormat :: TextureFormat -> (Bool, GLenum, GLenum, GLenum)
glFormatFromTextureFormat format = case format of
	UncompressedTextureFormat
		{ textureFormatComponents = components
		, textureFormatValueType = vt
		, textureFormatPixelSize = pixelSize
		, textureFormatColorSpace = colorSpace
		} -> case (components, vt, pixelSize, colorSpace) of
		(PixelR, PixelUint, Pixel8bit, LinearColorSpace) -> (False, gl_R8, gl_RED, gl_UNSIGNED_BYTE)
		(PixelR, PixelUint, Pixel16bit, LinearColorSpace) -> (False, gl_R16, gl_RED, gl_UNSIGNED_SHORT)
		(PixelR, PixelFloat, Pixel16bit, LinearColorSpace) -> (False, gl_R16F, gl_RED, gl_FLOAT)
		(PixelR, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, gl_R32F, gl_RED, gl_FLOAT)
		(PixelRG, PixelUint, Pixel16bit, LinearColorSpace) -> (False, gl_RG8, gl_RG, gl_UNSIGNED_BYTE)
		(PixelRG, PixelUint, Pixel32bit, LinearColorSpace) -> (False, gl_RG16, gl_RG, gl_UNSIGNED_SHORT)
		(PixelRG, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, gl_RG16F, gl_RG, gl_FLOAT)
		(PixelRG, PixelFloat, Pixel64bit, LinearColorSpace) -> (False, gl_RG32F, gl_RG, gl_FLOAT)
		(PixelRGB, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, gl_R11F_G11F_B10F, gl_RGB, gl_FLOAT)
		(PixelRGB, PixelFloat, Pixel96bit, LinearColorSpace) -> (False, gl_RGB32F, gl_RGB, gl_FLOAT)
		(PixelRGBA, PixelUint, Pixel32bit, LinearColorSpace) -> (False, gl_RGBA8, gl_RGBA, gl_UNSIGNED_BYTE)
		(PixelRGBA, PixelUint, Pixel32bit, StandardColorSpace) -> (False, gl_SRGB8_ALPHA8, gl_RGBA, gl_UNSIGNED_BYTE)
		(PixelRGBA, PixelUint, Pixel64bit, LinearColorSpace) -> (False, gl_RGBA16, gl_RGBA, gl_UNSIGNED_SHORT)
		(PixelRGBA, PixelFloat, Pixel64bit, LinearColorSpace) -> (False, gl_RGBA16F, gl_RGBA, gl_FLOAT)
		(PixelRGBA, PixelFloat, Pixel128bit, LinearColorSpace) -> (False, gl_RGBA32F, gl_RGBA, gl_FLOAT)
		_ -> error $ show ("uncompressed texture format unsupported by OpenGL", format)
	CompressedTextureFormat
		{ textureFormatCompression = compression
		, textureFormatColorSpace = colorSpace
		} -> case (compression, colorSpace) of
		(TextureCompressionBC1, LinearColorSpace) -> (True, gl_COMPRESSED_RGB_S3TC_DXT1_EXT, gl_COMPRESSED_RGB, gl_UNSIGNED_BYTE)
		(TextureCompressionBC1, StandardColorSpace) -> (True, gl_COMPRESSED_SRGB_S3TC_DXT1_EXT, gl_COMPRESSED_RGB, gl_UNSIGNED_BYTE)
		(TextureCompressionBC1Alpha, LinearColorSpace) -> (True, gl_COMPRESSED_RGBA_S3TC_DXT1_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC1Alpha, StandardColorSpace) -> (True, gl_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC2, LinearColorSpace) -> (True, gl_COMPRESSED_RGBA_S3TC_DXT3_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC2, StandardColorSpace) -> (True, gl_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC3, LinearColorSpace) -> (True, gl_COMPRESSED_RGBA_S3TC_DXT5_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC3, StandardColorSpace) -> (True, gl_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT, gl_COMPRESSED_RGBA, gl_UNSIGNED_BYTE)
		(TextureCompressionBC4, LinearColorSpace) -> (True, gl_COMPRESSED_RED_RGTC1, gl_COMPRESSED_RED, gl_UNSIGNED_BYTE)
		(TextureCompressionBC4Signed, LinearColorSpace) -> (True, gl_COMPRESSED_SIGNED_RED_RGTC1, gl_COMPRESSED_RED, gl_UNSIGNED_BYTE)
		(TextureCompressionBC5, LinearColorSpace) -> (True, gl_COMPRESSED_RG_RGTC2, gl_COMPRESSED_RG, gl_UNSIGNED_BYTE)
		(TextureCompressionBC5Signed, LinearColorSpace) -> (True, gl_COMPRESSED_SIGNED_RG_RGTC2, gl_COMPRESSED_RG, gl_UNSIGNED_BYTE)
		_ -> error $ show ("compressed texture format unsupported by OpenGL", format)

-- | Helper enumeration.
data TextureType
	= Texture3D
	| Texture2DArray
	| Texture2D
	| Texture1DArray
	| Texture1D
	deriving Eq

glUpdateContext :: GlContext -> IO ()
glUpdateContext context@GlContext
	{ glContextCaps = GlCaps
		{ glCapsArbVertexAttribBinding = capArbVertexAttribBinding
		, glCapsArbInstancedArrays = capArbInstancedArrays
		}
	, glContextActualState = GlContextState
		{ glContextStateViewport = actualViewportRef
		, glContextStateVertexBuffers = actualVertexBuffersVector
		, glContextStateIndexBuffer = actualIndexBufferRef
		, glContextStateUniformBuffers = actualUniformBuffersVector
		, glContextStateSamplers = actualSamplersVector
		, glContextStateProgram = actualProgramRef
		, glContextStateDepthTestFunc = actualDepthTestFuncRef
		, glContextStateDepthWrite = actualDepthWriteRef
		, glContextStateBlendState = actualBlendStateRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateViewport = desiredViewportRef
		, glContextStateVertexBuffers = desiredVertexBuffersVector
		, glContextStateIndexBuffer = desiredIndexBufferRef
		, glContextStateUniformBuffers = desiredUniformBuffersVector
		, glContextStateSamplers = desiredSamplersVector
		, glContextStateProgram = desiredProgramRef
		, glContextStateDepthTestFunc = desiredDepthTestFuncRef
		, glContextStateDepthWrite = desiredDepthWriteRef
		, glContextStateBlendState = desiredBlendStateRef
		}
	, glContextBoundAttributesCount = boundAttributesCountRef
	} = do
	-- framebuffer
	glUpdateFrameBuffer context

	-- samplers
	vectorSetup actualSamplersVector desiredSamplersVector $ \i (GlTextureId textureName, GlSamplerStateId samplerName) -> do
		glActiveTexture $ gl_TEXTURE0 + fromIntegral i
		glBindTexture gl_TEXTURE_2D textureName
		glBindSampler (fromIntegral i) samplerName
		glCheckErrors 0 "bind sampler"

	-- program
	programUpdated <- refSetup actualProgramRef desiredProgramRef $ \GlProgramId
		{ glProgramName = programName
		, glProgramVertexArrayName = vertexArrayName
		} -> do
		-- program
		glUseProgram programName
		glCheckErrors 0 "bind program"

		-- if vertex array is supported
		if vertexArrayName > 0 then do
			-- bind vertex array
			glBindVertexArray vertexArrayName
			glCheckErrors 0 "bind vertex array"
			-- reset current index buffer binding in order to refresh (as element array buffer is part of VAO state)
			writeIORef actualIndexBufferRef $ GlIndexBufferId (-1) 0
		else return ()

	-- uniform buffers
	uniformBindings <- liftM glProgramUniforms $ readIORef desiredProgramRef
	forM_ [0, VM.length actualUniformBuffersVector - 1] $ \i -> do
		actualUniformBuffer <- VM.read actualUniformBuffersVector i
		desiredUniformBuffer <- VM.read desiredUniformBuffersVector i
		let bindBuffer bufferName = do
			glBindBufferBase gl_UNIFORM_BUFFER (fromIntegral i) bufferName
			glCheckErrors 0 "bind uniform buffer"
		let updateActual = VM.write actualUniformBuffersVector i desiredUniformBuffer
		let bindMemoryBuffer bytes = do
			-- get uniforms for this slot
			let uniforms = uniformBindings V.! i
			-- setup manually
			B.unsafeUseAsCString bytes $ \bytesPtr -> forM_ uniforms $ \GlUniform
				{ glUniformLocation = location
				, glUniformOffset = offset
				, glUniformSize = size
				, glUniformType = t
				} -> do
				let ptr = plusPtr bytesPtr offset
				case t of
					ScalarValueType ScalarFloat -> glUniform1fv location size $ castPtr ptr
					ScalarValueType ScalarInt -> glUniform1iv location size $ castPtr ptr
					ScalarValueType ScalarUint -> glUniform1uiv location size $ castPtr ptr
					ScalarValueType ScalarBool -> glUniform1iv location size $ castPtr ptr
					VectorValueType Dimension1 ScalarFloat -> glUniform1fv location size $ castPtr ptr
					VectorValueType Dimension2 ScalarFloat -> glUniform2fv location size $ castPtr ptr
					VectorValueType Dimension3 ScalarFloat -> glUniform3fv location size $ castPtr ptr
					VectorValueType Dimension4 ScalarFloat -> glUniform4fv location size $ castPtr ptr
					VectorValueType Dimension1 ScalarInt -> glUniform1iv location size $ castPtr ptr
					VectorValueType Dimension2 ScalarInt -> glUniform2iv location size $ castPtr ptr
					VectorValueType Dimension3 ScalarInt -> glUniform3iv location size $ castPtr ptr
					VectorValueType Dimension4 ScalarInt -> glUniform4iv location size $ castPtr ptr
					VectorValueType Dimension1 ScalarUint -> glUniform1uiv location size $ castPtr ptr
					VectorValueType Dimension2 ScalarUint -> glUniform2uiv location size $ castPtr ptr
					VectorValueType Dimension3 ScalarUint -> glUniform3uiv location size $ castPtr ptr
					VectorValueType Dimension4 ScalarUint -> glUniform4uiv location size $ castPtr ptr
					VectorValueType Dimension1 ScalarBool -> glUniform1iv location size $ castPtr ptr
					VectorValueType Dimension2 ScalarBool -> glUniform2iv location size $ castPtr ptr
					VectorValueType Dimension3 ScalarBool -> glUniform3iv location size $ castPtr ptr
					VectorValueType Dimension4 ScalarBool -> glUniform4iv location size $ castPtr ptr
					MatrixValueType Dimension3 Dimension3 ScalarFloat -> glUniformMatrix3fv location size 0 $ castPtr ptr
					MatrixValueType Dimension4 Dimension4 ScalarFloat -> glUniformMatrix4fv location size 0 $ castPtr ptr
					_ -> return ()
				glCheckErrors 0 "set uniform"
		case desiredUniformBuffer of
			GlUniformBufferId bufferName _bufferSize -> case actualUniformBuffer of
				GlUniformBufferId prevBufferName _prevBufferSize -> do
					if bufferName /= prevBufferName then do
						bindBuffer bufferName
						updateActual
					else return ()
				GlUniformMemoryBufferId _ -> do
					bindBuffer bufferName
					updateActual
				GlNullUniformBufferId -> do
					bindBuffer bufferName
					updateActual
			GlUniformMemoryBufferId bytesRef -> case actualUniformBuffer of
				GlUniformBufferId _ _ -> do
					bindBuffer 0
					bytes <- readIORef bytesRef
					bindMemoryBuffer bytes
					updateActual
				GlUniformMemoryBufferId prevBytesRef -> do
					bytes <- readIORef bytesRef
					prevBytes <- readIORef prevBytesRef
					if bytes /= prevBytes then do
						bindMemoryBuffer bytes
						updateActual
					else return ()
				GlNullUniformBufferId -> do
					bytes <- readIORef bytesRef
					bindMemoryBuffer bytes
					updateActual
			GlNullUniformBufferId -> case actualUniformBuffer of
				GlUniformBufferId _prevBufferName _prevBufferSize -> do
					bindBuffer 0
					updateActual
				GlUniformMemoryBufferId _ -> do
					updateActual
				GlNullUniformBufferId -> return ()

	-- if vertex array is supported, bind vertex buffers
	if capArbVertexAttribBinding then do
		vectorSetupCond programUpdated actualVertexBuffersVector desiredVertexBuffersVector $ \i (GlVertexBufferId bufferName stride) -> do
			glBindVertexBuffer (fromIntegral i) bufferName 0 (fromIntegral stride)
			glCheckErrors 0 "bind vertex buffer"
	-- otherwise manually bind attributes
	else do
		vectorSetupCond programUpdated actualVertexBuffersVector desiredVertexBuffersVector $ \_ (GlVertexBufferId bufferName stride) -> do
			-- bind buffer
			glBindBuffer gl_ARRAY_BUFFER bufferName
			glCheckErrors 0 "bind array buffer"

			-- bind attributes
			attributeSlots <- liftM glProgramAttributeSlots $ readIORef desiredProgramRef
			V.forM_ attributeSlots $ \GlAttributeSlot
				{ glAttributeSlotElements = elements
				, glAttributeSlotDivisor = divisor
				} -> do
				V.forM_ elements $ \GlAttribute
					{ glAttributeIndex = i
					, glAttributeSize = size
					, glAttributeType = t
					, glAttributeIsNormalized = isNormalized
					, glAttributeIsInteger = isInteger
					, glAttributeOffset = offset
					} -> do

					glEnableVertexAttribArray i
					glCheckErrors 0 "enable vertex attrib array"

					if isInteger then
						glVertexAttribIPointer i size t stride (intPtrToPtr offset)
					else
						glVertexAttribPointer i size t isNormalized stride (intPtrToPtr offset)
					glCheckErrors 0 "vertex attrib pointer"

					if capArbInstancedArrays then do
						glVertexAttribDivisor i divisor
						glCheckErrors 0 "vertex attrib divisor"
					else return ()

	-- disable unused attributes
	newBoundAttributesCount <- liftM glProgramAttributesCount $ readIORef desiredProgramRef
	oldBoundAttributesCount <- readIORef boundAttributesCountRef
	forM_ [newBoundAttributesCount .. (oldBoundAttributesCount - 1)] $ \i -> do
		glDisableVertexAttribArray (fromIntegral i)
		glCheckErrors 0 "disable unused vertex attrib array"
	writeIORef boundAttributesCountRef newBoundAttributesCount

	-- index buffer
	refSetup_ actualIndexBufferRef desiredIndexBufferRef $ \(GlIndexBufferId indexBufferName _indicesType) -> do
		glBindBuffer gl_ELEMENT_ARRAY_BUFFER indexBufferName
		glCheckErrors 0 "bind index buffer"

	-- viewport
	refSetup_ actualViewportRef desiredViewportRef $ \(viewportWidth, viewportHeight) -> do
		glViewport 0 0 (fromIntegral viewportWidth) (fromIntegral viewportHeight)
		glCheckErrors 0 "bind viewport"

	-- depth test func & depth write
	do
		actualDepthTestFunc <- readIORef actualDepthTestFuncRef
		desiredDepthTestFunc <- readIORef desiredDepthTestFuncRef
		actualDepthWrite <- readIORef actualDepthWriteRef
		desiredDepthWrite <- readIORef desiredDepthWriteRef
		-- enable or disable depth test
		if actualDepthTestFunc /= desiredDepthTestFunc || actualDepthWrite /= desiredDepthWrite then do
			(if desiredDepthTestFunc /= DepthTestFuncAlways || desiredDepthWrite then glEnable else glDisable) gl_DEPTH_TEST
			glCheckErrors 0 "enable/disable depth test"
		else return ()
		-- depth test func
		if actualDepthTestFunc /= desiredDepthTestFunc then do
			let func = case desiredDepthTestFunc of
				DepthTestFuncNever -> gl_NEVER
				DepthTestFuncLess -> gl_LESS
				DepthTestFuncLessOrEqual -> gl_LEQUAL
				DepthTestFuncEqual -> gl_EQUAL
				DepthTestFuncNonEqual -> gl_NOTEQUAL
				DepthTestFuncGreaterOrEqual -> gl_GEQUAL
				DepthTestFuncGreater -> gl_GREATER
				DepthTestFuncAlways -> gl_ALWAYS
			glDepthFunc func
			glCheckErrors 0 "set depth test func"
			writeIORef actualDepthTestFuncRef desiredDepthTestFunc
		else return ()
		-- depth write
		if actualDepthWrite /= desiredDepthWrite then do
			glDepthMask (if desiredDepthWrite then 1 else 0)
			glCheckErrors 0 "set depth write"
			writeIORef actualDepthWriteRef desiredDepthWrite
		else return ()

	-- blend state
	refSetup_ actualBlendStateRef desiredBlendStateRef $ \blendStateId -> case blendStateId of
		GlBlendStateId BlendStateInfo
			{ blendSourceColor = sourceColor
			, blendDestColor = destColor
			, blendColorOperation = colorOperation
			, blendSourceAlpha = sourceAlpha
			, blendDestAlpha = destAlpha
			, blendAlphaOperation = alphaOperation
			} -> do
			-- enable blending
			glEnable gl_BLEND
			glCheckErrors 0 "enable blending"

			-- convert-to-OpenGL functions
			let convertColor c = case c of
				ColorSourceZero -> gl_ZERO
				ColorSourceOne -> gl_ONE
				ColorSourceSrc -> gl_SRC_COLOR
				ColorSourceInvSrc -> gl_ONE_MINUS_SRC_COLOR
				ColorSourceSrcAlpha -> gl_SRC_ALPHA
				ColorSourceInvSrcAlpha -> gl_ONE_MINUS_SRC_ALPHA
				ColorSourceDest -> gl_DST_COLOR
				ColorSourceInvDest -> gl_ONE_MINUS_DST_COLOR
				ColorSourceDestAlpha -> gl_DST_ALPHA
				ColorSourceInvDestAlpha -> gl_ONE_MINUS_DST_ALPHA
				ColorSourceSecondSrc -> gl_SRC1_COLOR
				ColorSourceInvSecondSrc -> gl_ONE_MINUS_SRC1_COLOR
				ColorSourceSecondSrcAlpha -> gl_SRC1_ALPHA
				ColorSourceInvSecondSrcAlpha -> gl_ONE_MINUS_SRC1_ALPHA
			let convertAlpha a = case a of
				AlphaSourceZero -> gl_ZERO
				AlphaSourceOne -> gl_ONE
				AlphaSourceSrc -> gl_SRC_ALPHA
				AlphaSourceInvSrc -> gl_ONE_MINUS_SRC_ALPHA
				AlphaSourceDest -> gl_DST_ALPHA
				AlphaSourceInvDest -> gl_ONE_MINUS_DST_ALPHA
				AlphaSourceSecondSrc -> gl_SRC1_ALPHA
				AlphaSourceInvSecondSrc -> gl_ONE_MINUS_SRC1_ALPHA
			let convertOperation o = case o of
				BlendOperationAdd -> gl_FUNC_ADD
				BlendOperationSubtractAB -> gl_FUNC_SUBTRACT
				BlendOperationSubtractBA -> gl_FUNC_REVERSE_SUBTRACT
				BlendOperationMin -> gl_MIN
				BlendOperationMax -> gl_MAX

			-- set blend funcs
			glBlendFuncSeparate (convertColor sourceColor) (convertColor destColor) (convertAlpha sourceAlpha) (convertAlpha destAlpha)
			glCheckErrors 0 "set blend func"
			-- set blend operations
			glBlendEquationSeparate (convertOperation colorOperation) (convertOperation alphaOperation)
			glCheckErrors 0 "set blend equation"

		GlNullBlendStateId -> do
			-- disable blending
			glDisable gl_BLEND
			glCheckErrors 0 "disable blending"

glUpdateFrameBuffer :: GlContext -> IO ()
glUpdateFrameBuffer GlContext
	{ glContextActualState = GlContextState
		{ glContextStateFrameBuffer = actualFrameBufferRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateFrameBuffer = desiredFrameBufferRef
		}
	} = refSetup_ actualFrameBufferRef desiredFrameBufferRef $ \(GlFrameBufferId name) -> do
	glBindFramebuffer gl_FRAMEBUFFER name
	glCheckErrors 0 "bind framebuffer"

glEnableDepthWriteForClearing :: GlContext -> IO ()
glEnableDepthWriteForClearing GlContext
	{ glContextActualState = GlContextState
		{ glContextStateDepthTestFunc = actualDepthTestFuncRef
		, glContextStateDepthWrite = actualDepthWriteRef
		}
	} = do
	-- enable depth test
	glEnable gl_DEPTH_TEST
	glDepthFunc gl_ALWAYS
	writeIORef actualDepthTestFuncRef DepthTestFuncAlways
	-- enable depth write
	glDepthMask 1
	writeIORef actualDepthWriteRef True

-- | Get size, type, isNormalized and isInteger for OpenGL by attribute type.
glGetAttributeSTNI :: AttributeType -> (GLint, GLenum, GLboolean, Bool)
glGetAttributeSTNI at = case at of
	ATFloat32 -> (1, gl_FLOAT, 0, False)
	ATFloat16 -> (1, gl_HALF_FLOAT, 0, False)
	ATInt32 n -> let nn = normalized n in (1, gl_INT, if nn then 1 else 0, not nn)
	ATInt16 n -> let nn = normalized n in (1, gl_SHORT, if nn then 1 else 0, not nn)
	ATInt8 n -> let nn = normalized n in (1, gl_BYTE, if nn then 1 else 0, not nn)
	ATUint32 n -> let nn = normalized n in (1, gl_UNSIGNED_INT, if nn then 1 else 0, not nn)
	ATUint16 n -> let nn = normalized n in (1, gl_UNSIGNED_SHORT, if nn then 1 else 0, not nn)
	ATUint8 n -> let nn = normalized n in (1, gl_UNSIGNED_BYTE, if nn then 1 else 0, not nn)
	ATVec1 r -> glGetAttributeSTNI r
	ATVec2 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (2, t, n, i)
	ATVec3 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (3, t, n, i)
	ATVec4 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (4, t, n, i)
	-- matrices are not supported in attributes
	_ -> (0, 0, 0, False) -- invalid values
	where normalized n = case n of
		NonNormalized -> False
		Normalized -> True

refSetup :: Eq a => IORef a -> IORef a -> (a -> IO ()) -> IO Bool
refSetup actualRef desiredRef setup = do
	actual <- readIORef actualRef
	desired <- readIORef desiredRef
	if actual /= desired then do
		setup desired
		writeIORef actualRef desired
		return True
	else return False

refSetup_ :: Eq a => IORef a -> IORef a -> (a -> IO ()) -> IO ()
refSetup_ actualRef desiredRef setup = do
	_ <- refSetup actualRef desiredRef setup
	return ()

vectorSetupCond :: Eq a => Bool -> VM.IOVector a -> VM.IOVector a -> (Int -> a -> IO ()) -> IO ()
vectorSetupCond forceSetup actualVector desiredVector setup = do
	let len = VM.length actualVector
	forM_ [0..(len - 1)] $ \i -> do
		actual <- VM.unsafeRead actualVector i
		desired <- VM.unsafeRead desiredVector i
		if forceSetup || actual /= desired then do
			setup i desired
			VM.unsafeWrite actualVector i desired
		else return ()

vectorSetup :: Eq a => VM.IOVector a -> VM.IOVector a -> (Int -> a -> IO ()) -> IO ()
vectorSetup = vectorSetupCond False

instance Eq (IndexBufferId GlDevice) where
	(GlIndexBufferId indexBufferName1 _indicesType1) == (GlIndexBufferId indexBufferName2 _indicesType2) = indexBufferName1 == indexBufferName2

instance Eq (ProgramId GlDevice) where
	(GlProgramId { glProgramName = name1 }) == (GlProgramId { glProgramName = name2 }) = name1 == name2

-- | Check for OpenGL errors, throw an exception if there's some.
glCheckErrors :: Int -> String -> IO ()
glCheckErrors level msg = if level < glErrorLevel then return () else do
	firstError <- glGetError
	if firstError == gl_NO_ERROR then return ()
	else do
		let f restErrors = do
			nextError <- glGetError
			if nextError == gl_NO_ERROR then return restErrors
			else f $ nextError : restErrors
		errors <- f [firstError]
		throwIO $ DescribeFirstException $ show ("OpenGL error", msg, errors)

-- | Clear OpenGL errors.
glClearErrors :: IO ()
glClearErrors = do
	e <- glGetError
	if e == gl_NO_ERROR then return ()
	else glClearErrors

-- | Minimal level of error checking.
-- Increase this number to do less checks.
glErrorLevel :: Int
glErrorLevel = 0
