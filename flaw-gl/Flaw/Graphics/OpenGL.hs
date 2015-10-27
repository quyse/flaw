{-|
Module: Flaw.Graphics.OpenGL
Description: OpenGL graphics implementation.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

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
	, glContextActualState :: GlContextState
	, glContextDesiredState :: GlContextState
	}

data GlContextState = GlContextState
	{ glContextStateFrameBuffer :: !(IORef (FrameBufferId GlDevice))
	, glContextStateViewport :: !(IORef (Int, Int))
	, glContextStateVertexBuffers :: !(VM.IOVector (VertexBufferId GlDevice))
	, glContextStateIndexBuffer :: !(IORef (IndexBufferId GlDevice))
	, glContextStateUniformBuffers :: !(VM.IOVector (UniformBufferId GlDevice))
	, glContextStateSamplers :: !(VM.IOVector (TextureId GlDevice, SamplerStateId GlDevice))
	, glContextStateProgram :: !(IORef (ProgramId GlDevice))
	, glContextStateAttributes :: !(IORef ([Attribute], VertexBufferId GlDevice))
	, glContextStateDepthTestFunc :: !(IORef DepthTestFunc)
	, glContextStateDepthWrite :: !(IORef Bool)
	}

type GlDevice = GlContext

data GlAttributeSlot = GlAttributeSlot
	{ glAttributeSlotElements :: !(V.Vector Attribute)
	, glAttributeSlotDivisor :: !GLuint
	}

data GlUniform = GlUniform
	{ glUniformLocation :: !GLint
	, glUniformInfo :: !Uniform
	}

instance Device GlContext where
	type DeferredContext GlContext = GlContext
	newtype TextureId GlContext = GlTextureId GLuint deriving Eq
	newtype SamplerStateId GlContext = GlSamplerStateId GLuint deriving Eq
	newtype BlendStateId GlContext = GlBlendStateId BlendStateInfo
	newtype RenderTargetId GlContext = GlRenderTargetId GLuint
	newtype DepthStencilTargetId GlContext = GlDepthStencilTargetId GLuint
	newtype FrameBufferId GlContext = GlFrameBufferId GLuint deriving Eq
	data VertexBufferId GlContext = GlVertexBufferId !GLuint !Int
	data IndexBufferId GlContext = GlIndexBufferId !GLuint !GLenum
	data ProgramId GlContext = GlProgramId
		{ glProgramName :: !GLuint
		, glProgramVertexArrayName :: !GLuint
		, glProgramAttributeSlots ::  !(V.Vector GlAttributeSlot)
		, glProgramUniforms :: !(V.Vector GlUniform)
		}
	data UniformBufferId GlContext
		-- | Real uniform buffer: buffer name, size.
		= GlUniformBufferId !GLuint !Int
		-- | Emulated uniform buffer: bytestring ref, size.
		| GlUniformMemoryBufferId !(IORef B.ByteString) !Int

	nullTexture = GlTextureId 0
	nullSamplerState = GlSamplerStateId 0
	nullBlendState = GlBlendStateId defaultBlendStateInfo
	nullDepthStencilTarget = GlDepthStencilTargetId 0
	nullIndexBuffer = GlIndexBufferId 0 gl_UNSIGNED_SHORT
	nullUniformBuffer = GlUniformBufferId 0 0

	createDeferredContext = undefined

	createStaticTexture GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} textureInfo@TextureInfo
		{ textureFormat = format
		} bytes = describeException ("failed to create OpenGL static texture", textureInfo) $ do

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
			peek namePtr

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		glPixelStorei gl_UNPACK_ALIGNMENT 1

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

		-- allocate texture storage, if we use it
		if useTextureStorage then case textureType of
			Texture3D      -> glTexStorage3D glTarget mips glInternalFormat width height depth
			Texture2DArray -> glTexStorage3D glTarget mips glInternalFormat width height count
			Texture2D      -> glTexStorage2D glTarget mips glInternalFormat width height
			Texture1DArray -> glTexStorage2D glTarget mips glInternalFormat width count
			Texture1D      -> glTexStorage1D glTarget mips glInternalFormat width
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
			else return ()

			-- set unpack row length if needed
			if textureType == Texture3D || textureType == Texture2DArray || textureType == Texture2D || textureType == Texture1DArray then do
				glPixelStorei gl_UNPACK_ROW_LENGTH $ mipLinePitch `div` pixelSize
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

		if useTextureStorage then return ()
		else do
			glTexParameteri glTarget gl_TEXTURE_BASE_LEVEL 0
			glTexParameteri glTarget gl_TEXTURE_MAX_LEVEL $ mips - 1

		return (GlTextureId textureName, with textureName $ glDeleteTextures 1)

	createSamplerState _context _samplerStateInfo = describeException "failed to create OpenGL sampler state" $ do
		return (GlSamplerStateId 0, return ())

	createBlendState _context blendStateInfo = return (GlBlendStateId blendStateInfo, return ())

	createReadableRenderTarget GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} width height format = describeException "failed to create OpenGL readable render target" $ do
		-- allocate texture name
		textureName <- alloca $ \namePtr -> do
			glGenTextures 1 namePtr
			peek namePtr

		glBindTexture gl_TEXTURE_2D textureName

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		if compressed then throwIO $ DescribeFirstException "render target cannot use compressed format"
		else return ()

		if useTextureStorage then
			glTexStorage2D gl_TEXTURE_2D 1 glInternalFormat (fromIntegral width) (fromIntegral height)
		else
			glTexImage2D gl_TEXTURE_2D 0 (fromIntegral glInternalFormat) (fromIntegral width) (fromIntegral height) 0 glFormat glType nullPtr

		return ((GlRenderTargetId textureName, GlTextureId textureName), with textureName $ glDeleteTextures 1)

	createDepthStencilTarget _context width height = describeException "failed to create OpenGL depth stencil target" $ do
		-- allocate texture name
		textureName <- alloca $ \namePtr -> do
			glGenTextures 1 namePtr
			peek namePtr

		glBindTexture gl_TEXTURE_2D textureName
		glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_DEPTH_STENCIL) (fromIntegral width) (fromIntegral height) 0 gl_DEPTH_STENCIL gl_UNSIGNED_INT_24_8 nullPtr

		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
		glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_R $ fromIntegral gl_CLAMP_TO_EDGE

		return (GlDepthStencilTargetId textureName, with textureName $ glDeleteTextures 1)

	createReadableDepthStencilTarget context width height = do
		(depthStencilTarget@(GlDepthStencilTargetId bufferName), destroy) <- createDepthStencilTarget context width height
		return ((depthStencilTarget, GlTextureId bufferName), destroy)

	createFrameBuffer _context renderTargets (GlDepthStencilTargetId depthStencilName) = describeException "failed to create OpenGL framebuffer" $ do
		-- allocate framebuffer name
		framebufferName <- alloca $ \namePtr -> do
			glGenFramebuffers 1 namePtr
			peek namePtr

		-- bind render targets
		forM_ (zip [0..] renderTargets) $ \(i, GlRenderTargetId renderTargetName) -> do
			glFramebufferTexture2D gl_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + i) gl_TEXTURE_2D renderTargetName 0

		-- bind depth-stencil target
		glFramebufferTexture2D gl_FRAMEBUFFER gl_DEPTH_STENCIL_ATTACHMENT gl_TEXTURE_2D depthStencilName 0

		return (GlFrameBufferId framebufferName, with framebufferName $ glDeleteFramebuffers 1)

	createStaticVertexBuffer _context bytes stride = describeException "failed to create OpenGL static vertex buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			peek namePtr

		glBindBuffer gl_ARRAY_BUFFER bufferName
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			glBufferData gl_ARRAY_BUFFER (fromIntegral bytesLen) bytesPtr gl_STATIC_DRAW

		return (GlVertexBufferId bufferName stride, with bufferName $ glDeleteBuffers 1)

	createDynamicVertexBuffer _context size stride = describeException "failed to create OpenGL dynamic vertex buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			peek namePtr

		glBindBuffer gl_ARRAY_BUFFER bufferName
		glBufferData gl_ARRAY_BUFFER (fromIntegral size) nullPtr gl_DYNAMIC_DRAW

		return (GlVertexBufferId bufferName stride, with bufferName $ glDeleteBuffers 1)

	createStaticIndexBuffer _context bytes is32Bit = describeException "failed to create OpenGL static index buffer" $ do
		-- allocate buffer name
		bufferName <- alloca $ \namePtr -> do
			glGenBuffers 1 namePtr
			peek namePtr

		glBindBuffer gl_ELEMENT_ARRAY_BUFFER bufferName
		B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			glBufferData gl_ELEMENT_ARRAY_BUFFER (fromIntegral bytesLen) bytesPtr gl_STATIC_DRAW

		return (GlIndexBufferId bufferName (if is32Bit then gl_UNSIGNED_INT else gl_UNSIGNED_SHORT), with bufferName $ glDeleteBuffers 1)

	createProgram GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = useUniformBufferObject
			, glCapsArbVertexAttribBinding = capArbVertexAttribBinding
			}
		} program = describeException "failed to create OpenGL program" $ do

		bk <- newBook

		let createShader (GlslShader source) shaderType = describeException "failed to create OpenGL shader" $ do
			shaderName <- glCreateShader shaderType
			glCheckErrors 0 "create shader"
			B.unsafeUseAsCStringLen (T.encodeUtf8 source) $ \(sourcePtr, sourceLen) -> do
				with sourcePtr $ \sourcePtrPtr -> do
					with (fromIntegral sourceLen) $ \sourceLenPtr -> do
						glShaderSource shaderName 1 sourcePtrPtr sourceLenPtr
			glCheckErrors 0 "set shader source"
			glCompileShader shaderName
			glCheckErrors 0 "compile shader"
			status <- alloca $ \statusPtr -> do
				glGetShaderiv shaderName gl_COMPILE_STATUS statusPtr
				peek statusPtr
			if status == 1 then return (shaderName, glDeleteShader shaderName)
			else do
				logLength <- alloca $ \logLengthPtr -> do
					glGetShaderiv shaderName gl_INFO_LOG_LENGTH logLengthPtr
					peek logLengthPtr
				logBytes <- allocaBytes (fromIntegral logLength) $ \logPtr -> do
					realLogLength <- alloca $ \logLengthPtr -> do
						glGetShaderInfoLog shaderName logLength logLengthPtr logPtr
						peek logLengthPtr
					B.packCStringLen (logPtr, fromIntegral realLogLength)
				glClearErrors
				throwIO $ DescribeFirstException ("failed to compile shader", T.decodeUtf8 logBytes)

		-- generate GLSL
		let glslConfig = GlslConfig
			{ glslConfigForceFloatAttributes = False
			, glslConfigUnsignedUnsupported = False
			, glslConfigDeclareTargets = True
			, glslConfigUniformBlocks = useUniformBufferObject
			}
		glslProgram <- liftM (generateProgram glslConfig) $ runProgram program
		case glslProgram of
			GlslVertexPixelProgram attributes uniforms samplers targets vertexShader pixelShader -> do
				-- create program
				programName <- book bk $ do
					p <- glCreateProgram
					return (p, glDeleteProgram p)

				-- create and attach shaders
				glAttachShader programName =<< book bk (createShader vertexShader gl_VERTEX_SHADER)
				glCheckErrors 0 "attach vertex shader"
				glAttachShader programName =<< book bk (createShader pixelShader gl_FRAGMENT_SHADER)
				glCheckErrors 0 "attach pixel shader"

				-- bind attributes
				forM_ (zip attributes [0..]) $ \(GlslAttribute
					{ glslAttributeName = attributeName
					}, i) -> do
					B.useAsCString (T.encodeUtf8 attributeName) $ glBindAttribLocation programName i
					glCheckErrors 0 "bind attribute location"

				-- bind targets
				forM_ (zip targets [0..]) $ \(GlslTarget
					{ glslTargetName = targetName
					, glslTargetIndex = targetIndex
					}, i) -> do
					B.useAsCString (T.encodeUtf8 targetName) $ glBindFragDataLocation programName (fromIntegral targetIndex)
					glCheckErrors 0 "bind frag data location"

				-- link program
				glLinkProgram programName
				glCheckErrors 0 "link program"
				status <- alloca $ \statusPtr -> do
					glGetProgramiv programName gl_LINK_STATUS statusPtr
					peek statusPtr
				if status == 1 then return ()
				else do
					logLength <- alloca $ \logLengthPtr -> do
						glGetProgramiv programName gl_INFO_LOG_LENGTH logLengthPtr
						peek logLengthPtr
					logBytes <- allocaBytes (fromIntegral logLength) $ \logPtr -> do
						realLogLength <- alloca $ \logLengthPtr -> do
							glGetProgramInfoLog programName logLength logLengthPtr logPtr
							peek logLengthPtr
						B.packCStringLen (logPtr, fromIntegral realLogLength)
					glClearErrors
					throwIO $ DescribeFirstException ("failed to link program", T.decodeUtf8 logBytes)

				-- set as current
				glUseProgram programName
				glCheckErrors 0 "set program"

				-- bind uniform blocks
				forM_ uniforms $ \GlslUniformBlock
					{ glslUniformName = uniformName
					, glslUniformInfo = Uniform
						{ uniformSlot = slot
						}
					} -> do
					index <- B.useAsCString (T.encodeUtf8 uniformName) $ glGetUniformBlockIndex programName
					glCheckErrors 0 "get uniform block index"
					glUniformBlockBinding programName index (fromIntegral slot)
					glCheckErrors 0 "uniform block binding"

				-- bind samplers
				forM_ (zip samplers [0..]) $ \(GlslSampler
					{ glslSamplerName = samplerName
					, glslSamplerInfo = Sampler
						{ samplerSlot = slot
						}
					}, i) -> do
					location <- B.useAsCString (T.encodeUtf8 samplerName) $ glGetUniformLocation programName
					glCheckErrors 0 "get sampler location"
					glUniform1i location (fromIntegral slot)
					glCheckErrors 0 "bind sampler to texture unit"

				-- get non-buffer uniform bindings
				-- TODO

				-- reset current program
				glUseProgram 0
				glCheckErrors 0 "reset program"

				-- create vertex array if supported
				(vertexArrayName, attributeSlots) <- if capArbVertexAttribBinding then do
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
						{ glslAttributeName = aName
						, glslAttributeInfo = Attribute
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
						glVertexBindingDivisor i (fromIntegral divisor)
						glCheckErrors 0 "vertex binding divisor"

					-- unbind vertex array
					glBindVertexArray 0
					glCheckErrors 0 "unbind vertex array"

					return (vaName, V.empty)
				-- else create "manual" attribute binding
				else do
					-- sort attributes by slot
					let getSlot GlslAttribute
						{ glslAttributeInfo = Attribute
							{ attributeSlot = slot
							}
						} = slot
					let eqBySlot a b = getSlot a == getSlot b
					let compareBySlot a b = compare (getSlot a) (getSlot b)
					let attributesBySlot = groupBy eqBySlot $ sortBy compareBySlot attributes
					-- get maximum slot
					let maxSlot = maximum $ map (getSlot . head) attributesBySlot
					-- create attribute slots
					let attributeSlots = V.create $ do
						slots <- VM.replicate (maxSlot + 1) GlAttributeSlot
							{ glAttributeSlotElements = V.empty
							, glAttributeSlotDivisor = 0
							}
						forM_ attributesBySlot $ \as@(a : _) -> do
							let slot = getSlot a
							VM.write slots slot GlAttributeSlot
								{ glAttributeSlotElements = V.fromList $ map glslAttributeInfo as
								, glAttributeSlotDivisor = fromIntegral $ attributeDivisor $ glslAttributeInfo a
								}
						return slots

					return (0, attributeSlots)

				-- create uniform bindings
				let createUniformBinding GlslUniform
					{ glslUniformName = 
				uniformBindings = map 

				return (GlProgramId
					{ glProgramName = programName
					, glProgramVertexArrayName = vertexArrayName
					, glProgramAttributeSlots = attributeSlots
					, glProgramUniforms = uniformBindings
					}, freeBook bk)

	createUniformBuffer GlContext
		{ glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = useUniformBufferObject
			}
		} size = describeException "failed to create OpenGL uniform buffer" $ do

		if useUniformBufferObject then do
			-- allocate buffer name
			bufferName <- alloca $ \namePtr -> do
				glGenBuffers 1 namePtr
				peek namePtr

			glBindBuffer gl_UNIFORM_BUFFER bufferName
			glBufferData gl_UNIFORM_BUFFER (fromIntegral size) nullPtr gl_DYNAMIC_DRAW
			
			return (GlUniformBufferId bufferName size, with bufferName $ glDeleteBuffers 1)
		else do
			bufferRef <- newIORef B.empty
			return (GlUniformMemoryBufferId bufferRef size, return ())

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
		GlUniformMemoryBufferId bufferRef _bufferSize -> do
			-- remember buffer data
			writeIORef bufferRef bytes

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

	{-
	contextDraw :: c
		-> Int -- ^ Instances count (1 for non-instanced).
		-> Int -- ^ Indices count.
		-> IO ()
	contextPlay :: Context dc d => c -> dc -> IO ()
	contextRender :: c -> IO a -> IO a
	contextSetFrameBuffer :: c -> FrameBufferId d -> IO a -> IO a
	contextSetViewport :: c -> Int -> Int -> IO a -> IO a
	contextGetViewport :: c -> IO (Int, Int)
	contextSetVertexBuffer :: c -> Int -> VertexBufferId d -> IO a -> IO a
	contextSetIndexBuffer :: c -> IndexBufferId d -> IO a -> IO a
	contextSetUniformBuffer :: c -> Int -> UniformBufferId d -> IO a -> IO a
	contextSetSampler :: c -> Int -> TextureId d -> SamplerStateId d -> IO a -> IO a
	contextSetBlendState :: c -> BlendStateId d -> IO a -> IO a
	contextSetDepthTestFunc :: c -> DepthTestFunc -> IO a -> IO a
	contextSetDepthWrite :: c -> Bool -> IO a -> IO a
	contextSetProgram :: c -> ProgramId d -> IO a -> IO a
	-}

type GlPresenter = GlContext

instance Presenter GlContext GlSystem GlContext GlContext where

createGlSystem :: IO (GlSystem, IO ())
createGlSystem = return (GlSystem, return ())

data GlCaps = GlCaps
	{ glCapsArbUniformBufferObject :: !Bool
	, glCapsArbSamplerObjects :: !Bool
	, glCapsArbVertexAttribBinding :: !Bool
	, glCapsArbFramebufferObject :: !Bool
	, glCapsArbTextureStorage :: !Bool
	, glCapsArbInstancedArrays :: !Bool
	}

createGlContext :: DeviceId GlSystem -> SdlWindow -> IO (GlContext, IO ())
createGlContext _deviceId SdlWindow
	{ swSystem = ws
	, swHandle = windowHandle
	} = describeException "failed to create OpenGL device" $ invokeSdlWindowSystem ws $ do
	glContext <- SDL.glCreateContext windowHandle
	-- init capabilities
	capArbUniformBufferObject <- isGlExtensionSupported "ARB_uniform_buffer_object"
	capArbSamplerObjects <- isGlExtensionSupported "ARB_sampler_objects"
	capArbVertexAttribBinding <- isGlExtensionSupported "ARB_vertex_attrib_binding"
	capArbFramebufferObject <- isGlExtensionSupported "ARB_framebuffer_object"
	capArbTextureStorage <- isGlExtensionSupported "ARB_texture_storage"
	capArbInstancedArrays <- isGlExtensionSupported "ARB_instanced_arrays"

	-- create context
	actualContextState <- glCreateContextState
	desiredContextState <- glCreateContextState
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
		, glContextActualState = actualContextState
		, glContextDesiredState = desiredContextState
		}

	-- set front face mode
	glFrontFace gl_CW

	return (context, SDL.glDeleteContext glContext)

glCreateContextState :: IO GlContextState
glCreateContextState = do
	frameBuffer <- newIORef $ GlFrameBufferId 0
	viewport <- newIORef (0, 0)
	vertexBuffers <- VM.replicate 8 $ GlVertexBufferId 0 0
	indexBuffer <- newIORef $ GlIndexBufferId 0 gl_UNSIGNED_SHORT
	uniformBuffers <- VM.replicate 8 $ GlUniformBufferId 0 0
	samplers <- VM.replicate 8 (GlTextureId 0, GlSamplerStateId 0)
	program <- newIORef $ GlProgramId
		{ glProgramName = 0
		, glProgramVertexArrayName = 0
		, glProgramAttributeSlots = V.empty
		, glProgramUniforms = V.empty
		}
	attributes <- newIORef ([], GlVertexBufferId 0 0)
	depthTestFunc <- newIORef DepthTestFuncLess
	depthWrite <- newIORef True
	return GlContextState
		{ glContextStateFrameBuffer = frameBuffer
		, glContextStateViewport = viewport
		, glContextStateVertexBuffers = vertexBuffers
		, glContextStateIndexBuffer = indexBuffer
		, glContextStateUniformBuffers = uniformBuffers
		, glContextStateSamplers = samplers
		, glContextStateProgram = program
		, glContextStateAttributes = attributes
		, glContextStateDepthTestFunc = depthTestFunc
		, glContextStateDepthWrite = depthWrite
		}

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
		}
	, glContextActualState = GlContextState
		{ glContextStateViewport = actualViewportRef
		, glContextStateVertexBuffers = actualVertexBuffersVector
		, glContextStateIndexBuffer = actualIndexBufferRef
		, glContextStateUniformBuffers = actualUniformBuffersVector
		, glContextStateSamplers = actualSamplersVector
		, glContextStateProgram = actualProgramRef
		, glContextStateAttributes = actualAttributesVector
		, glContextStateDepthTestFunc = actualDepthTestFuncRef
		, glContextStateDepthWrite = actualDepthWriteRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateViewport = desiredViewportRef
		, glContextStateVertexBuffers = desiredVertexBuffersVector
		, glContextStateIndexBuffer = desiredIndexBufferRef
		, glContextStateUniformBuffers = desiredUniformBuffersVector
		, glContextStateSamplers = desiredSamplersVector
		, glContextStateProgram = desiredProgramRef
		, glContextStateAttributes = desiredAttributesVector
		, glContextStateDepthTestFunc = desiredDepthTestFuncRef
		, glContextStateDepthWrite = desiredDepthWriteRef
		}
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
	refSetup actualProgramRef desiredProgramRef $ \GlProgramId
		{ glProgramName = programName
		, glProgramVertexArrayName = vertexArrayName
		, glProgramAttributeSlots = attributeSlots
		, glProgramUniforms = uniforms
		} -> do
		-- program
		glUseProgram programName
		glCheckErrors 0 "bind program"
		-- vertex array
		if vertexArrayName > 0 then do
			glBindVertexArray vertexArrayName
			glCheckErrors 0 "bind vertex array"
		else return ()

	-- vertex buffers
	vectorSetup actualVertexBuffersVector desiredVertexBuffersVector $ \i (GlVertexBufferId bufferName stride) -> do
		glBindBuffer gl_ARRAY_BUFFER bufferName
		-- TODO

	-- viewport
	refSetup actualViewportRef desiredViewportRef $ \(viewportWidth, viewportHeight) -> do
		glViewport 0 0 (fromIntegral viewportWidth) (fromIntegral viewportHeight)
		glCheckErrors 0 "bind viewport"

glUpdateFrameBuffer :: GlContext -> IO ()
glUpdateFrameBuffer GlContext
	{ glContextActualState = GlContextState
		{ glContextStateFrameBuffer = actualFrameBufferRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateFrameBuffer = desiredFrameBufferRef
		}
	} = refSetup actualFrameBufferRef desiredFrameBufferRef $ \(GlFrameBufferId name) -> do
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
glGetAttributeSTNI t = case t of
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

refSetup :: Eq a => IORef a -> IORef a -> (a -> IO ()) -> IO ()
refSetup actualRef desiredRef setup = do
	actual <- readIORef actualRef
	desired <- readIORef desiredRef
	if actual /= desired then do
		setup desired
		writeIORef actualRef desired
	else return ()

vectorSetup :: Eq a => VM.IOVector a -> VM.IOVector a -> (Int -> a -> IO ()) -> IO ()
vectorSetup actualArray desiredArray setup = do
	let len = VM.length actualArray
	forM_ [0..(len - 1)] $ \i -> do
		actual <- VM.unsafeRead actualArray i
		desired <- VM.unsafeRead desiredArray i
		if actual /= desired then do
			setup i desired
			VM.unsafeWrite actualArray i desired
		else return ()

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
