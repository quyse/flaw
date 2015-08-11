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
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.ARB.TextureStorage
import Graphics.Rendering.OpenGL.Raw.ARB.UniformBufferObject
import Graphics.Rendering.OpenGL.Raw.Core30
import Graphics.Rendering.OpenGL.Raw.EXT.TextureCompressionS3TC
import Graphics.Rendering.OpenGL.Raw.EXT.TextureSRGB
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Video as SDL

import Flaw.Exception
import Flaw.Graphics.Blend
import Flaw.Graphics.Internal
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
	}

type GlDevice = GlContext

instance Device GlContext where
	type DeferredContext GlContext = GlContext
	newtype TextureId GlContext = GlTextureId GLuint
	newtype SamplerStateId GlContext = GlSamplerStateId GLuint
	newtype BlendStateId GlContext = GlBlendStateId BlendStateInfo
	newtype RenderTargetId GlContext = GlRenderTargetId GLuint
	newtype DepthStencilTargetId GlContext = GlDepthStencilTargetId GLuint
	newtype FrameBufferId GlContext = GlFrameBufferId GLuint
	data VertexBufferId GlContext = GlVertexBufferId GLuint Int
	data IndexBufferId GlContext = GlIndexBufferId GLuint Int
	newtype ProgramId GlContext = GlProgramId GLuint
	data UniformBufferId GlContext = GlUniformBufferId GLuint Int

	nullTexture = GlTextureId 0
	nullSamplerState = GlSamplerStateId 0
	nullBlendState = GlBlendStateId defaultBlendStateInfo
	nullDepthStencilTarget = GlDepthStencilTargetId 0
	nullIndexBuffer = GlIndexBufferId 0 0
	nullUniformBuffer = GlUniformBufferId 0

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

	createSamplerState = undefined

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

		return (GlIndexBufferId bufferName (if is32Bit then 4 else 2), with bufferName $ glDeleteBuffers 1)

	createProgram _context program = describeException "failed to create OpenGL program" $ do
		

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
			glBufferData gl_UNIFORM_BUFFER size nullPtr gl_DYNAMIC_DRAW
			
			return (GlUniformBufferId bufferName size, with bufferName $ glDeleteBuffers 1)
		else return (GlUniformBufferId 0 size, return ())

instance Context GlContext GlContext where

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
	return (GlContext
		{ glContextContext = glContext
		, glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = capArbUniformBufferObject
			, glCapsArbSamplerObjects = capArbSamplerObjects
			, glCapsArbVertexAttribBinding = capArbVertexAttribBinding
			, glCapsArbFramebufferObject = capArbFramebufferObject
			, glCapsArbTextureStorage = capArbTextureStorage
			, glCapsArbInstancedArrays = capArbInstancedArrays
			}
		}, SDL.glDeleteContext glContext)

isGlExtensionSupported :: String -> IO Bool
isGlExtensionSupported name = withCString name SDL.glExtensionSupported

-- | Returns (compressed, internal format, format, type)
glFormatFromTextureFormat :: TextureFormat -> (Bool, GLenum, GLenum, GLenum)
glFormatFromTextureFormat format = case format of
	UncompressedTextureFormat
		{ textureFormatComponents = components
		, textureFormatValueType = valueType
		, textureFormatPixelSize = pixelSize
		, textureFormatColorSpace = colorSpace
		} -> case (components, valueType, pixelSize, colorSpace) of
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
