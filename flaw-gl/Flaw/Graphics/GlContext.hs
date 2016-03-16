{-|
Module: Flaw.Graphics.GlContext
Description: OpenGL/WebGL context.
License: MIT
-}

{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, RankNTypes, TypeFamilies #-}

module Flaw.Graphics.GlContext
	( GlDevice
	, GlContext(..)
	, FrameBufferId(..)
	, TextureId(..)
	, GlCaps(..)
	, GlContextState(..)
	, newGlContext
	, glCheckErrors
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Foldable
import Data.List
import Data.IORef
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float

import Flaw.BinaryCache
import Flaw.Book
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.GLSL
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math

#if defined(ghcjs_HOST_OS)

import Flaw.Graphics.WebGL.FFI

#else

import Graphics.GL.ARB.GetProgramBinary
import Graphics.GL.ARB.TextureStorage
import Graphics.GL.ARB.UniformBufferObject
import Graphics.GL.ARB.VertexAttribBinding
import Graphics.GL.Core33
import Graphics.GL.EXT.TextureCompressionS3TC
import Graphics.GL.EXT.TextureSRGB

import Flaw.Graphics.OpenGL.FFI

#endif

-- | 'GlContext' is a 'Device' and 'Context' simultaneously.
data GlContext = GlContext
	{
	-- | Function to do stuff in device mode.
	  glContextInvoke :: !(forall a. IO a -> IO a)
	, glContextCaps :: {-# UNPACK #-} !GlCaps
	, glContextGlslConfig :: {-# UNPACK #-} !GlslConfig
	, glContextActualState :: {-# UNPACK #-} !GlContextState
	, glContextDesiredState :: {-# UNPACK #-} !GlContextState
	-- | Number of manually bound attributes.
	, glContextBoundAttributesCount :: {-# UNPACK #-} !(IORef Int)
	-- | Binary program cache.
	, glContextProgramCache :: !SomeBinaryCache
	}

type GlDevice = GlContext

data GlCaps = GlCaps
	{ glCapsArbUniformBufferObject :: !Bool
	, glCapsArbSamplerObjects :: !Bool
	, glCapsArbVertexAttribBinding :: !Bool
	, glCapsArbFramebufferObject :: !Bool
	, glCapsArbTextureStorage :: !Bool
	, glCapsArbInstancedArrays :: !Bool
	, glCapsClearBuffer :: !Bool
	, glCapsArbDebugOutput :: !Bool
	, glCapsArbGetProgramBinary :: !Bool
	} deriving Show

data GlContextState = GlContextState
	{ glContextStateFrameBuffer :: {-# UNPACK #-} !(IORef (FrameBufferId GlDevice))
	, glContextStateViewport :: {-# UNPACK #-} !(IORef Int4)
	, glContextStateScissor :: {-# UNPACK #-} !(IORef (Maybe Int4))
	, glContextStateVertexBuffers :: {-# UNPACK #-} !(VM.IOVector (VertexBufferId GlDevice))
	, glContextStateIndexBuffer :: {-# UNPACK #-} !(IORef (IndexBufferId GlDevice))
	, glContextStateUniformBuffers :: {-# UNPACK #-} !(VM.IOVector (UniformBufferId GlDevice))
	, glContextStateSamplers :: {-# UNPACK #-} !(VM.IOVector (TextureId GlDevice, SamplerStateId GlDevice))
	, glContextStateProgram :: {-# UNPACK #-} !(IORef (ProgramId GlDevice))
	, glContextStateDepthTestFunc :: {-# UNPACK #-} !(IORef DepthTestFunc)
	, glContextStateDepthWrite :: {-# UNPACK #-} !(IORef Bool)
	, glContextStateBlendState :: {-# UNPACK #-} !(IORef (BlendStateId GlDevice))
	}

data GlAttributeSlot = GlAttributeSlot
	{ glAttributeSlotElements :: !(V.Vector GlAttribute)
	, glAttributeSlotDivisor :: !GLuint
	}

data GlAttribute = GlAttribute
	{
	-- | Generic attribute index.
	  glAttributeIndex :: {-# UNPACK #-} !GLuint
	-- | Attribute size as specified to glVertexAttribPointer.
	, glAttributeSize :: {-# UNPACK #-} !GLint
	-- | Attribute type as specified to glVertexAttribPointer.
	, glAttributeType :: {-# UNPACK #-} !GLenum
	-- | Is attribute normalized.
	, glAttributeIsNormalized :: {-# UNPACK #-} !GLboolean
	-- | Is attribute integer.
	, glAttributeIsInteger :: {-# UNPACK #-} !GLboolean
	-- | Offset within vertex buffer.
	, glAttributeOffset :: {-# UNPACK #-} !Int
	}

data GlUniform = GlUniform
	{ glUniformLocation :: {-# UNPACK #-} !UniformLocation
	, glUniformOffset :: {-# UNPACK #-} !Int
	, glUniformSize :: {-# UNPACK #-} !GLint
	, glUniformType :: !ValueType
	}

instance Device GlContext where
	type DeferredContext GlContext = GlContext
	newtype TextureId GlContext = GlTextureId TextureName deriving Eq
	newtype SamplerStateId GlContext = GlSamplerStateId SamplerName deriving Eq
	data BlendStateId GlContext
		= GlBlendStateId {-# UNPACK #-} !BlendStateInfo
		| GlNullBlendStateId
		deriving Eq
	data RenderTargetId GlContext = GlRenderTargetId
		{ glRenderTargetName :: {-# UNPACK #-} !TextureName
		, glRenderTargetWidth :: {-# UNPACK #-} !Int
		, glRenderTargetHeight :: {-# UNPACK #-} !Int
		}
	data DepthStencilTargetId GlContext = GlDepthStencilTargetId
		{ glDepthStencilTargetName :: {-# UNPACK #-} !TextureName
		, glDepthStencilWidth :: {-# UNPACK #-} !Int
		, glDepthStencilHeight :: {-# UNPACK #-} !Int
		}
	data FrameBufferId GlContext = GlFrameBufferId
		{ glFrameBufferName :: {-# UNPACK #-} !FramebufferName
		, glFrameBufferWidth :: {-# UNPACK #-} !Int
		, glFrameBufferHeight :: {-# UNPACK #-} !Int
		} deriving Eq
	data VertexBufferId GlContext = GlVertexBufferId {-# UNPACK #-} !BufferName {-# UNPACK #-} !GLsizei deriving Eq
	data IndexBufferId GlContext = GlIndexBufferId {-# UNPACK #-} !BufferName {-# UNPACK #-} !GLenum
	data ProgramId GlContext = GlProgramId
		{ glProgramName :: {-# UNPACK #-} !ProgramName
		, glProgramVertexArrayName :: {-# UNPACK #-} !VertexArrayName
		, glProgramAttributeSlots :: {-# UNPACK #-} !(V.Vector GlAttributeSlot)
		-- | Number of generic vertex attributes used (i.e. used attributes are from 0 to this number minus 1).
		-- Only for manual binding, i.e. zero when vertex array is used.
		, glProgramAttributesCount :: {-# UNPACK #-} !Int
		-- | "Manual" uniforms by slot.
		, glProgramUniforms :: {-# UNPACK #-} !(V.Vector (V.Vector GlUniform))
		}
	data UniformBufferId GlContext
		-- | Real uniform buffer: buffer name, size.
		= GlUniformBufferId {-# UNPACK #-} !BufferName {-# UNPACK #-} !Int
		-- | Emulated uniform buffer: bytestring ref.
		| GlUniformMemoryBufferId {-# UNPACK #-} !(IORef B.ByteString)
		-- | Null uniform buffer.
		| GlNullUniformBufferId

	nullTexture = GlTextureId glNullTextureName
	nullSamplerState = GlSamplerStateId glNullSamplerName
	nullBlendState = GlNullBlendStateId
	nullDepthStencilTarget = GlDepthStencilTargetId
		{ glDepthStencilTargetName = glNullTextureName
		, glDepthStencilWidth = 0
		, glDepthStencilHeight = 0
		}
	nullIndexBuffer = GlIndexBufferId glNullBufferName GL_UNSIGNED_SHORT
	nullUniformBuffer = GlNullUniformBufferId

	createDeferredContext = undefined

	createStaticTexture GlContext
		{ glContextInvoke = invoke
		, glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} textureInfo@TextureInfo
		{ textureFormat = format
		} samplerStateInfo bytes = invoke $ describeException ("failed to create OpenGL static texture", textureInfo) $ do

		let
			width = fromIntegral $ textureWidth textureInfo
			height = fromIntegral $ textureHeight textureInfo
			depth = fromIntegral $ textureDepth textureInfo
			mips = fromIntegral $ textureMips textureInfo
			count = fromIntegral $ textureCount textureInfo

		-- arrays of 3D textures not supported
		when (depth > 0 && count > 0) $ throwIO $ DescribeFirstException "array of 3D textures is not supported"

		let TextureMetrics
			{ textureMipsMetrics = mipsMetrics
			} = calcTextureMetrics textureInfo

		textureName <- glAllocTextureName

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		glPixelStorei GL_UNPACK_ALIGNMENT 1
		glCheckErrors 0 "set unpack alignment"

		-- pixel size, use only for uncompressed formats
		let pixelSize = fromIntegral $ pixelSizeByteSize $ textureFormatPixelSize format

		-- get texture type
		let textureType
			| depth > 0 = Texture3D
			| height > 0 = if count > 0 then Texture2DArray else Texture2D
			| count > 0 = Texture1DArray
			| otherwise = Texture1D

		-- get target
		let glTarget = case textureType of
			Texture3D      -> GL_TEXTURE_3D
			Texture2DArray -> GL_TEXTURE_2D_ARRAY
			Texture2D      -> GL_TEXTURE_2D
			Texture1DArray -> GL_TEXTURE_1D_ARRAY
			Texture1D      -> GL_TEXTURE_1D

		-- bind texture
		glBindTexture glTarget textureName
		glCheckErrors 0 "bind texture"

		-- allocate texture storage, if we use it
		when useTextureStorage $ do
			case textureType of
				Texture3D      -> glTexStorage3D glTarget mips glInternalFormat width height depth
				Texture2DArray -> glTexStorage3D glTarget mips glInternalFormat width height count
				Texture2D      -> glTexStorage2D glTarget mips glInternalFormat width height
				Texture1DArray -> glTexStorage2D glTarget mips glInternalFormat width count
				Texture1D      -> glTexStorage1D glTarget mips glInternalFormat width
			glCheckErrors 0 "tex storage"

		-- gl[Compressed]TexImage* requires GLint, but glInternal format is GLenum (GLuint)
		let glInternalFormatS = fromIntegral glInternalFormat

		-- loop for mips
		forM_ (zip [0..(mips - 1)] mipsMetrics) $ \(mip, mipMetrics) -> do
			let
				mipWidth = fromIntegral $ textureMipWidth mipMetrics
				mipHeight = fromIntegral $ textureMipHeight mipMetrics
				mipDepth = fromIntegral $ textureMipDepth mipMetrics
				mipLinePitch = fromIntegral $ textureMipLinePitch mipMetrics
				mipSlicePitch = fromIntegral $ textureMipSlicePitch mipMetrics
				mipOffset = fromIntegral $ textureMipOffset mipMetrics
				mipSize = fromIntegral $ textureMipSize mipMetrics

			-- set unpack image height if needed
			when (textureType == Texture3D || textureType == Texture2DArray) $ do
				glPixelStorei GL_UNPACK_IMAGE_HEIGHT $ if compressed then 0 else mipSlicePitch `div` pixelSize
				glCheckErrors 0 "set unpack image height"

			-- set unpack row length if needed
			when (textureType == Texture3D || textureType == Texture2DArray || textureType == Texture2D || textureType == Texture1DArray) $ do
				glPixelStorei GL_UNPACK_ROW_LENGTH $ if compressed then 0 else mipLinePitch `div` pixelSize
				glCheckErrors 0 "set unpack row length"

			-- get mip data
			let mipBytes = B.take mipSize $ B.drop mipOffset bytes

			-- upload data
			if useTextureStorage then
				if compressed then
					case textureType of
						Texture3D      -> glCompressedTexSubImage3D_bs glTarget mip 0 0 0 mipWidth mipHeight mipDepth glInternalFormat mipBytes
						Texture2DArray -> glCompressedTexSubImage3D_bs glTarget mip 0 0 0 mipWidth mipHeight count    glInternalFormat mipBytes
						Texture2D      -> glCompressedTexSubImage2D_bs glTarget mip 0 0   mipWidth mipHeight          glInternalFormat mipBytes
						Texture1DArray -> glCompressedTexSubImage2D_bs glTarget mip 0 0   mipWidth count              glInternalFormat mipBytes
						Texture1D      -> glCompressedTexSubImage1D_bs glTarget mip 0     mipWidth                    glInternalFormat mipBytes
				else
					case textureType of
						Texture3D      -> glTexSubImage3D_bs glTarget mip 0 0 0 mipWidth mipHeight mipDepth glFormat glType mipBytes
						Texture2DArray -> glTexSubImage3D_bs glTarget mip 0 0 0 mipWidth mipHeight count    glFormat glType mipBytes
						Texture2D      -> glTexSubImage2D_bs glTarget mip 0 0   mipWidth mipHeight          glFormat glType mipBytes
						Texture1DArray -> glTexSubImage2D_bs glTarget mip 0 0   mipWidth count              glFormat glType mipBytes
						Texture1D      -> glTexSubImage1D_bs glTarget mip 0     mipWidth                    glFormat glType mipBytes
			else
				if compressed then
					case textureType of
						Texture3D      -> glCompressedTexImage3D_bs glTarget mip glInternalFormat mipWidth mipHeight mipDepth 0 mipBytes
						Texture2DArray -> glCompressedTexImage3D_bs glTarget mip glInternalFormat mipWidth mipHeight count    0 mipBytes
						Texture2D      -> glCompressedTexImage2D_bs glTarget mip glInternalFormat mipWidth mipHeight          0 mipBytes
						Texture1DArray -> glCompressedTexImage2D_bs glTarget mip glInternalFormat mipWidth count              0 mipBytes
						Texture1D      -> glCompressedTexImage1D_bs glTarget mip glInternalFormat mipWidth                    0 mipBytes
				else
					case textureType of
						Texture3D      -> glTexImage3D_bs glTarget mip glInternalFormatS mipWidth mipHeight mipDepth 0 glFormat glType mipBytes
						Texture2DArray -> glTexImage3D_bs glTarget mip glInternalFormatS mipWidth mipHeight count    0 glFormat glType mipBytes
						Texture2D      -> glTexImage2D_bs glTarget mip glInternalFormatS mipWidth mipHeight          0 glFormat glType mipBytes
						Texture1DArray -> glTexImage2D_bs glTarget mip glInternalFormatS mipWidth count              0 glFormat glType mipBytes
						Texture1D      -> glTexImage1D_bs glTarget mip glInternalFormatS mipWidth                    0 glFormat glType mipBytes
			glCheckErrors 0 "tex image"

		unless useTextureStorage $ do
			glTexParameteri glTarget GL_TEXTURE_BASE_LEVEL 0
			glTexParameteri glTarget GL_TEXTURE_MAX_LEVEL $ mips - 1
			glCheckErrors 0 "texture parameters"

		-- setup sampling
		glSetupTextureSampling glTarget samplerStateInfo

		return (GlTextureId textureName, invoke $ glDeleteTextureName textureName)

	createSamplerState GlContext
		{ glContextInvoke = invoke
		} samplerStateInfo@SamplerStateInfo
		{ samplerWrapU = wrapU
		, samplerWrapV = wrapV
		, samplerWrapW = wrapW
		, samplerMinLod = minLod
		, samplerMaxLod = maxLod
		, samplerBorderColor = borderColor
		} = invoke $ describeException "failed to create OpenGL sampler state" $ do

		samplerName <- glAllocSamplerName

		-- setup sampler

		-- min filter
		glSamplerParameteri samplerName GL_TEXTURE_MIN_FILTER $ fromIntegral $ glSamplerMinFilter samplerStateInfo
		glCheckErrors 0 "min filter"
		-- mag filter
		glSamplerParameteri samplerName GL_TEXTURE_MAG_FILTER $ fromIntegral $ glSamplerMagFilter samplerStateInfo
		glCheckErrors 0 "mag filter"

		-- wrap U
		glSamplerParameteri samplerName GL_TEXTURE_WRAP_S $ fromIntegral $ glSamplerWrap wrapU
		glCheckErrors 0 "wrap U"
		-- wrap V
		glSamplerParameteri samplerName GL_TEXTURE_WRAP_T $ fromIntegral $ glSamplerWrap wrapV
		glCheckErrors 0 "wrap V"
		-- wrap W
		glSamplerParameteri samplerName GL_TEXTURE_WRAP_R $ fromIntegral $ glSamplerWrap wrapW
		glCheckErrors 0 "wrap W"

		-- min LOD
		glSamplerParameterf samplerName GL_TEXTURE_MIN_LOD minLod
		glCheckErrors 0 "min LOD"
		-- max LOD
		glSamplerParameterf samplerName GL_TEXTURE_MAX_LOD maxLod
		glCheckErrors 0 "max LOD"

		-- border color
		glSamplerParameterfv_4 samplerName GL_TEXTURE_BORDER_COLOR borderColor
		glCheckErrors 0 "border color"

		return (GlSamplerStateId samplerName, invoke $ glDeleteSamplerName samplerName)

	createBlendState _context blendStateInfo = return (GlBlendStateId blendStateInfo, return ())

	createReadableRenderTarget GlContext
		{ glContextInvoke = invoke
		, glContextCaps = GlCaps
			{ glCapsArbTextureStorage = useTextureStorage
			}
		} width height format samplerStateInfo = invoke $ describeException "failed to create OpenGL readable render target" $ do

		textureName <- glAllocTextureName

		glBindTexture GL_TEXTURE_2D textureName
		glCheckErrors 0 "bind texture"

		let (compressed, glInternalFormat, glFormat, glType) = glFormatFromTextureFormat format

		when compressed $ throwIO $ DescribeFirstException "render target cannot use compressed format"

		if useTextureStorage then
			glTexStorage2D GL_TEXTURE_2D 1 glInternalFormat (fromIntegral width) (fromIntegral height)
		else
			glTexImage2D_null GL_TEXTURE_2D 0 (fromIntegral glInternalFormat) (fromIntegral width) (fromIntegral height) 0 glFormat glType
		glCheckErrors 0 "texture storage"

		-- setup sampling
		glSetupTextureSampling GL_TEXTURE_2D samplerStateInfo

		return ((GlRenderTargetId
			{ glRenderTargetName = textureName
			, glRenderTargetWidth = width
			, glRenderTargetHeight = height
			}, GlTextureId textureName), invoke $ glDeleteTextureName textureName)

	createDepthStencilTarget GlContext
		{ glContextInvoke = invoke
		} width height = invoke $ describeException "failed to create OpenGL depth stencil target" $ do

		textureName <- glAllocTextureName

		glBindTexture GL_TEXTURE_2D textureName
		glCheckErrors 0 "bind texture"
		glTexImage2D_null GL_TEXTURE_2D 0 (fromIntegral (GL_DEPTH_STENCIL :: GLenum)) (fromIntegral width) (fromIntegral height) 0 GL_DEPTH_STENCIL GL_UNSIGNED_INT_24_8
		glCheckErrors 0 "tex image"

		glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral (GL_NEAREST :: GLenum)
		glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral (GL_NEAREST :: GLenum)
		glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral (GL_CLAMP_TO_EDGE :: GLenum)
		glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral (GL_CLAMP_TO_EDGE :: GLenum)
		glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_R $ fromIntegral (GL_CLAMP_TO_EDGE :: GLenum)
		glCheckErrors 0 "texture parameters"

		return (GlDepthStencilTargetId
			{ glDepthStencilTargetName = textureName
			, glDepthStencilWidth = width
			, glDepthStencilHeight = height
			}, invoke $ glDeleteTextureName textureName)

	createReadableDepthStencilTarget context width height = do
		(depthStencilTarget@GlDepthStencilTargetId
			{ glDepthStencilTargetName = bufferName
			}, destroy) <- createDepthStencilTarget context width height
		return ((depthStencilTarget, GlTextureId bufferName), destroy)

	createFrameBuffer GlContext
		{ glContextInvoke = invoke
		, glContextActualState = GlContextState
			{ glContextStateFrameBuffer = actualFrameBufferRef
			}
		} renderTargets GlDepthStencilTargetId
		{ glDepthStencilTargetName = depthStencilName
		, glDepthStencilWidth = depthStencilWidth
		, glDepthStencilHeight = depthStencilHeight
		} = invoke $ describeException "failed to create OpenGL framebuffer" $ do

		framebufferName <- glAllocFramebufferName

		-- bind framebuffer
		glBindFramebuffer GL_FRAMEBUFFER framebufferName
		glCheckErrors 0 "bind framebuffer"

		-- bind render targets
		forM_ (zip [0..] renderTargets) $ \(i, GlRenderTargetId
			{ glRenderTargetName = renderTargetName
			}) -> do
			glFramebufferTexture2D GL_FRAMEBUFFER (GL_COLOR_ATTACHMENT0 + i) GL_TEXTURE_2D renderTargetName 0
			glCheckErrors 0 "bind framebuffer color buffer"

		-- bind depth-stencil target
		glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_TEXTURE_2D depthStencilName 0
		glCheckErrors 0 "bind framebuffer depth-stencil buffer"

		-- get width and height of framebuffer (and check that they're equal)
		let foldSize (n, w, h) (rw, rh) = do
			if n /= glNullTextureName then do
				when ((rw > 0 && w /= rw) || (rh > 0 && h /= rh)) $ throwIO $ DescribeFirstException "sizes are not equal"
				return (rw .|. w, rh .|. h)
			else return (rw, rh)
		(frameBufferWidth, frameBufferHeight) <- foldrM foldSize (depthStencilWidth, depthStencilHeight) $ map (\GlRenderTargetId
			{ glRenderTargetName = n
			, glRenderTargetWidth = w
			, glRenderTargetHeight = h
			} -> (n, w, h)) renderTargets

		let frameBufferId = GlFrameBufferId
			{ glFrameBufferName = framebufferName
			, glFrameBufferWidth = frameBufferWidth
			, glFrameBufferHeight = frameBufferHeight
			}

		writeIORef actualFrameBufferRef frameBufferId

		return (frameBufferId, invoke $ glDeleteFramebufferName framebufferName)

	createStaticVertexBuffer GlContext
		{ glContextInvoke = invoke
		} bytes stride = invoke $ describeException "failed to create OpenGL static vertex buffer" $ do
		bufferName <- glAllocBufferName
		glBindBuffer GL_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		glBufferData_bs GL_ARRAY_BUFFER bytes GL_STATIC_DRAW
		glCheckErrors 0 "buffer data"

		return (GlVertexBufferId bufferName (fromIntegral stride), invoke $ glDeleteBufferName bufferName)

	createDynamicVertexBuffer GlContext
		{ glContextInvoke = invoke
		} size stride = invoke $ describeException "failed to create OpenGL dynamic vertex buffer" $ do
		bufferName <- glAllocBufferName
		glBindBuffer GL_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		glBufferData_null GL_ARRAY_BUFFER (fromIntegral size) GL_DYNAMIC_DRAW
		glCheckErrors 0 "buffer data"

		return (GlVertexBufferId bufferName (fromIntegral stride), invoke $ glDeleteBufferName bufferName)

	createStaticIndexBuffer GlContext
		{ glContextInvoke = invoke
		, glContextActualState = GlContextState
			{ glContextStateIndexBuffer = actualIndexBufferRef
			}
		} bytes is32Bit = invoke $ describeException "failed to create OpenGL static index buffer" $ do
		bufferName <- glAllocBufferName
		glBindBuffer GL_ELEMENT_ARRAY_BUFFER bufferName
		glCheckErrors 0 "bind buffer"
		glBufferData_bs GL_ELEMENT_ARRAY_BUFFER bytes GL_STATIC_DRAW
		glCheckErrors 0 "buffer data"

		let indexBufferId = GlIndexBufferId bufferName (if is32Bit then GL_UNSIGNED_INT else GL_UNSIGNED_SHORT)

		-- element array buffer is a part of VAO; unbind it in order to re-bind with actual VAO during drawing
		glBindBuffer GL_ELEMENT_ARRAY_BUFFER glNullBufferName
		glCheckErrors 0 "unbind buffer"

		writeIORef actualIndexBufferRef $ GlIndexBufferId glNullBufferName GL_UNSIGNED_SHORT

		return (indexBufferId, invoke $ glDeleteBufferName bufferName)

	createProgram GlContext
		{ glContextInvoke = invoke
		, glContextCaps = GlCaps
			{ glCapsArbVertexAttribBinding = capArbVertexAttribBinding
			, glCapsArbGetProgramBinary = capArbGetProgramBinary
			}
		, glContextGlslConfig = glslConfig
		, glContextActualState = GlContextState
			{ glContextStateProgram = actualProgramRef
			}
		, glContextProgramCache = SomeBinaryCache programCache
		} program = invoke $ describeException "failed to create OpenGL program" $ withSpecialBook $ \bk -> do

		-- reset current program in order to correctly rebind it later for drawing
		writeIORef actualProgramRef glNullProgram

		-- generate GLSL
		glslProgram@GlslProgram
			{ glslProgramAttributes = attributes
			, glslProgramUniformBlocks = uniformBlocks
			, glslProgramUniforms = uniforms
			, glslProgramSamplers = samplers
			, glslProgramFragmentTargets = fragmentTargets
			, glslProgramShaders = shaders
			} <- glslGenerateProgram glslConfig <$> runProgram program

		-- create program
		programName <- book bk $ do
			p <- glCreateProgram
			return (p, glDeleteProgram p)

		-- if binary programs are supported, try to use binary cache
		let cacheKey = S.encode glslProgram
		binaryLoaded <- if capArbGetProgramBinary then do
			encodedBinaryProgram <- getCachedBinary programCache cacheKey
			case S.decode encodedBinaryProgram of
				Right binaryProgram -> do
					let (bytes, format) = binaryProgram :: BinaryProgram
					-- load binary program
					B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> glProgramBinary programName (fromIntegral format) bytesPtr (fromIntegral bytesLen)
					glCheckErrors 1 "load binary program"
					-- check link status
					status <- alloca $ \statusPtr -> do
						glGetProgramiv programName GL_LINK_STATUS statusPtr
						glCheckErrors 0 "get program link status"
						peek statusPtr
					return $ status == 1
				Left _err -> return False
		else return False

		-- if binary program is not loaded, compile and link from source
		unless binaryLoaded $ do
			-- create and attach shaders
			forM_ shaders $ \(shaderStage, shaderSource) -> describeException ("failed to create OpenGL shader", shaderStage) $ do

				-- create shader
				shaderName <- glCreateShader $ case shaderStage of
					GlslVertexStage -> GL_VERTEX_SHADER
					GlslFragmentStage -> GL_FRAGMENT_SHADER
				glCheckErrors 0 "create shader"
				_ <- book bk $ return (shaderName, glDeleteShader shaderName)

				-- set shader source
				glShaderSource_s shaderName shaderSource
				glCheckErrors 0 "set shader source"

				-- compile shader
				glCompileShader shaderName
				glCheckErrors 0 "compile shader"
				-- check compilation status
				status <- alloca $ \statusPtr -> do
					glGetShaderiv shaderName GL_COMPILE_STATUS statusPtr
					glCheckErrors 0 "get shader compile status"
					peek statusPtr
				if status == 1 then do
					-- compilation succeeded, attach shader to program
					glAttachShader programName shaderName
					glCheckErrors 0 "attach shader"
				else do
					shaderLog <- glGetShaderInfoLog_s shaderName
					glCheckErrors 0 "get shader log"
					throwIO $ DescribeFirstException ("failed to compile shader", shaderLog)

			-- bind attributes
			forM_ (zip attributes [0..]) $ \(GlslAttribute
				{ glslAttributeName = attributeName
				}, i) -> do
				glBindAttribLocation_s programName i attributeName
				glCheckErrors 0 "bind attribute location"

			-- bind fragment targets
			forM_ fragmentTargets $ \fragmentTarget -> case fragmentTarget of
				GlslFragmentTarget
					{ glslFragmentTargetName = targetName
					, glslFragmentTargetIndex = targetIndex
					} -> do
					B.useAsCString (T.encodeUtf8 targetName) $ glBindFragDataLocation programName (fromIntegral targetIndex)
					glCheckErrors 0 "bind frag data location"
				GlslDualFragmentTarget
					{ glslFragmentTargetName0 = targetName0
					, glslFragmentTargetName1 = targetName1
					} -> do
					B.useAsCString (T.encodeUtf8 targetName0) $ glBindFragDataLocationIndexed programName 0 0
					B.useAsCString (T.encodeUtf8 targetName1) $ glBindFragDataLocationIndexed programName 0 1
					glCheckErrors 0 "bind frag data location indexed"

			-- link program
			glLinkProgram programName
			glCheckErrors 0 "link program"
			-- check link status
			status <- alloca $ \statusPtr -> do
				glGetProgramiv programName GL_LINK_STATUS statusPtr
				glCheckErrors 0 "get program link status"
				peek statusPtr
			if status == 1 then do
				-- save binary program into cache
				when capArbGetProgramBinary $ do
					len <- alloca $ \lenPtr -> do
						glGetProgramiv programName GL_PROGRAM_BINARY_LENGTH lenPtr
						glCheckErrors 0 "get binary program length"
						fromIntegral <$> peek lenPtr
					when (len > 0) $ do
						bytesPtr <- mallocBytes len
						binaryProgram <- alloca $ \formatPtr -> do
							glGetProgramBinary programName (fromIntegral len) nullPtr formatPtr bytesPtr
							bytes <- B.unsafePackMallocCStringLen (bytesPtr, len)
							glCheckErrors 0 "get program binary"
							format <- peek formatPtr
							return (bytes, fromIntegral format)
						setCachedBinary programCache cacheKey $ S.encode (binaryProgram :: BinaryProgram)
			else do
				programLog <- glGetProgramInfoLog_s programName
				glCheckErrors 0 "get program log"
				throwIO $ DescribeFirstException ("failed to link program", programLog)

		-- set as current
		glUseProgram programName
		glCheckErrors 0 "set program"

		-- bind uniform blocks
		forM_ uniformBlocks $ \GlslUniformBlock
			{ glslUniformBlockName = uniformBlockName
			, glslUniformBlockSlot = slot
			} -> do
			index <- glGetUniformBlockIndex_s programName uniformBlockName
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
			location <- glGetUniformLocation_s programName samplerName
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
			let slotsCount = if null uniformsBySlot then 0 else 1 + maximum (map (getSlot . head) uniformsBySlot)
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
					location <- glGetUniformLocation_s programName uniformName
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
				vaName <- glAllocVertexArrayName
				return (vaName, glDeleteVertexArrayName vaName)

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
				if isInteger > 0 then glVertexAttribIFormat i size t (fromIntegral offset)
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
			glBindVertexArray glNullVertexArrayName
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
			let slotsCount = if null attributesBySlot then 0 else 1 + maximum (map (getSlot . fst . head) attributesBySlot)
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
							, glAttributeOffset = offset
							}
					VM.write slots slot GlAttributeSlot
						{ glAttributeSlotElements = elements
						, glAttributeSlotDivisor = fromIntegral $ attributeDivisor $ glslAttributeInfo $ fst a
						}
				return slots

			return (glNullVertexArrayName, attributeSlots, length attributes)

		return GlProgramId
			{ glProgramName = programName
			, glProgramVertexArrayName = vertexArrayName
			, glProgramAttributeSlots = attributeSlots
			, glProgramAttributesCount = attributesCount
			, glProgramUniforms = uniformBindings
			}

	createUniformBuffer GlContext
		{ glContextInvoke = invoke
		, glContextCaps = GlCaps
			{ glCapsArbUniformBufferObject = useUniformBufferObject
			}
		} size = invoke $ describeException "failed to create OpenGL uniform buffer" $ do

		if useUniformBufferObject then do
			bufferName <- glAllocBufferName
			glBindBuffer GL_UNIFORM_BUFFER bufferName
			glCheckErrors 0 "bind buffer"
			glBufferData_null GL_UNIFORM_BUFFER (fromIntegral size) GL_DYNAMIC_DRAW
			glCheckErrors 0 "buffer data"
			
			return (GlUniformBufferId bufferName size, invoke $ glDeleteBufferName bufferName)
		else do
			bufferRef <- newIORef B.empty
			return (GlUniformMemoryBufferId bufferRef, return ())

instance Context GlContext GlContext where
	contextClearColor context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsClearBuffer = useClearBuffer
			}
		} targetIndex color@(Float4 r g b a) = do
		glUpdateFrameBufferViewportScissor context
		if useClearBuffer then
			glClearBufferfv_4 GL_COLOR (fromIntegral targetIndex) color
		else do
			glClearColor r g b a
			glClear GL_COLOR_BUFFER_BIT
		glCheckErrors 1 "clear color"

	contextClearDepth context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsClearBuffer = useClearBuffer
			}
		} depth = do
		glUpdateFrameBufferViewportScissor context
		glEnableDepthWriteForClearing context
		if useClearBuffer then
			glClearBufferfv_1 GL_DEPTH 0 depth
		else do
			glClearDepth (float2Double depth)
			glClear GL_DEPTH_BUFFER_BIT
		glCheckErrors 1 "clear depth"

	contextClearStencil context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsClearBuffer = useClearBuffer
			}
		} stencil = do
		glUpdateFrameBufferViewportScissor context
		if useClearBuffer then
			glClearBufferiv_1 GL_STENCIL 0 (fromIntegral stencil)
		else do
			glClearStencil (fromIntegral stencil)
			glClear GL_STENCIL_BUFFER_BIT
		glCheckErrors 1 "clear stencil"

	contextClearDepthStencil context@GlContext
		{ glContextCaps = GlCaps
			{ glCapsClearBuffer = useClearBuffer
			}
		} depth stencil = do
		glUpdateFrameBufferViewportScissor context
		glEnableDepthWriteForClearing context
		if useClearBuffer then
			glClearBufferfi GL_DEPTH_STENCIL 0 depth (fromIntegral stencil)
		else do
			glClearDepth (float2Double depth)
			glClearStencil (fromIntegral stencil)
			glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT
		glCheckErrors 1 "clear depth stencil"

	contextUploadUniformBuffer _context uniformBuffer bytes = case uniformBuffer of
		GlUniformBufferId bufferName _bufferSize -> do
			glBindBuffer GL_UNIFORM_BUFFER bufferName
			glBufferData_bs GL_UNIFORM_BUFFER bytes GL_DYNAMIC_DRAW
			glCheckErrors 1 "upload uniform buffer"
		GlUniformMemoryBufferId bufferRef -> do
			-- remember buffer data
			writeIORef bufferRef bytes
		GlNullUniformBufferId -> throwIO $ DescribeFirstException "uploading to null uniform buffer"

	contextUploadVertexBuffer _context (GlVertexBufferId bufferName _stride) bytes = do
		glBindBuffer GL_ARRAY_BUFFER bufferName
		glBufferData_bs GL_ARRAY_BUFFER bytes GL_DYNAMIC_DRAW
		glCheckErrors 1 "upload vertex buffer"

	contextDraw context@GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateIndexBuffer = indexBufferRef
			}
		} instancesCount indicesCount = do
		glUpdateContext context
		GlIndexBufferId indexBufferName indicesType <- readIORef indexBufferRef
		if indexBufferName /= glNullBufferName then do
			if instancesCount > 1 then
				glDrawElementsInstanced GL_TRIANGLES (fromIntegral indicesCount) indicesType (glIntToOffset 0) (fromIntegral instancesCount)
			else
				glDrawElements GL_TRIANGLES (fromIntegral indicesCount) indicesType (glIntToOffset 0)
		else do
			if instancesCount > 1 then
				glDrawArraysInstanced GL_TRIANGLES 0 (fromIntegral indicesCount) (fromIntegral instancesCount)
			else
				glDrawArrays GL_TRIANGLES 0 (fromIntegral indicesCount)
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
		} viewport scope = do
		oldViewport <- readIORef viewportRef
		writeIORef viewportRef viewport
		r <- scope
		writeIORef viewportRef oldViewport
		return r

	contextGetViewport GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateViewport = viewportRef
			}
		} = readIORef viewportRef

	contextSetScissor GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateScissor = scissorRef
			}
		} scissor scope = do
		oldScissor <- readIORef scissorRef
		writeIORef scissorRef scissor
		r <- scope
		writeIORef scissorRef oldScissor
		return r

	contextGetScissor GlContext
		{ glContextDesiredState = GlContextState
			{ glContextStateScissor = scissorRef
			}
		} = readIORef scissorRef

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

glNullProgram :: ProgramId GlDevice
glNullProgram = GlProgramId
	{ glProgramName = glNullProgramName
	, glProgramVertexArrayName = glNullVertexArrayName
	, glProgramAttributeSlots = V.empty
	, glProgramAttributesCount = 0
	, glProgramUniforms = V.empty
	}

newGlContext :: (forall a. IO a -> IO a) -> GlCaps -> GlslConfig -> SomeBinaryCache -> IO GlContext
newGlContext invoke caps glslConfig programCache = do
	actualState <- glCreateContextState
	desiredState <- glCreateContextState
	boundAttributesCount <- newIORef 0
	return GlContext
		{ glContextInvoke = invoke
		, glContextCaps = caps
		, glContextGlslConfig = glslConfig
		, glContextActualState = actualState
		, glContextDesiredState = desiredState
		, glContextBoundAttributesCount = boundAttributesCount
		, glContextProgramCache = programCache
		}

glCreateContextState :: IO GlContextState
glCreateContextState = do
	frameBuffer <- newIORef GlFrameBufferId
		{ glFrameBufferName = glNullFramebufferName
		, glFrameBufferWidth = 0
		, glFrameBufferHeight = 0
		}
	viewport <- newIORef $ Vec4 0 0 0 0
	scissor <- newIORef Nothing
	vertexBuffers <- VM.replicate 8 $ GlVertexBufferId glNullBufferName 0
	indexBuffer <- newIORef $ GlIndexBufferId glNullBufferName GL_UNSIGNED_SHORT
	uniformBuffers <- VM.replicate 8 GlNullUniformBufferId
	samplers <- VM.replicate 8 (GlTextureId glNullTextureName, GlSamplerStateId glNullSamplerName)
	program <- newIORef glNullProgram
	depthTestFunc <- newIORef DepthTestFuncLess
	depthWrite <- newIORef True
	blendState <- newIORef nullBlendState
	return GlContextState
		{ glContextStateFrameBuffer = frameBuffer
		, glContextStateViewport = viewport
		, glContextStateScissor = scissor
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
	, glContextStateScissor = scissorRef
	, glContextStateVertexBuffers = vertexBuffersVector
	, glContextStateIndexBuffer = indexBufferRef
	, glContextStateUniformBuffers = uniformBuffersVector
	, glContextStateSamplers = samplersVector
	, glContextStateProgram = programRef
	, glContextStateDepthTestFunc = depthTestFuncRef
	, glContextStateDepthWrite = depthWriteRef
	, glContextStateBlendState = blendStateRef
	} = do
	writeIORef frameBufferRef GlFrameBufferId
		{ glFrameBufferName = glNullFramebufferName
		, glFrameBufferWidth = 0
		, glFrameBufferHeight = 0
		}
	writeIORef viewportRef $ Vec4 0 0 0 0
	writeIORef scissorRef Nothing
	VM.set vertexBuffersVector $ GlVertexBufferId glNullBufferName 0
	writeIORef indexBufferRef $ GlIndexBufferId glNullBufferName GL_UNSIGNED_SHORT
	VM.set uniformBuffersVector GlNullUniformBufferId
	VM.set samplersVector (GlTextureId glNullTextureName, GlSamplerStateId glNullSamplerName)
	writeIORef programRef glNullProgram
	writeIORef depthTestFuncRef DepthTestFuncLess
	writeIORef depthWriteRef True
	writeIORef blendStateRef nullBlendState

-- | Returns (compressed, internal format, format, type)
glFormatFromTextureFormat :: TextureFormat -> (Bool, GLenum, GLenum, GLenum)
glFormatFromTextureFormat format = case format of
	UncompressedTextureFormat
		{ textureFormatComponents = components
		, textureFormatValueType = vt
		, textureFormatPixelSize = pixelSize
		, textureFormatColorSpace = colorSpace
		} -> case (components, vt, pixelSize, colorSpace) of
		(PixelR, PixelUint, Pixel8bit, LinearColorSpace) -> (False, GL_R8, GL_RED, GL_UNSIGNED_BYTE)
		(PixelR, PixelUint, Pixel16bit, LinearColorSpace) -> (False, GL_R16, GL_RED, GL_UNSIGNED_SHORT)
		(PixelR, PixelFloat, Pixel16bit, LinearColorSpace) -> (False, GL_R16F, GL_RED, GL_FLOAT)
		(PixelR, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, GL_R32F, GL_RED, GL_FLOAT)
		(PixelRG, PixelUint, Pixel16bit, LinearColorSpace) -> (False, GL_RG8, GL_RG, GL_UNSIGNED_BYTE)
		(PixelRG, PixelUint, Pixel32bit, LinearColorSpace) -> (False, GL_RG16, GL_RG, GL_UNSIGNED_SHORT)
		(PixelRG, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, GL_RG16F, GL_RG, GL_FLOAT)
		(PixelRG, PixelFloat, Pixel64bit, LinearColorSpace) -> (False, GL_RG32F, GL_RG, GL_FLOAT)
		(PixelRGB, PixelFloat, Pixel32bit, LinearColorSpace) -> (False, GL_R11F_G11F_B10F, GL_RGB, GL_FLOAT)
		(PixelRGB, PixelFloat, Pixel96bit, LinearColorSpace) -> (False, GL_RGB32F, GL_RGB, GL_FLOAT)
		(PixelRGBA, PixelUint, Pixel32bit, LinearColorSpace) -> (False, GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE)
		(PixelRGBA, PixelUint, Pixel32bit, StandardColorSpace) -> (False, GL_SRGB8_ALPHA8, GL_RGBA, GL_UNSIGNED_BYTE)
		(PixelRGBA, PixelUint, Pixel64bit, LinearColorSpace) -> (False, GL_RGBA16, GL_RGBA, GL_UNSIGNED_SHORT)
		(PixelRGBA, PixelFloat, Pixel64bit, LinearColorSpace) -> (False, GL_RGBA16F, GL_RGBA, GL_FLOAT)
		(PixelRGBA, PixelFloat, Pixel128bit, LinearColorSpace) -> (False, GL_RGBA32F, GL_RGBA, GL_FLOAT)
		_ -> error $ show ("uncompressed texture format unsupported by OpenGL", format)
	CompressedTextureFormat
		{ textureFormatCompression = compression
		, textureFormatColorSpace = colorSpace
		} -> case (compression, colorSpace) of
		(TextureCompressionBC1, LinearColorSpace) -> (True, GL_COMPRESSED_RGB_S3TC_DXT1_EXT, GL_COMPRESSED_RGB, GL_UNSIGNED_BYTE)
		(TextureCompressionBC1, StandardColorSpace) -> (True, GL_COMPRESSED_SRGB_S3TC_DXT1_EXT, GL_COMPRESSED_RGB, GL_UNSIGNED_BYTE)
		(TextureCompressionBC1Alpha, LinearColorSpace) -> (True, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC1Alpha, StandardColorSpace) -> (True, GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC2, LinearColorSpace) -> (True, GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC2, StandardColorSpace) -> (True, GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC3, LinearColorSpace) -> (True, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC3, StandardColorSpace) -> (True, GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT, GL_COMPRESSED_RGBA, GL_UNSIGNED_BYTE)
		(TextureCompressionBC4, LinearColorSpace) -> (True, GL_COMPRESSED_RED_RGTC1, GL_COMPRESSED_RED, GL_UNSIGNED_BYTE)
		(TextureCompressionBC4Signed, LinearColorSpace) -> (True, GL_COMPRESSED_SIGNED_RED_RGTC1, GL_COMPRESSED_RED, GL_UNSIGNED_BYTE)
		(TextureCompressionBC5, LinearColorSpace) -> (True, GL_COMPRESSED_RG_RGTC2, GL_COMPRESSED_RG, GL_UNSIGNED_BYTE)
		(TextureCompressionBC5Signed, LinearColorSpace) -> (True, GL_COMPRESSED_SIGNED_RG_RGTC2, GL_COMPRESSED_RG, GL_UNSIGNED_BYTE)
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
		{ glContextStateVertexBuffers = actualVertexBuffersVector
		, glContextStateIndexBuffer = actualIndexBufferRef
		, glContextStateUniformBuffers = actualUniformBuffersVector
		, glContextStateSamplers = actualSamplersVector
		, glContextStateProgram = actualProgramRef
		, glContextStateDepthTestFunc = actualDepthTestFuncRef
		, glContextStateDepthWrite = actualDepthWriteRef
		, glContextStateBlendState = actualBlendStateRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateVertexBuffers = desiredVertexBuffersVector
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
	-- framebuffer, viewport and scissor
	glUpdateFrameBufferViewportScissor context

	-- samplers
	vectorSetup actualSamplersVector desiredSamplersVector $ \i (GlTextureId textureName, GlSamplerStateId samplerName) -> do
		glActiveTexture $ GL_TEXTURE0 + fromIntegral i
		glBindTexture GL_TEXTURE_2D textureName
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
		when (vertexArrayName /= glNullVertexArrayName) $ do
			-- bind vertex array
			glBindVertexArray vertexArrayName
			glCheckErrors 0 "bind vertex array"
			-- reset current index buffer binding in order to refresh (as element array buffer is part of VAO state)
			writeIORef actualIndexBufferRef $ GlIndexBufferId glUndefinedBufferName 0

	-- uniform buffers
	uniformBindings <- glProgramUniforms <$> readIORef desiredProgramRef
	let uniformBindingsCount = V.length uniformBindings
	forM_ [0..(VM.length actualUniformBuffersVector - 1)] $ \i -> do
		actualUniformBuffer <- VM.read actualUniformBuffersVector i
		desiredUniformBuffer <- VM.read desiredUniformBuffersVector i
		let updateActual = VM.write actualUniformBuffersVector i desiredUniformBuffer
		let bindBuffer bufferName = do
			glBindBufferBase GL_UNIFORM_BUFFER (fromIntegral i) bufferName
			glCheckErrors 0 "bind uniform buffer"
		-- we don't support mix of real uniform buffers and memory buffers
		case desiredUniformBuffer of
			GlUniformBufferId bufferName _bufferSize -> do
				-- skip update if buffer is the same
				let needUpdate = case actualUniformBuffer of
					GlUniformBufferId prevBufferName _prevBufferSize -> bufferName /= prevBufferName
					_ -> True
				when needUpdate $ do
					bindBuffer bufferName
					updateActual
			GlUniformMemoryBufferId bytesRef -> do
				-- update uniforms every time
				-- we cannot check if nothing has changed currently, as we don't keep
				-- actual bound immutable data in the context state, but only a ref to the buffer data
				bytes <- readIORef bytesRef
				-- get uniforms for this slot
				let uniforms = if i < uniformBindingsCount then uniformBindings V.! i else V.empty
				-- setup manually
				B.unsafeUseAsCString bytes $ \bytesPtr -> forM_ uniforms $ \GlUniform
					{ glUniformLocation = location
					, glUniformOffset = offset
					, glUniformSize = s
					, glUniformType = t
					} -> do
					let ptr = plusPtr bytesPtr offset
					let size = if s > 0 then s else 1
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
				updateActual
			GlNullUniformBufferId -> case actualUniformBuffer of
				GlUniformBufferId {} -> do
					bindBuffer glNullBufferName
					updateActual
				GlUniformMemoryBufferId {} -> do
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
			glBindBuffer GL_ARRAY_BUFFER bufferName
			glCheckErrors 0 "bind array buffer"

			-- bind attributes
			attributeSlots <- glProgramAttributeSlots <$> readIORef desiredProgramRef
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

					if isInteger > 0 then
						glVertexAttribIPointer i size t stride (glIntToOffset offset)
					else
						glVertexAttribPointer i size t isNormalized stride (glIntToOffset offset)
					glCheckErrors 0 "vertex attrib pointer"

					when capArbInstancedArrays $ do
						glVertexAttribDivisor i divisor
						glCheckErrors 0 "vertex attrib divisor"

	-- disable unused attributes
	newBoundAttributesCount <- glProgramAttributesCount <$> readIORef desiredProgramRef
	oldBoundAttributesCount <- readIORef boundAttributesCountRef
	forM_ [newBoundAttributesCount .. (oldBoundAttributesCount - 1)] $ \i -> do
		glDisableVertexAttribArray (fromIntegral i)
		glCheckErrors 0 "disable unused vertex attrib array"
	writeIORef boundAttributesCountRef newBoundAttributesCount

	-- index buffer
	refSetup_ actualIndexBufferRef desiredIndexBufferRef $ \(GlIndexBufferId indexBufferName _indicesType) -> do
		glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBufferName
		glCheckErrors 0 "bind index buffer"

	-- depth test func & depth write
	do
		actualDepthTestFunc <- readIORef actualDepthTestFuncRef
		desiredDepthTestFunc <- readIORef desiredDepthTestFuncRef
		actualDepthWrite <- readIORef actualDepthWriteRef
		desiredDepthWrite <- readIORef desiredDepthWriteRef
		-- enable or disable depth test
		when (actualDepthTestFunc /= desiredDepthTestFunc || actualDepthWrite /= desiredDepthWrite) $ do
			(if desiredDepthTestFunc /= DepthTestFuncAlways || desiredDepthWrite then glEnable else glDisable) GL_DEPTH_TEST
			glCheckErrors 0 "enable/disable depth test"
		-- depth test func
		when (actualDepthTestFunc /= desiredDepthTestFunc) $ do
			let func = case desiredDepthTestFunc of
				DepthTestFuncNever -> GL_NEVER
				DepthTestFuncLess -> GL_LESS
				DepthTestFuncLessOrEqual -> GL_LEQUAL
				DepthTestFuncEqual -> GL_EQUAL
				DepthTestFuncNonEqual -> GL_NOTEQUAL
				DepthTestFuncGreaterOrEqual -> GL_GEQUAL
				DepthTestFuncGreater -> GL_GREATER
				DepthTestFuncAlways -> GL_ALWAYS
			glDepthFunc func
			glCheckErrors 0 "set depth test func"
			writeIORef actualDepthTestFuncRef desiredDepthTestFunc
		-- depth write
		when (actualDepthWrite /= desiredDepthWrite) $ do
			glDepthMask (if desiredDepthWrite then 1 else 0)
			glCheckErrors 0 "set depth write"
			writeIORef actualDepthWriteRef desiredDepthWrite

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
			glEnable GL_BLEND
			glCheckErrors 0 "enable blending"

			-- convert-to-OpenGL functions
			let convertColor c = case c of
				ColorSourceZero -> GL_ZERO
				ColorSourceOne -> GL_ONE
				ColorSourceSrc -> GL_SRC_COLOR
				ColorSourceInvSrc -> GL_ONE_MINUS_SRC_COLOR
				ColorSourceSrcAlpha -> GL_SRC_ALPHA
				ColorSourceInvSrcAlpha -> GL_ONE_MINUS_SRC_ALPHA
				ColorSourceDest -> GL_DST_COLOR
				ColorSourceInvDest -> GL_ONE_MINUS_DST_COLOR
				ColorSourceDestAlpha -> GL_DST_ALPHA
				ColorSourceInvDestAlpha -> GL_ONE_MINUS_DST_ALPHA
				ColorSourceSecondSrc -> GL_SRC1_COLOR
				ColorSourceInvSecondSrc -> GL_ONE_MINUS_SRC1_COLOR
				ColorSourceSecondSrcAlpha -> GL_SRC1_ALPHA
				ColorSourceInvSecondSrcAlpha -> GL_ONE_MINUS_SRC1_ALPHA
			let convertAlpha a = case a of
				AlphaSourceZero -> GL_ZERO
				AlphaSourceOne -> GL_ONE
				AlphaSourceSrc -> GL_SRC_ALPHA
				AlphaSourceInvSrc -> GL_ONE_MINUS_SRC_ALPHA
				AlphaSourceDest -> GL_DST_ALPHA
				AlphaSourceInvDest -> GL_ONE_MINUS_DST_ALPHA
				AlphaSourceSecondSrc -> GL_SRC1_ALPHA
				AlphaSourceInvSecondSrc -> GL_ONE_MINUS_SRC1_ALPHA
			let convertOperation o = case o of
				BlendOperationAdd -> GL_FUNC_ADD
				BlendOperationSubtractAB -> GL_FUNC_SUBTRACT
				BlendOperationSubtractBA -> GL_FUNC_REVERSE_SUBTRACT
				BlendOperationMin -> GL_MIN
				BlendOperationMax -> GL_MAX

			-- set blend funcs
			glBlendFuncSeparate (convertColor sourceColor) (convertColor destColor) (convertAlpha sourceAlpha) (convertAlpha destAlpha)
			glCheckErrors 0 "set blend func"
			-- set blend operations
			glBlendEquationSeparate (convertOperation colorOperation) (convertOperation alphaOperation)
			glCheckErrors 0 "set blend equation"

		GlNullBlendStateId -> do
			-- disable blending
			glDisable GL_BLEND
			glCheckErrors 0 "disable blending"

glUpdateFrameBufferViewportScissor :: GlContext -> IO ()
glUpdateFrameBufferViewportScissor GlContext
	{ glContextActualState = GlContextState
		{ glContextStateFrameBuffer = actualFrameBufferRef
		, glContextStateViewport = actualViewportRef
		, glContextStateScissor = actualScissorRef
		}
	, glContextDesiredState = GlContextState
		{ glContextStateFrameBuffer = desiredFrameBufferRef
		, glContextStateViewport = desiredViewportRef
		, glContextStateScissor = desiredScissorRef
		}
	} = do
	actualFrameBufferId <- readIORef actualFrameBufferRef
	desiredFrameBufferId@GlFrameBufferId
		{ glFrameBufferWidth = frameBufferWidth
		, glFrameBufferHeight = frameBufferHeight
		} <- readIORef desiredFrameBufferRef
	actualViewport <- readIORef actualViewportRef
	desiredViewport <- readIORef desiredViewportRef
	actualScissor <- readIORef actualScissorRef
	desiredScissor <- readIORef desiredScissorRef

	let clipX = max 0 . min frameBufferWidth
	let clipY = max 0 . min frameBufferHeight

	-- framebuffer
	when (actualFrameBufferId /= desiredFrameBufferId) $ do
		glBindFramebuffer GL_FRAMEBUFFER $ glFrameBufferName desiredFrameBufferId
		glCheckErrors 0 "bind framebuffer"
		writeIORef actualFrameBufferRef desiredFrameBufferId

	-- viewport
	when (actualViewport /= desiredViewport || actualFrameBufferId /= desiredFrameBufferId) $ do
		let Vec4 left top right bottom = desiredViewport
		glViewport (fromIntegral $ clipX left) (fromIntegral $ clipY $ frameBufferHeight - bottom) (fromIntegral $ clipX $ right - left) (fromIntegral $ clipY $ bottom - top)
		glCheckErrors 0 "bind viewport"
		writeIORef actualViewportRef desiredViewport

	-- scissor
	when (actualScissor /= desiredScissor || actualViewport /= desiredViewport || actualFrameBufferId /= desiredFrameBufferId) $ do
		case desiredScissor of
			Just (Vec4 left top right bottom) -> do
				glEnable GL_SCISSOR_TEST
				glScissor (fromIntegral $ clipX left) (fromIntegral $ clipY $ frameBufferHeight - bottom) (fromIntegral $ clipX $ right - left) (fromIntegral $ clipY $ bottom - top)
				glCheckErrors 0 "bind scissor"
			Nothing -> do
				glDisable GL_SCISSOR_TEST
				glCheckErrors 0 "disable scissor"
		writeIORef actualScissorRef desiredScissor

glEnableDepthWriteForClearing :: GlContext -> IO ()
glEnableDepthWriteForClearing GlContext
	{ glContextActualState = GlContextState
		{ glContextStateDepthTestFunc = actualDepthTestFuncRef
		, glContextStateDepthWrite = actualDepthWriteRef
		}
	} = do
	-- enable depth test
	glEnable GL_DEPTH_TEST
	glDepthFunc GL_ALWAYS
	writeIORef actualDepthTestFuncRef DepthTestFuncAlways
	-- enable depth write
	glDepthMask 1
	writeIORef actualDepthWriteRef True

-- | Get size, type, isNormalized and isInteger for OpenGL by attribute type.
glGetAttributeSTNI :: AttributeType -> (GLint, GLenum, GLboolean, GLboolean)
glGetAttributeSTNI at = case at of
	ATFloat32 -> (1, GL_FLOAT, 0, 0)
	ATFloat16 -> (1, GL_HALF_FLOAT, 0, 0)
	ATInt32 n -> let nn = normalized n in (1, GL_INT, nn, 1 - nn)
	ATInt16 n -> let nn = normalized n in (1, GL_SHORT, nn, 1 - nn)
	ATInt8 n -> let nn = normalized n in (1, GL_BYTE, nn, 1 - nn)
	ATUint32 n -> let nn = normalized n in (1, GL_UNSIGNED_INT, nn, 1 - nn)
	ATUint16 n -> let nn = normalized n in (1, GL_UNSIGNED_SHORT, nn, 1 - nn)
	ATUint8 n -> let nn = normalized n in (1, GL_UNSIGNED_BYTE, nn, 1 - nn)
	ATVec1 r -> glGetAttributeSTNI r
	ATVec2 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (2, t, n, i)
	ATVec3 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (3, t, n, i)
	ATVec4 r -> let (_s, t, n, i) = glGetAttributeSTNI r in (4, t, n, i)
	-- matrices are not supported in attributes
	_ -> (0, 0, 0, 0) -- invalid values
	where normalized n = case n of
		NonNormalized -> 0
		Normalized -> 1

-- | Calculate OpenGL enum value for min filter.
glSamplerMinFilter :: SamplerStateInfo -> GLenum
glSamplerMinFilter SamplerStateInfo
	{ samplerMinFilter = minFilter
	, samplerMipFilter = mipFilter
	, samplerMaxLod = maxLod
	} = if maxLod > 0 then convertedMinMipFilter else convertedMinFilter where
	convertedMinMipFilter = case minFilter of
		SamplerPointFilter -> case mipFilter of
			SamplerPointFilter -> GL_NEAREST_MIPMAP_NEAREST
			SamplerLinearFilter -> GL_NEAREST_MIPMAP_LINEAR
		SamplerLinearFilter -> case mipFilter of
			SamplerPointFilter -> GL_LINEAR_MIPMAP_NEAREST
			SamplerLinearFilter -> GL_LINEAR_MIPMAP_LINEAR
	convertedMinFilter = case minFilter of
		SamplerPointFilter -> GL_NEAREST
		SamplerLinearFilter -> GL_LINEAR

-- | Calculate OpenGL enum value for mag filter.
glSamplerMagFilter :: SamplerStateInfo -> GLenum
glSamplerMagFilter SamplerStateInfo
	{ samplerMagFilter = magFilter
	} = case magFilter of
	SamplerPointFilter -> GL_NEAREST
	SamplerLinearFilter -> GL_LINEAR

-- | Calculate OpenGL enum value for wrapping.
glSamplerWrap :: SamplerWrap -> GLenum
glSamplerWrap wrap = case wrap of
	SamplerWrapRepeat -> GL_REPEAT
	SamplerWrapRepeatMirror -> GL_MIRRORED_REPEAT
	SamplerWrapClamp -> GL_CLAMP_TO_EDGE
	SamplerWrapBorder -> GL_CLAMP_TO_BORDER

-- | Set sampling settings for a texture.
glSetupTextureSampling :: GLenum -> SamplerStateInfo -> IO ()
glSetupTextureSampling target samplerStateInfo@SamplerStateInfo
	{ samplerWrapU = wrapU
	, samplerWrapV = wrapV
	, samplerWrapW = wrapW
	, samplerMinLod = minLod
	, samplerMaxLod = maxLod
	, samplerBorderColor = borderColor
	} = do
	-- min filter
	glTexParameteri target GL_TEXTURE_MIN_FILTER $ fromIntegral $ glSamplerMinFilter samplerStateInfo
	glCheckErrors 0 "min filter"
	-- mag filter
	glTexParameteri target GL_TEXTURE_MAG_FILTER $ fromIntegral $ glSamplerMagFilter samplerStateInfo
	glCheckErrors 0 "mag filter"

	-- wrap U
	glTexParameteri target GL_TEXTURE_WRAP_S $ fromIntegral $ glSamplerWrap wrapU
	glCheckErrors 0 "wrap U"
	-- wrap V
	glTexParameteri target GL_TEXTURE_WRAP_T $ fromIntegral $ glSamplerWrap wrapV
	glCheckErrors 0 "wrap V"
	-- wrap W
	glTexParameteri target GL_TEXTURE_WRAP_R $ fromIntegral $ glSamplerWrap wrapW
	glCheckErrors 0 "wrap W"

	-- min LOD
	glTexParameterf target GL_TEXTURE_MIN_LOD minLod
	glCheckErrors 0 "min LOD"
	-- max LOD
	glTexParameterf target GL_TEXTURE_MAX_LOD maxLod
	glCheckErrors 0 "max LOD"

	-- border color
	with borderColor $ glTexParameterfv target GL_TEXTURE_BORDER_COLOR . castPtr
	glCheckErrors 0 "border color"

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
refSetup_ actualRef desiredRef setup = void $ refSetup actualRef desiredRef setup

vectorSetupCond :: Eq a => Bool -> VM.IOVector a -> VM.IOVector a -> (Int -> a -> IO ()) -> IO ()
vectorSetupCond forceSetup actualVector desiredVector setup = do
	let len = VM.length actualVector
	forM_ [0..(len - 1)] $ \i -> do
		actual <- VM.unsafeRead actualVector i
		desired <- VM.unsafeRead desiredVector i
		when (forceSetup || actual /= desired) $ do
			setup i desired
			VM.unsafeWrite actualVector i desired

vectorSetup :: Eq a => VM.IOVector a -> VM.IOVector a -> (Int -> a -> IO ()) -> IO ()
vectorSetup = vectorSetupCond False

instance Eq (IndexBufferId GlDevice) where
	GlIndexBufferId indexBufferName1 _indicesType1 == GlIndexBufferId indexBufferName2 _indicesType2 = indexBufferName1 == indexBufferName2

instance Eq (ProgramId GlDevice) where
	GlProgramId { glProgramName = name1 } == GlProgramId { glProgramName = name2 } = name1 == name2

-- | Check for OpenGL errors, throw an exception if there's some.
{-# INLINABLE glCheckErrors #-}
glCheckErrors :: Int -> String -> IO ()
glCheckErrors level msg = unless (level < GL_ERROR_LEVEL) $ do
	firstError <- glGetError
	unless (firstError == GL_NO_ERROR) $ do
		let f restErrors = do
			nextError <- glGetError
			if nextError == GL_NO_ERROR then return restErrors
			else f $ nextError : restErrors
		errors <- f [firstError]
		throwIO $ DescribeFirstException $ show ("OpenGL error", msg, errors)

-- | Minimal level of error checking.
-- Increase this number to do less checks.
pattern GL_ERROR_LEVEL = 0

-- | Declaration for easier type annotation.
type BinaryProgram = (B.ByteString, Word64)
