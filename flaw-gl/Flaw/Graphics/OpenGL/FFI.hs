{-|
Module: Flaw.Graphics.OpenGL.FFI
Description: Some OpenGL FFI shims.
License: MIT
-}

module Flaw.Graphics.OpenGL.FFI
	(
	-- * Name manipulation
	-- ** Types
	  UniformLocation
	, BufferName
	, TextureName
	, SamplerName
	, FramebufferName
	, VertexArrayName
  , ProgramName
  , ShaderName
	-- ** Allocation and deletion
	, glAllocBufferName
	, glDeleteBufferName
	, glNullBufferName
	, glUndefinedBufferName
	, glAllocTextureName
	, glDeleteTextureName
	, glNullTextureName
	, glAllocSamplerName
	, glDeleteSamplerName
	, glNullSamplerName
	, glAllocFramebufferName
	, glDeleteFramebufferName
	, glNullFramebufferName
	, glAllocVertexArrayName
	, glDeleteVertexArrayName
	, glNullVertexArrayName
	, glNullProgramName
	-- * Buffer uploading
	, glBufferData_bs
	, glBufferData_null
	-- * Texture uploading
	, glTexImage1D_bs
	, glTexImage2D_bs
	, glTexImage3D_bs
	, glCompressedTexImage1D_bs
	, glCompressedTexImage2D_bs
	, glCompressedTexImage3D_bs
	, glTexSubImage1D_bs
	, glTexSubImage2D_bs
	, glTexSubImage3D_bs
	, glCompressedTexSubImage1D_bs
	, glCompressedTexSubImage2D_bs
	, glCompressedTexSubImage3D_bs
	, glTexImage2D_null
	-- * Sampler parameters
	, glSamplerParameterfv_4
	-- * Programs and shaders
	, glShaderSource_s
	, glGetProgramInfoLog_s
	, glGetShaderInfoLog_s
	, glBindAttribLocation_s
	, glGetUniformLocation_s
	, glGetUniformBlockIndex_s
	-- * Framebuffer
	, glDrawBuffers_n
	-- * Clearing
	, glClearBufferiv_1
	, glClearBufferfv_1
	, glClearBufferfv_4
	-- * Offsets
	, GlOffset
	, glIntToOffset
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core33

import Flaw.Math

type UniformLocation = GLint

type BufferName = GLuint
type TextureName = GLuint
type SamplerName = GLuint
type FramebufferName = GLuint
type VertexArrayName = GLuint
type ProgramName = GLuint
type ShaderName = GLuint

{-# INLINABLE glAllocBufferName #-}
glAllocBufferName :: IO BufferName
glAllocBufferName = alloca $ \namePtr -> do
	glGenBuffers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteBufferName #-}
glDeleteBufferName :: BufferName -> IO ()
glDeleteBufferName name = with name $ glDeleteBuffers 1

{-# INLINE glNullBufferName #-}
glNullBufferName :: BufferName
glNullBufferName = 0

{-# INLINE glUndefinedBufferName #-}
glUndefinedBufferName :: BufferName
glUndefinedBufferName = -1

{-# INLINABLE glAllocTextureName #-}
glAllocTextureName :: IO TextureName
glAllocTextureName = alloca $ \namePtr -> do
	glGenTextures 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteTextureName #-}
glDeleteTextureName :: TextureName -> IO ()
glDeleteTextureName name = with name $ glDeleteTextures 1

{-# INLINE glNullTextureName #-}
glNullTextureName :: TextureName
glNullTextureName = 0

{-# INLINABLE glAllocSamplerName #-}
glAllocSamplerName :: IO SamplerName
glAllocSamplerName = alloca $ \namePtr -> do
	glGenSamplers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteSamplerName #-}
glDeleteSamplerName :: SamplerName -> IO ()
glDeleteSamplerName name = with name $ glDeleteSamplers 1

{-# INLINE glNullSamplerName #-}
glNullSamplerName :: SamplerName
glNullSamplerName = 0

{-# INLINABLE glAllocFramebufferName #-}
glAllocFramebufferName :: IO FramebufferName
glAllocFramebufferName = alloca $ \namePtr -> do
	glGenFramebuffers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteFramebufferName #-}
glDeleteFramebufferName :: FramebufferName -> IO ()
glDeleteFramebufferName name = with name $ glDeleteFramebuffers 1

{-# INLINE glNullFramebufferName #-}
glNullFramebufferName :: FramebufferName
glNullFramebufferName = 0

{-# INLINABLE glAllocVertexArrayName #-}
glAllocVertexArrayName :: IO VertexArrayName
glAllocVertexArrayName = alloca $ \vaNamePtr -> do
	glGenVertexArrays 1 vaNamePtr
	peek vaNamePtr

{-# INLINABLE glDeleteVertexArrayName #-}
glDeleteVertexArrayName :: VertexArrayName -> IO ()
glDeleteVertexArrayName name = with name $ glDeleteVertexArrays 1

{-# INLINE glNullVertexArrayName #-}
glNullVertexArrayName :: VertexArrayName
glNullVertexArrayName = 0

{-# INLINE glNullProgramName #-}
glNullProgramName :: ProgramName
glNullProgramName = 0

{-# INLINABLE glBufferData_bs #-}
glBufferData_bs :: GLenum -> B.ByteString -> GLenum -> IO ()
glBufferData_bs target bytes usage = B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) ->
	glBufferData target (fromIntegral bytesLen) bytesPtr usage

{-# INLINABLE glBufferData_null #-}
glBufferData_null :: GLenum -> GLsizeiptr -> GLenum -> IO ()
glBufferData_null target size usage = glBufferData target size nullPtr usage

{-# INLINABLE glTexImage1D_bs #-}
glTexImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage1D_bs target level internalFormat width border format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexImage1D target level internalFormat width border format type_

{-# INLINABLE glTexImage2D_bs #-}
glTexImage2D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage2D_bs target level internalFormat width height border format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexImage2D target level internalFormat width height border format type_

{-# INLINABLE glTexImage3D_bs #-}
glTexImage3D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage3D_bs target level internalFormat width height depth border format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexImage3D target level internalFormat width height depth border format type_

{-# INLINABLE glCompressedTexImage1D_bs #-}
glCompressedTexImage1D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage1D_bs target level internalFormat width border bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexImage1D target level internalFormat width border (fromIntegral len) ptr

{-# INLINABLE glCompressedTexImage2D_bs #-}
glCompressedTexImage2D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage2D_bs target level internalFormat width height border bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexImage2D target level internalFormat width height border (fromIntegral len) ptr

{-# INLINABLE glCompressedTexImage3D_bs #-}
glCompressedTexImage3D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage3D_bs target level internalFormat width height depth border bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexImage3D target level internalFormat width height depth border (fromIntegral len) ptr

{-# INLINABLE glTexSubImage1D_bs #-}
glTexSubImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage1D_bs target level xoffset width format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexSubImage1D target level xoffset width format type_

{-# INLINABLE glTexSubImage2D_bs #-}
glTexSubImage2D_bs :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage2D_bs target level xoffset yoffset width height format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexSubImage2D target level xoffset yoffset width height format type_

{-# INLINABLE glTexSubImage3D_bs #-}
glTexSubImage3D_bs :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage3D_bs target level xoffset yoffset zoffset width height depth format type_ bytes =
	B.unsafeUseAsCString bytes $ glTexSubImage3D target level xoffset yoffset zoffset width height depth format type_

{-# INLINABLE glCompressedTexSubImage1D_bs #-}
glCompressedTexSubImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage1D_bs target level xoffset width format bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexSubImage1D target level xoffset width format (fromIntegral len) ptr

{-# INLINABLE glCompressedTexSubImage2D_bs #-}
glCompressedTexSubImage2D_bs :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage2D_bs target level xoffset yoffset width height format bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexSubImage2D target level xoffset yoffset width height format (fromIntegral len) ptr

{-# INLINABLE glCompressedTexSubImage3D_bs #-}
glCompressedTexSubImage3D_bs :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage3D_bs target level xoffset yoffset zoffset width height depth format bytes =
	B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> glCompressedTexSubImage3D target level xoffset yoffset zoffset width height depth format (fromIntegral len) ptr

{-# INLINABLE glTexImage2D_null #-}
glTexImage2D_null :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> IO ()
glTexImage2D_null target level internalFormat width height border format type_ =
	glTexImage2D target level internalFormat width height border format type_ nullPtr

{-# INLINABLE glSamplerParameterfv_4 #-}
glSamplerParameterfv_4 :: GLuint -> GLenum -> Float4 -> IO ()
glSamplerParameterfv_4 sampler pname fval = with fval $ glSamplerParameterfv sampler pname . castPtr


glShaderSource_s :: GLuint -> T.Text -> IO ()
glShaderSource_s shaderName source = B.unsafeUseAsCStringLen (T.encodeUtf8 source) $ \(sourcePtr, sourceLen) ->
	with sourcePtr $ \sourcePtrPtr ->
		with (fromIntegral sourceLen) $ \sourceLenPtr ->
			glShaderSource shaderName 1 sourcePtrPtr sourceLenPtr

glGetProgramInfoLog_s :: GLuint -> IO T.Text
glGetProgramInfoLog_s programName = do
	logLength <- alloca $ \logLengthPtr -> do
		glGetProgramiv programName GL_INFO_LOG_LENGTH logLengthPtr
		peek logLengthPtr
	fmap T.decodeUtf8 $ allocaBytes (fromIntegral logLength) $ \logPtr -> do
		realLogLength <- alloca $ \logLengthPtr -> do
			glGetProgramInfoLog programName logLength logLengthPtr logPtr
			peek logLengthPtr
		B.packCStringLen (logPtr, fromIntegral realLogLength)

glGetShaderInfoLog_s :: GLuint -> IO T.Text
glGetShaderInfoLog_s shaderName = do
	logLength <- alloca $ \logLengthPtr -> do
		poke logLengthPtr 0
		glGetShaderiv shaderName GL_INFO_LOG_LENGTH logLengthPtr
		peek logLengthPtr
	fmap T.decodeUtf8 $ allocaBytes (fromIntegral logLength) $ \logPtr -> do
		realLogLength <- alloca $ \logLengthPtr -> do
			glGetShaderInfoLog shaderName logLength logLengthPtr logPtr
			peek logLengthPtr
		B.packCStringLen (logPtr, fromIntegral realLogLength)

glBindAttribLocation_s :: GLuint -> GLuint -> T.Text -> IO ()
glBindAttribLocation_s programName index attributeName = B.useAsCString (T.encodeUtf8 attributeName) $ glBindAttribLocation programName index

glGetUniformLocation_s :: GLuint -> T.Text -> IO GLint
glGetUniformLocation_s programName locationName = B.useAsCString (T.encodeUtf8 locationName) $ glGetUniformLocation programName

glGetUniformBlockIndex_s :: GLuint -> T.Text -> IO GLuint
glGetUniformBlockIndex_s programName uniformBlockName = B.useAsCString (T.encodeUtf8 uniformBlockName) $ glGetUniformBlockIndex programName

glDrawBuffers_n :: Int -> IO ()
glDrawBuffers_n colorBuffersCount =
	withArray [GL_COLOR_ATTACHMENT0 + fromIntegral i | i <- [0..(colorBuffersCount - 1)]] $
	glDrawBuffers $ fromIntegral colorBuffersCount

{-# INLINABLE glClearBufferiv_1 #-}
glClearBufferiv_1 :: GLenum -> GLint -> GLint -> IO ()
glClearBufferiv_1 buffer drawBuffer ival = with ival $ glClearBufferiv buffer drawBuffer

{-# INLINABLE glClearBufferfv_1 #-}
glClearBufferfv_1 :: GLenum -> GLint -> GLfloat -> IO ()
glClearBufferfv_1 buffer drawBuffer fval = with fval $ glClearBufferfv buffer drawBuffer

{-# INLINABLE glClearBufferfv_4 #-}
glClearBufferfv_4 :: GLenum -> GLint -> Float4 -> IO ()
glClearBufferfv_4 buffer drawBuffer fval = with fval $ glClearBufferfv buffer drawBuffer . castPtr

type GlOffset = Ptr ()

{-# INLINE glIntToOffset #-}
glIntToOffset :: Int -> GlOffset
glIntToOffset = intPtrToPtr . fromIntegral
