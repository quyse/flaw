{-|
Module: Flaw.Graphics.OpenGL.FFI
Description: Some OpenGL FFI shims.
License: MIT
-}

module Flaw.Graphics.OpenGL.FFI
	(
	-- * Name manipulation
	-- ** Types
	  BufferName
	, TextureName
	, SamplerName
	, FramebufferName
	, VertexArrayName
	-- ** Allocation and deletion
	, glAllocBufferName
	, glDeleteBufferName
	, glAllocTextureName
	, glDeleteTextureName
	, glAllocSamplerName
	, glDeleteSamplerName
	, glAllocFramebufferName
	, glDeleteFramebufferName
	, glAllocVertexArrayName
	, glDeleteVertexArrayName
	-- * Texture uploading.
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
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.GL.Core33

type BufferName = GLuint
type TextureName = GLuint
type SamplerName = GLuint
type FramebufferName = GLuint
type VertexArrayName = GLuint

{-# INLINABLE glAllocBufferName #-}
glAllocBufferName :: IO BufferName
glAllocBufferName = alloca $ \namePtr -> do
	glGenBuffers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteBufferName #-}
glDeleteBufferName :: BufferName -> IO ()
glDeleteBufferName name = with name $ glDeleteBuffers 1

{-# INLINABLE glAllocTextureName #-}
glAllocTextureName :: IO TextureName
glAllocTextureName = alloca $ \namePtr -> do
	glGenTextures 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteTextureName #-}
glDeleteTextureName :: TextureName -> IO ()
glDeleteTextureName name = with name $ glDeleteTextures 1

{-# INLINABLE glAllocSamplerName #-}
glAllocSamplerName :: IO SamplerName
glAllocSamplerName = alloca $ \namePtr -> do
	glGenSamplers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteSamplerName #-}
glDeleteSamplerName :: SamplerName -> IO ()
glDeleteSamplerName name = with name $ glDeleteSamplers 1

{-# INLINABLE glAllocFramebufferName #-}
glAllocFramebufferName :: IO FramebufferName
glAllocFramebufferName = alloca $ \namePtr -> do
	glGenFramebuffers 1 namePtr
	peek namePtr

{-# INLINABLE glDeleteFramebufferName #-}
glDeleteFramebufferName :: FramebufferName -> IO ()
glDeleteFramebufferName name = with name $ glDeleteFramebuffers 1

{-# INLINABLE glAllocVertexArrayName #-}
glAllocVertexArrayName :: IO VertexArrayName
glAllocVertexArrayName = alloca $ \vaNamePtr -> do
	glGenVertexArrays 1 vaNamePtr
	peek vaNamePtr

{-# INLINABLE glDeleteVertexArrayName #-}
glDeleteVertexArrayName :: VertexArrayName -> IO ()
glDeleteVertexArrayName name = with name $ glDeleteVertexArrays 1

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
