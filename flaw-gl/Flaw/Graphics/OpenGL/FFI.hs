{-|
Module: Flaw.Graphics.OpenGL.FFI
Description: Some OpenGL FFI shims.
License: MIT
-}

module Flaw.Graphics.OpenGL.FFI
	( BufferName
	, TextureName
	, SamplerName
	, FramebufferName
	, VertexArrayName
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
	) where

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
