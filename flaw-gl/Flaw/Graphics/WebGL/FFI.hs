{-|
Module: Flaw.Graphics.WebGL.FFI
Description: Bindings for WebGL API.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI, PatternSynonyms, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Graphics.WebGL.FFI
  (
  -- * Introduction
  -- | This module is written manually using definitions from WebGL specs:
  -- <https://www.khronos.org/registry/webgl/specs/latest/1.0/webgl.idl WebGL 1.0 WebIDL>
  -- <https://www.khronos.org/registry/webgl/specs/latest/2.0/webgl2.idl WebGL 2.0 WebIDL>
    JS_WebGLContext(..)
  , JS_WebGLTexture(..)
  , JS_WebGLRenderbuffer(..)
  , JS_WebGLFramebuffer(..)
  , JS_WebGLBuffer(..)
  , JS_WebGLProgram(..)
  , JS_WebGLShader(..)
  , JS_WebGLUniformLocation(..)
  , UniformLocation
  , GLenum
  , GLint
  , GLuint
  , GLsizei
  , GLsizeiptr
  , GLintptr
  , GLclampf
  , GLboolean
  , GLbitfield
  -- * Initialization
  , js_getCanvasContext
  , js_setContext
  , js_requestAnimationFrame
  -- * WebGL functions
  , glGetExtension
  , glFrontFace
  , glCullFace
  , glEnable
  , glDisable
  , glGetError
  -- ** Textures
  , glCreateTexture
  , glDeleteTexture
  , glActiveTexture
  , glBindTexture
  , glTexImage2D_image
  , glTexParameterf
  , glTexParameteri
  , glTexParameterfv
  , glPixelStorei
  , glTexStorage1D
  , glTexStorage2D
  , glTexStorage3D
  -- ** Samplers
  , glCreateSampler
  , glDeleteSampler
  , glBindSampler
  , glSamplerParameteri
  , glSamplerParameterf
  , glSamplerParameterfv_4
  -- ** Buffers
  , glCreateBuffer
  , glDeleteBuffer
  , glBindBuffer
  , glBindBufferBase
  , glEnableVertexAttribArray
  , glDisableVertexAttribArray
  , glVertexAttribPointer
  , glVertexAttribIPointer
  , glVertexAttribDivisor
  -- ** Framebuffers
  , glCreateFramebuffer
  , glDeleteFramebuffer
  , glBindFramebuffer
  , glFramebufferTexture2D
  , glDrawBuffers_n
  -- ** Shaders and programs
  , glCreateShader
  , glDeleteShader
  , glCompileShader
  , glCreateProgram
  , glDeleteProgram
  , glAttachShader
  , glLinkProgram
  , glUseProgram
  , glUniformBlockBinding
  , glShaderSource_s
  , glGetShaderiv
  , glGetShaderInfoLog_s
  , glGetProgramiv
  , glGetProgramInfoLog_s
  , glBindAttribLocation_s
  , glGetUniformLocation_s
  , glGetUniformBlockIndex_s
  -- ** Uniforms
  , glUniform1i
  , glUniform1fv
  , glUniform2fv
  , glUniform3fv
  , glUniform4fv
  , glUniform1iv
  , glUniform2iv
  , glUniform3iv
  , glUniform4iv
  , glUniform1uiv
  , glUniform2uiv
  , glUniform3uiv
  , glUniform4uiv
  , glUniformMatrix3fv
  , glUniformMatrix4fv
  -- ** Vertex Arrays
  , glCreateVertexArray
  , glDeleteVertexArray
  , glBindVertexArray
  -- ** Depth output
  , glDepthFunc
  , glDepthMask
  -- ** Blending
  , glBlendEquationSeparate
  , glBlendFuncSeparate
  -- ** Scissor
  , glScissor
  -- ** Clearing.
  , glClearColor
  , glClearDepth
  , glClearStencil
  , glClear
  , glClearBufferiv_1
  , glClearBufferfv_1
  , glClearBufferfv_4
  , glClearBufferfi
  -- ** Viewport.
  , glViewport
  -- ** Drawing.
  , glDrawArrays
  , glDrawElements
  , glDrawArraysInstanced
  , glDrawElementsInstanced
  -- ** Misc.
  , glNativeTexture
  -- * Name manipulation.
  , BufferName
  , TextureName
  , SamplerName
  , FramebufferName
  , VertexArrayName
  , ProgramName
  , ShaderName
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
  -- * Offsets
  , GlOffset
  , glIntToOffset
  -- * Stubs
  , glProgramBinary
  , glGetProgramBinary
  , pattern GL_PROGRAM_BINARY_LENGTH
  , glBindFragDataLocation
  , glBindFragDataLocationIndexed
  , glVertexAttribFormat
  , glVertexAttribIFormat
  , glVertexAttribBinding
  , glVertexBindingDivisor
  , glBindVertexBuffer
  , glFinish
  -- * Shims
  -- ** Buffer uploading
  , glBufferData_bs
  , glBufferData_null
  -- ** Texture uploading
  -- | 1D versions don't exist in WebGL 1.0/2.0, but here for completeness.
  -- 3D versions exist only in WebGL 2.0.
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
  -- * Constants.
  , pattern GL_COLOR
  , pattern GL_DEPTH
  , pattern GL_STENCIL
  , pattern GL_UNIFORM_BUFFER
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_STENCIL_BUFFER_BIT
  , pattern GL_COLOR_BUFFER_BIT
  , pattern GL_POINTS
  , pattern GL_LINES
  , pattern GL_LINE_LOOP
  , pattern GL_LINE_STRIP
  , pattern GL_TRIANGLES
  , pattern GL_TRIANGLE_STRIP
  , pattern GL_TRIANGLE_FAN
  , pattern GL_PATCHES
  , pattern GL_ZERO
  , pattern GL_ONE
  , pattern GL_SRC_COLOR
  , pattern GL_ONE_MINUS_SRC_COLOR
  , pattern GL_SRC_ALPHA
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , pattern GL_DST_ALPHA
  , pattern GL_ONE_MINUS_DST_ALPHA
  , pattern GL_SRC1_ALPHA
  , pattern GL_SRC1_COLOR
  , pattern GL_ONE_MINUS_SRC1_COLOR
  , pattern GL_ONE_MINUS_SRC1_ALPHA
  , pattern GL_DST_COLOR
  , pattern GL_ONE_MINUS_DST_COLOR
  , pattern GL_SRC_ALPHA_SATURATE
  , pattern GL_FUNC_ADD
  , pattern GL_MIN
  , pattern GL_MAX
  , pattern GL_BLEND_EQUATION
  , pattern GL_BLEND_EQUATION_RGB
  , pattern GL_BLEND_EQUATION_ALPHA
  , pattern GL_FUNC_SUBTRACT
  , pattern GL_FUNC_REVERSE_SUBTRACT
  , pattern GL_BLEND_DST_RGB
  , pattern GL_BLEND_SRC_RGB
  , pattern GL_BLEND_DST_ALPHA
  , pattern GL_BLEND_SRC_ALPHA
  , pattern GL_CONSTANT_COLOR
  , pattern GL_ONE_MINUS_CONSTANT_COLOR
  , pattern GL_CONSTANT_ALPHA
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA
  , pattern GL_BLEND_COLOR
  , pattern GL_ARRAY_BUFFER
  , pattern GL_ELEMENT_ARRAY_BUFFER
  , pattern GL_ARRAY_BUFFER_BINDING
  , pattern GL_ELEMENT_ARRAY_BUFFER_BINDING
  , pattern GL_STREAM_DRAW
  , pattern GL_STATIC_DRAW
  , pattern GL_DYNAMIC_DRAW
  , pattern GL_BUFFER_SIZE
  , pattern GL_BUFFER_USAGE
  , pattern GL_CURRENT_VERTEX_ATTRIB
  , pattern GL_FRONT
  , pattern GL_BACK
  , pattern GL_FRONT_AND_BACK
  -- ** Comparison functions
  , pattern GL_NEVER
  , pattern GL_LESS
  , pattern GL_EQUAL
  , pattern GL_LEQUAL
  , pattern GL_GREATER
  , pattern GL_NOTEQUAL
  , pattern GL_GEQUAL
  , pattern GL_ALWAYS
  -- ** Features
  , pattern GL_CULL_FACE
  , pattern GL_BLEND
  , pattern GL_DITHER
  , pattern GL_STENCIL_TEST
  , pattern GL_DEPTH_TEST
  , pattern GL_SCISSOR_TEST
  , pattern GL_POLYGON_OFFSET_FILL
  , pattern GL_SAMPLE_ALPHA_TO_COVERAGE
  , pattern GL_SAMPLE_COVERAGE
  -- ** Errors
  , pattern GL_NO_ERROR
  , pattern GL_INVALID_ENUM
  , pattern GL_INVALID_VALUE
  , pattern GL_INVALID_OPERATION
  , pattern GL_OUT_OF_MEMORY
  , pattern GL_CW
  , pattern GL_CCW
  , pattern GL_LINE_WIDTH
  , pattern GL_ALIASED_POINT_SIZE_RANGE
  , pattern GL_ALIASED_LINE_WIDTH_RANGE
  , pattern GL_CULL_FACE_MODE
  , pattern GL_FRONT_FACE
  , pattern GL_DEPTH_RANGE
  , pattern GL_DEPTH_WRITEMASK
  , pattern GL_DEPTH_CLEAR_VALUE
  , pattern GL_DEPTH_FUNC
  , pattern GL_STENCIL_CLEAR_VALUE
  , pattern GL_STENCIL_FUNC
  , pattern GL_STENCIL_FAIL
  , pattern GL_STENCIL_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_PASS_DEPTH_PASS
  , pattern GL_STENCIL_REF
  , pattern GL_STENCIL_VALUE_MASK
  , pattern GL_STENCIL_WRITEMASK
  , pattern GL_STENCIL_BACK_FUNC
  , pattern GL_STENCIL_BACK_FAIL
  , pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL
  , pattern GL_STENCIL_BACK_PASS_DEPTH_PASS
  , pattern GL_STENCIL_BACK_REF
  , pattern GL_STENCIL_BACK_VALUE_MASK
  , pattern GL_STENCIL_BACK_WRITEMASK
  , pattern GL_VIEWPORT
  , pattern GL_SCISSOR_BOX
  , pattern GL_COLOR_CLEAR_VALUE
  , pattern GL_COLOR_WRITEMASK
  , pattern GL_UNPACK_ROW_LENGTH
  , pattern GL_UNPACK_SKIP_ROWS
  , pattern GL_UNPACK_SKIP_PIXELS
  , pattern GL_UNPACK_ALIGNMENT
  , pattern GL_UNPACK_IMAGE_HEIGHT
  , pattern GL_PACK_ROW_LENGTH
  , pattern GL_PACK_SKIP_ROWS
  , pattern GL_PACK_SKIP_PIXELS
  , pattern GL_PACK_ALIGNMENT
  , pattern GL_MAX_TEXTURE_SIZE
  , pattern GL_MAX_VIEWPORT_DIMS
  , pattern GL_SUBPIXEL_BITS
  , pattern GL_RED_BITS
  , pattern GL_GREEN_BITS
  , pattern GL_BLUE_BITS
  , pattern GL_ALPHA_BITS
  , pattern GL_DEPTH_BITS
  , pattern GL_STENCIL_BITS
  , pattern GL_POLYGON_OFFSET_UNITS
  , pattern GL_POLYGON_OFFSET_FACTOR
  , pattern GL_TEXTURE_BINDING_2D
  , pattern GL_SAMPLE_BUFFERS
  , pattern GL_SAMPLES
  , pattern GL_SAMPLE_COVERAGE_VALUE
  , pattern GL_SAMPLE_COVERAGE_INVERT
  , pattern GL_COMPRESSED_TEXTURE_FORMATS
  , pattern GL_DONT_CARE
  , pattern GL_FASTEST
  , pattern GL_NICEST
  , pattern GL_GENERATE_MIPMAP_HINT
  , pattern GL_BYTE
  , pattern GL_UNSIGNED_BYTE
  , pattern GL_SHORT
  , pattern GL_UNSIGNED_SHORT
  , pattern GL_INT
  , pattern GL_UNSIGNED_INT
  , pattern GL_FLOAT
  , pattern GL_HALF_FLOAT
  , pattern GL_DEPTH_COMPONENT
  , pattern GL_RED
  , pattern GL_ALPHA
  , pattern GL_RGB
  , pattern GL_RGBA
  , pattern GL_LUMINANCE
  , pattern GL_LUMINANCE_ALPHA
  , pattern GL_RGB8
  , pattern GL_RGBA8
  , pattern GL_RGBA16
  , pattern GL_RG
  , pattern GL_R8
  , pattern GL_R16
  , pattern GL_RG8
  , pattern GL_RG16
  , pattern GL_R16F
  , pattern GL_R32F
  , pattern GL_RG16F
  , pattern GL_RG32F
  , pattern GL_RGBA32F
  , pattern GL_RGB32F
  , pattern GL_RGBA16F
  , pattern GL_R11F_G11F_B10F
  , pattern GL_SRGB8_ALPHA8
  , pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
  , pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
  , pattern GL_COMPRESSED_RED
  , pattern GL_COMPRESSED_RG
  , pattern GL_COMPRESSED_RGB
  , pattern GL_COMPRESSED_RGBA
  , pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT
  , pattern GL_COMPRESSED_RED_RGTC1
  , pattern GL_COMPRESSED_SIGNED_RED_RGTC1
  , pattern GL_COMPRESSED_RG_RGTC2
  , pattern GL_COMPRESSED_SIGNED_RG_RGTC2
  , pattern GL_UNSIGNED_SHORT_4_4_4_4
  , pattern GL_UNSIGNED_SHORT_5_5_5_1
  , pattern GL_UNSIGNED_SHORT_5_6_5
  , pattern GL_UNSIGNED_INT_24_8
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_VERTEX_SHADER
  , pattern GL_MAX_VERTEX_ATTRIBS
  , pattern GL_MAX_VERTEX_UNIFORM_VECTORS
  , pattern GL_MAX_VARYING_VECTORS
  , pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_FRAGMENT_UNIFORM_VECTORS
  , pattern GL_SHADER_TYPE
  , pattern GL_DELETE_STATUS
  , pattern GL_LINK_STATUS
  , pattern GL_VALIDATE_STATUS
  , pattern GL_ATTACHED_SHADERS
  , pattern GL_ACTIVE_UNIFORMS
  , pattern GL_ACTIVE_ATTRIBUTES
  , pattern GL_SHADING_LANGUAGE_VERSION
  , pattern GL_CURRENT_PROGRAM
  , pattern GL_KEEP
  , pattern GL_REPLACE
  , pattern GL_INCR
  , pattern GL_DECR
  , pattern GL_INVERT
  , pattern GL_INCR_WRAP
  , pattern GL_DECR_WRAP
  , pattern GL_VENDOR
  , pattern GL_RENDERER
  , pattern GL_VERSION
  , pattern GL_NEAREST
  , pattern GL_LINEAR
  , pattern GL_NEAREST_MIPMAP_NEAREST
  , pattern GL_LINEAR_MIPMAP_NEAREST
  , pattern GL_NEAREST_MIPMAP_LINEAR
  , pattern GL_LINEAR_MIPMAP_LINEAR
  , pattern GL_TEXTURE_BORDER_COLOR
  , pattern GL_TEXTURE_MAG_FILTER
  , pattern GL_TEXTURE_MIN_FILTER
  , pattern GL_TEXTURE_WRAP_S
  , pattern GL_TEXTURE_WRAP_T
  , pattern GL_TEXTURE_WRAP_R
  , pattern GL_TEXTURE_MAX_ANISOTROPY_EXT
  , pattern GL_TEXTURE
  , pattern GL_TEXTURE_1D
  , pattern GL_TEXTURE_1D_ARRAY
  , pattern GL_TEXTURE_2D
  , pattern GL_TEXTURE_2D_ARRAY
  , pattern GL_TEXTURE_3D
  , pattern GL_TEXTURE_CUBE_MAP
  , pattern GL_TEXTURE_BINDING_CUBE_MAP
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  , pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE
  , pattern GL_TEXTURE0
  , pattern GL_TEXTURE1
  , pattern GL_TEXTURE2
  , pattern GL_TEXTURE3
  , pattern GL_TEXTURE4
  , pattern GL_TEXTURE5
  , pattern GL_TEXTURE6
  , pattern GL_TEXTURE7
  , pattern GL_TEXTURE8
  , pattern GL_TEXTURE9
  , pattern GL_TEXTURE10
  , pattern GL_TEXTURE11
  , pattern GL_TEXTURE12
  , pattern GL_TEXTURE13
  , pattern GL_TEXTURE14
  , pattern GL_TEXTURE15
  , pattern GL_TEXTURE16
  , pattern GL_TEXTURE17
  , pattern GL_TEXTURE18
  , pattern GL_TEXTURE19
  , pattern GL_TEXTURE20
  , pattern GL_TEXTURE21
  , pattern GL_TEXTURE22
  , pattern GL_TEXTURE23
  , pattern GL_TEXTURE24
  , pattern GL_TEXTURE25
  , pattern GL_TEXTURE26
  , pattern GL_TEXTURE27
  , pattern GL_TEXTURE28
  , pattern GL_TEXTURE29
  , pattern GL_TEXTURE30
  , pattern GL_TEXTURE31
  , pattern GL_ACTIVE_TEXTURE
  , pattern GL_REPEAT
  , pattern GL_CLAMP_TO_BORDER
  , pattern GL_CLAMP_TO_EDGE
  , pattern GL_MIRRORED_REPEAT
  , pattern GL_TEXTURE_MIN_LOD
  , pattern GL_TEXTURE_MAX_LOD
  , pattern GL_TEXTURE_BASE_LEVEL
  , pattern GL_TEXTURE_MAX_LEVEL
  , pattern GL_FLOAT_VEC2
  , pattern GL_FLOAT_VEC3
  , pattern GL_FLOAT_VEC4
  , pattern GL_INT_VEC2
  , pattern GL_INT_VEC3
  , pattern GL_INT_VEC4
  , pattern GL_BOOL
  , pattern GL_BOOL_VEC2
  , pattern GL_BOOL_VEC3
  , pattern GL_BOOL_VEC4
  , pattern GL_FLOAT_MAT2
  , pattern GL_FLOAT_MAT3
  , pattern GL_FLOAT_MAT4
  , pattern GL_SAMPLER_2D
  , pattern GL_SAMPLER_CUBE
  , pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED
  , pattern GL_VERTEX_ATTRIB_ARRAY_SIZE
  , pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE
  , pattern GL_VERTEX_ATTRIB_ARRAY_TYPE
  , pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
  , pattern GL_VERTEX_ATTRIB_ARRAY_POINTER
  , pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
  , pattern GL_IMPLEMENTATION_COLOR_READ_TYPE
  , pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT
  , pattern GL_COMPILE_STATUS
  , pattern GL_LOW_FLOAT
  , pattern GL_MEDIUM_FLOAT
  , pattern GL_HIGH_FLOAT
  , pattern GL_LOW_INT
  , pattern GL_MEDIUM_INT
  , pattern GL_HIGH_INT
  , pattern GL_FRAMEBUFFER
  , pattern GL_RENDERBUFFER
  , pattern GL_RGBA4
  , pattern GL_RGB5_A1
  , pattern GL_RGB565
  , pattern GL_DEPTH_COMPONENT16
  , pattern GL_STENCIL_INDEX
  , pattern GL_STENCIL_INDEX8
  , pattern GL_DEPTH_STENCIL
  , pattern GL_DEPTH24_STENCIL8
  , pattern GL_RENDERBUFFER_WIDTH
  , pattern GL_RENDERBUFFER_HEIGHT
  , pattern GL_RENDERBUFFER_INTERNAL_FORMAT
  , pattern GL_RENDERBUFFER_RED_SIZE
  , pattern GL_RENDERBUFFER_GREEN_SIZE
  , pattern GL_RENDERBUFFER_BLUE_SIZE
  , pattern GL_RENDERBUFFER_ALPHA_SIZE
  , pattern GL_RENDERBUFFER_DEPTH_SIZE
  , pattern GL_RENDERBUFFER_STENCIL_SIZE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
  , pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
  , pattern GL_COLOR_ATTACHMENT0
  , pattern GL_DEPTH_ATTACHMENT
  , pattern GL_STENCIL_ATTACHMENT
  , pattern GL_DEPTH_STENCIL_ATTACHMENT
  , pattern GL_NONE
  , pattern GL_FRAMEBUFFER_COMPLETE
  , pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  , pattern GL_FRAMEBUFFER_UNSUPPORTED
  , pattern GL_FRAMEBUFFER_BINDING
  , pattern GL_RENDERBUFFER_BINDING
  , pattern GL_MAX_RENDERBUFFER_SIZE
  , pattern GL_INVALID_FRAMEBUFFER_OPERATION
  , pattern GL_UNPACK_FLIP_Y_WEBGL
  , pattern GL_UNPACK_PREMULTIPLY_ALPHA_WEBGL
  , pattern GL_CONTEXT_LOST_WEBGL
  , pattern GL_UNPACK_COLORSPACE_CONVERSION_WEBGL
  , pattern GL_BROWSER_DEFAULT_WEBGL
  ) where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Text as T
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHCJS.Foreign.Callback
import GHCJS.Marshal.Pure
import GHCJS.Types

import Flaw.Exception
import Flaw.Js
import Flaw.Math

-- | Placeholders for WebGL javascript types.

newtype JS_WebGLContext = JS_WebGLContext JSVal
newtype JS_WebGLTexture = JS_WebGLTexture JSVal
newtype JS_WebGLSampler = JS_WebGLSampler JSVal
newtype JS_WebGLRenderbuffer = JS_WebGLRenderbuffer JSVal
newtype JS_WebGLFramebuffer = JS_WebGLFramebuffer JSVal
newtype JS_WebGLBuffer = JS_WebGLBuffer JSVal
newtype JS_WebGLProgram = JS_WebGLProgram JSVal
newtype JS_WebGLShader = JS_WebGLShader JSVal
newtype JS_WebGLUniformLocation = JS_WebGLUniformLocation JSVal
newtype JS_WebGLVertexArray = JS_WebGLVertexArray JSVal

type UniformLocation = JS_WebGLUniformLocation

-- 'Eq' instances

instance Eq JS_WebGLTexture where
  {-# INLINE (==) #-}
  (JS_WebGLTexture a) == (JS_WebGLTexture b) = a `jseq` b

instance Eq JS_WebGLSampler where
  {-# INLINE (==) #-}
  (JS_WebGLSampler a) == (JS_WebGLSampler b) = a `jseq` b

instance Eq JS_WebGLFramebuffer where
  {-# INLINE (==) #-}
  (JS_WebGLFramebuffer a) == (JS_WebGLFramebuffer b) = a `jseq` b

instance Eq JS_WebGLBuffer where
  {-# INLINE (==) #-}
  (JS_WebGLBuffer a) == (JS_WebGLBuffer b) = a `jseq` b

instance Eq JS_WebGLProgram where
  {-# INLINE (==) #-}
  (JS_WebGLProgram a) == (JS_WebGLProgram b) = a `jseq` b

instance Eq JS_WebGLVertexArray where
  {-# INLINE (==) #-}
  (JS_WebGLVertexArray a) == (JS_WebGLVertexArray b) = a `jseq` b

foreign import javascript unsafe "$1 === $2" js_eq :: JSVal -> JSVal -> JSVal
jseq :: JSVal -> JSVal -> Bool
jseq a b = pFromJSVal $ js_eq a b

-- WebGL API

type GLenum = Word
type GLfloat = Float
type GLint = Int
type GLuint = Word
type GLsizei = Int
type GLsizeiptr = Int
type GLintptr = Int
type GLclampf = Double
type GLboolean = Int
type GLbitfield = Word

-- | Get WebGL context from canvas DOM element.
foreign import javascript unsafe "h$flaw_webgl_get_canvas_context($1, $2)" js_getCanvasContext :: JSVal -> Bool -> IO JS_WebGLContext
-- | Set current WebGL context.
foreign import javascript unsafe "h$flaw_webgl_context = $1" js_setContext :: JS_WebGLContext -> IO ()
-- | Call callback when browser thinks it's better to render frame.
foreign import javascript unsafe "h$requestAnimationFrame($1);" js_requestAnimationFrame :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.getExtension($1)" glGetExtension :: JSString -> IO JSVal

foreign import javascript unsafe "h$flaw_webgl_context.frontFace($1)" glFrontFace :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.cullFace($1)" glCullFace :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.enable($1)" glEnable :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.disable($1)" glDisable :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.getError()" glGetError :: IO GLenum

foreign import javascript unsafe "h$flaw_webgl_context.createTexture()" glCreateTexture :: IO JS_WebGLTexture
foreign import javascript unsafe "h$flaw_webgl_context.deleteTexture($1)" glDeleteTexture :: JS_WebGLTexture -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.activeTexture($1)" glActiveTexture :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindTexture($1, $2)" glBindTexture :: GLenum -> JS_WebGLTexture -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.texImage2D($1, $2, $3, $4, $5, $6, $7, $8)"
  glTexImage1D_val :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texImage2D($1, $2, $3, $4, $5, $6, $7, $8, $9)"
  glTexImage2D_val :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texImage2D($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
  glTexImage3D_val :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> JSVal -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.compressedTexImage1D($1, $2, $3, $4, $5, $6)"
  glCompressedTexImage1D_val :: GLenum -> GLint -> GLenum -> GLsizei -> GLint -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.compressedTexImage2D($1, $2, $3, $4, $5, $6, $7)"
  glCompressedTexImage2D_val :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.compressedTexImage3D($1, $2, $3, $4, $5, $6, $7, $8)"
  glCompressedTexImage3D_val :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> JSVal -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.texSubImage1D($1, $2, $3, $4, $5, $6, $7)"
  glTexSubImage1D_val :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texSubImage2D($1, $2, $3, $4, $5, $6, $7, $8, $9)"
  glTexSubImage2D_val :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texSubImage3D($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"
  glTexSubImage3D_val :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> JSVal -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.compressedTexSubImage1D($1, $2, $3, $4, $5, $6)"
  glCompressedTexSubImage1D_val :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.compressedTexSubImage2D($1, $2, $3, $4, $5, $6, $7, $8)"
  glCompressedTexSubImage2D_val :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.compressedTexSubImage3D($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
  glCompressedTexSubImage3D_val :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> JSVal -> IO ()


foreign import javascript unsafe "h$flaw_webgl_context.texImage2D($1, $2, $3, $4, $5, $6)"
  glTexImage2D_image :: GLenum -> GLint -> GLint -> GLenum -> GLenum -> JSVal -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.texParameterf($1, $2, $3)" glTexParameterf :: GLenum -> GLenum -> GLfloat -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texParameteri($1, $2, $3)" glTexParameteri :: GLenum -> GLenum -> GLint -> IO ()
-- | 'glTexParameterfv' is not supported by WebGL 1.0/2.0, so here it's a no-op only for completeness.
{-# INLINE glTexParameterfv #-}
glTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()
glTexParameterfv _target _pname _value = return ()
foreign import javascript unsafe "h$flaw_webgl_context.pixelStorei($1, $2)" glPixelStorei :: GLenum -> GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texStorage1D($1, $2, $3, $4)" glTexStorage1D :: GLenum -> GLsizei -> GLenum -> GLsizei -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texStorage2D($1, $2, $3, $4, $5)" glTexStorage2D :: GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.texStorage3D($1, $2, $3, $4, $5, $6)" glTexStorage3D :: GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.createSampler()" glCreateSampler :: IO JS_WebGLSampler
foreign import javascript unsafe "h$flaw_webgl_context.deleteSampler($1)" glDeleteSampler :: JS_WebGLSampler -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindSampler($1, $2)" glBindSampler :: GLuint -> JS_WebGLSampler -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.samplerParameteri($1, $2, $3)" glSamplerParameteri :: JS_WebGLSampler -> GLenum -> GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.samplerParameterf($1, $2, $3)" glSamplerParameterf :: JS_WebGLSampler -> GLenum -> GLfloat -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.createBuffer()" glCreateBuffer :: IO JS_WebGLBuffer
foreign import javascript unsafe "h$flaw_webgl_context.deleteBuffer($1)" glDeleteBuffer :: JS_WebGLBuffer -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindBuffer($1, $2)" glBindBuffer :: GLenum -> JS_WebGLBuffer -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindBufferBase($1, $2, $3)" glBindBufferBase :: GLenum -> GLuint -> JS_WebGLBuffer -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bufferData($1, $2, $3)" glBufferData_val :: GLenum -> JSVal -> GLenum -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.createFramebuffer()" glCreateFramebuffer :: IO JS_WebGLFramebuffer
foreign import javascript unsafe "h$flaw_webgl_context.deleteFramebuffer($1)" glDeleteFramebuffer :: JS_WebGLFramebuffer -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindFramebuffer($1, $2)" glBindFramebuffer :: GLenum -> JS_WebGLFramebuffer -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.framebufferTexture2D($1, $2, $3, $4, $5)" glFramebufferTexture2D :: GLenum -> GLenum -> GLenum -> JS_WebGLTexture -> GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_glDrawBuffers_n($1)" glDrawBuffers_n :: Int -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.enableVertexAttribArray($1)" glEnableVertexAttribArray :: GLuint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.disableVertexAttribArray($1)" glDisableVertexAttribArray :: GLuint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.vertexAttribPointer($1, $2, $3, $4, $5, $6)" glVertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.vertexAttribIPointer($1, $2, $3, $4, $5)" glVertexAttribIPointer :: GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.vertexAttribDivisor($1, $2)" glVertexAttribDivisor :: GLuint -> GLuint -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.createShader($1)" glCreateShader :: GLenum -> IO JS_WebGLShader
foreign import javascript unsafe "h$flaw_webgl_context.deleteShader($1)" glDeleteShader :: JS_WebGLShader -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.shaderSource($1, $2)" glShaderSource_val :: JS_WebGLShader -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.compileShader($1)" glCompileShader :: JS_WebGLShader -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.getShaderParameter($1, $2)" glGetShaderParameter_val :: JS_WebGLShader -> GLenum -> IO JSVal
foreign import javascript unsafe "h$flaw_webgl_context.getShaderInfoLog($1)" glGetShaderInfoLog_val :: JS_WebGLShader -> IO JSVal
foreign import javascript unsafe "h$flaw_webgl_context.createProgram()" glCreateProgram :: IO JS_WebGLProgram
foreign import javascript unsafe "h$flaw_webgl_context.deleteProgram($1)" glDeleteProgram :: JS_WebGLProgram -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.getProgramInfoLog($1)" glGetProgramInfoLog_val :: JS_WebGLProgram -> IO JSVal
foreign import javascript unsafe "h$flaw_webgl_context.attachShader($1, $2)" glAttachShader :: JS_WebGLProgram -> JS_WebGLShader -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindAttribLocation($1, $2, $3)" glBindAttribLocation_val :: JS_WebGLProgram -> GLuint -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.linkProgram($1)" glLinkProgram :: JS_WebGLProgram -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.getProgramParameter($1, $2)" glGetProgramParameter_val :: JS_WebGLProgram -> GLenum -> IO JSVal
foreign import javascript unsafe "h$flaw_webgl_context.useProgram($1)" glUseProgram :: JS_WebGLProgram -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.getUniformLocation($1, $2)" glGetUniformLocation_val :: JS_WebGLProgram -> JSVal -> IO JS_WebGLUniformLocation
foreign import javascript unsafe "h$flaw_webgl_context.getUniformBlockIndex($1, $2)" glGetUniformBlockIndex_val :: JS_WebGLProgram -> JSVal -> IO GLuint
foreign import javascript unsafe "h$flaw_webgl_context.uniformBlockBinding($1, $2, $3)" glUniformBlockBinding :: JS_WebGLProgram -> GLuint -> GLuint -> IO ()

glShaderSource_s :: JS_WebGLShader -> T.Text -> IO ()
glShaderSource_s shader source = glShaderSource_val shader $ pToJSVal source

glGetShaderiv :: JS_WebGLShader -> GLenum -> Ptr GLint -> IO ()
glGetShaderiv shader param ptr = poke ptr =<< pFromJSVal <$> glGetShaderParameter_val shader param

glGetShaderInfoLog_s :: JS_WebGLShader -> IO T.Text
glGetShaderInfoLog_s shader = pFromJSVal <$> glGetShaderInfoLog_val shader

glGetProgramiv :: JS_WebGLProgram -> GLenum -> Ptr GLint -> IO ()
glGetProgramiv program param ptr = poke ptr =<< pFromJSVal <$> glGetProgramParameter_val program param

glGetProgramInfoLog_s :: JS_WebGLProgram -> IO T.Text
glGetProgramInfoLog_s program = pFromJSVal <$> glGetProgramInfoLog_val program

glBindAttribLocation_s :: JS_WebGLProgram -> GLuint -> T.Text -> IO ()
glBindAttribLocation_s program index attributeName = glBindAttribLocation_val program index $ pToJSVal attributeName

glGetUniformLocation_s :: JS_WebGLProgram -> T.Text -> IO JS_WebGLUniformLocation
glGetUniformLocation_s program locationName = glGetUniformLocation_val program $ pToJSVal locationName

glGetUniformBlockIndex_s :: JS_WebGLProgram -> T.Text -> IO GLuint
glGetUniformBlockIndex_s programName uniformBlockName = glGetUniformBlockIndex_val programName $ pToJSVal uniformBlockName

foreign import javascript unsafe "h$flaw_webgl_context.uniform1i($1, $2)" glUniform1i :: JS_WebGLUniformLocation -> GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform1fv($1, $2)" glUniform1fv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform2fv($1, $2)" glUniform2fv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform3fv($1, $2)" glUniform3fv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform4fv($1, $2)" glUniform4fv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform1iv($1, $2)" glUniform1iv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform2iv($1, $2)" glUniform2iv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform3iv($1, $2)" glUniform3iv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform4iv($1, $2)" glUniform4iv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform1uiv($1, $2)" glUniform1uiv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform2uiv($1, $2)" glUniform2uiv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform3uiv($1, $2)" glUniform3uiv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniform4uiv($1, $2)" glUniform4uiv_val :: JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniformMatrix3fv($1, $2, $3)" glUniformMatrix3fv_val :: JS_WebGLUniformLocation -> GLboolean -> JSVal -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.uniformMatrix4fv($1, $2, $3)" glUniformMatrix4fv_val :: JS_WebGLUniformLocation -> GLboolean -> JSVal -> IO ()

{-# INLINE glUniform1fv #-}
glUniform1fv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLfloat -> IO ()
glUniform1fv location size ptr = glUniform1fv_val location $ ptrToFloat32Array ptr size
{-# INLINE glUniform2fv #-}
glUniform2fv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLfloat -> IO ()
glUniform2fv location size ptr = glUniform2fv_val location $ ptrToFloat32Array ptr $ size * 2
{-# INLINE glUniform3fv #-}
glUniform3fv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLfloat -> IO ()
glUniform3fv location size ptr = glUniform3fv_val location $ ptrToFloat32Array ptr $ size * 3
{-# INLINE glUniform4fv #-}
glUniform4fv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLfloat -> IO ()
glUniform4fv location size ptr = glUniform4fv_val location $ ptrToFloat32Array ptr $ size * 4

{-# INLINE glUniform1iv #-}
glUniform1iv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLint -> IO ()
glUniform1iv location size ptr = glUniform1iv_val location $ ptrToInt32Array ptr size
{-# INLINE glUniform2iv #-}
glUniform2iv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLint -> IO ()
glUniform2iv location size ptr = glUniform2iv_val location $ ptrToInt32Array ptr $ size * 2
{-# INLINE glUniform3iv #-}
glUniform3iv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLint -> IO ()
glUniform3iv location size ptr = glUniform3iv_val location $ ptrToInt32Array ptr $ size * 3
{-# INLINE glUniform4iv #-}
glUniform4iv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLint -> IO ()
glUniform4iv location size ptr = glUniform4iv_val location $ ptrToInt32Array ptr $ size * 4

{-# INLINE glUniform1uiv #-}
glUniform1uiv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLuint -> IO ()
glUniform1uiv location size ptr = glUniform1uiv_val location $ ptrToUint32Array ptr size
{-# INLINE glUniform2uiv #-}
glUniform2uiv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLuint -> IO ()
glUniform2uiv location size ptr = glUniform2uiv_val location $ ptrToUint32Array ptr $ size * 2
{-# INLINE glUniform3uiv #-}
glUniform3uiv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLuint -> IO ()
glUniform3uiv location size ptr = glUniform3uiv_val location $ ptrToUint32Array ptr $ size * 3
{-# INLINE glUniform4uiv #-}
glUniform4uiv :: JS_WebGLUniformLocation -> GLsizei -> Ptr GLuint -> IO ()
glUniform4uiv location size ptr = glUniform4uiv_val location $ ptrToUint32Array ptr $ size * 4

{-# INLINE glUniformMatrix3fv #-}
glUniformMatrix3fv :: JS_WebGLUniformLocation -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()
glUniformMatrix3fv location size transpose ptr = glUniformMatrix3fv_val location transpose $ ptrToFloat32Array ptr $ size * 16
{-# INLINE glUniformMatrix4fv #-}
glUniformMatrix4fv :: JS_WebGLUniformLocation -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()
glUniformMatrix4fv location size transpose ptr = glUniformMatrix4fv_val location transpose $ ptrToFloat32Array ptr $ size * 16


foreign import javascript unsafe "h$flaw_webgl_context.createVertexArray()" glCreateVertexArray :: IO JS_WebGLVertexArray
foreign import javascript unsafe "h$flaw_webgl_context.deleteVertexArray($1)" glDeleteVertexArray :: JS_WebGLVertexArray -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.bindVertexArray($1)" glBindVertexArray :: JS_WebGLVertexArray -> IO ()


foreign import javascript unsafe "h$flaw_webgl_context.depthFunc($1)" glDepthFunc :: GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.depthMask($1)" glDepthMask :: GLboolean -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.blendEquationSeparate($1, $2)" glBlendEquationSeparate :: GLenum -> GLenum -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.blendFuncSeparate($1, $2, $3, $4)" glBlendFuncSeparate :: GLenum -> GLenum -> GLenum -> GLenum -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.scissor($1, $2, $3, $4)" glScissor :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- Clearing functions.
foreign import javascript unsafe "h$flaw_webgl_context.clearColor($1, $2, $3, $4)"
  glClearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clearDepth($1)"
  glClearDepth :: GLclampf -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clearStencil($1)"
  glClearStencil :: GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clear($1)"
  glClear :: GLbitfield -> IO ()

{-# INLINE glClearBufferfv_4 #-}
glClearBufferfv_4 :: GLenum -> GLint -> Float4 -> IO ()
glClearBufferfv_4 buffer drawBuffer (Float4 r g b a) = glClearBufferfv_4_helper buffer drawBuffer r g b a

-- MRT clearing functions (WebGL 2.0).
foreign import javascript unsafe "h$flaw_webgl_context.clearBufferiv($1, $2, [$3])"
  glClearBufferiv_1 :: GLenum -> GLint -> GLint -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clearBufferfv($1, $2, [$3])"
  glClearBufferfv_1 :: GLenum -> GLint -> GLfloat -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clearBufferfv($1, $2, [$3, $4, $5, $6])"
  glClearBufferfv_4_helper :: GLenum -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.clearBufferfi($1, $2, $3, $4)"
  glClearBufferfi :: GLenum -> GLint -> GLfloat -> GLint -> IO ()

foreign import javascript unsafe "h$flaw_webgl_context.viewport($1, $2, $3, $4)"
  glViewport :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.drawArrays($1, $2, $3)"
  glDrawArrays :: GLenum -> GLint -> GLsizei -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.drawElements($1, $2, $3, $4)"
  glDrawElements :: GLenum -> GLsizei -> GLenum -> GLintptr -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.drawArraysInstanced($1, $2, $3, $4)"
  glDrawArraysInstanced :: GLenum -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "h$flaw_webgl_context.drawElementsInstanced($1, $2, $3, $4, $5)"
  glDrawElementsInstanced :: GLenum -> GLsizei -> GLenum -> GLintptr -> GLsizei -> IO ()


-- Buffer uploading.

{-# INLINABLE glBufferData_bs #-}
glBufferData_bs :: GLenum -> B.ByteString -> GLenum -> IO ()
glBufferData_bs target bytes usage = glBufferData_val target (byteStringToJsDataView bytes) usage

{-# INLINABLE glBufferData_null #-}
glBufferData_null :: GLenum -> GLsizeiptr -> GLenum -> IO ()
glBufferData_null target size usage = glBufferData_val target (pToJSVal size) usage


-- Texture uploading.

-- | Helper for calling functions with typed array.
{-# INLINE texImageBytes #-}
texImageBytes :: GLenum -> B.ByteString -> JSVal
texImageBytes type_ bytes = case type_ of
  GL_UNSIGNED_BYTE -> byteStringToJsUint8Array bytes
  GL_UNSIGNED_SHORT -> byteStringToJsUint16Array bytes
  GL_UNSIGNED_INT -> byteStringToJsUint32Array bytes
  GL_FLOAT -> byteStringToJsFloatArray bytes
  _ -> byteStringToJsDataView bytes


{-# INLINABLE glTexImage1D_bs #-}
glTexImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage1D_bs target level internalFormat width border format type_ bytes =
  glTexImage1D_val target level internalFormat width border format type_ $ texImageBytes type_ bytes

{-# INLINABLE glTexImage2D_bs #-}
glTexImage2D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage2D_bs target level internalFormat width height border format type_ bytes =
  glTexImage2D_val target level internalFormat width height border format type_ $ texImageBytes type_ bytes

{-# INLINABLE glTexImage3D_bs #-}
glTexImage3D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexImage3D_bs target level internalFormat width height depth border format type_ bytes =
  glTexImage3D_val target level internalFormat width height depth border format type_ $ texImageBytes type_ bytes

{-# INLINABLE glCompressedTexImage1D_bs #-}
glCompressedTexImage1D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage1D_bs target level internalFormat width border bytes =
  glCompressedTexImage1D_val target level internalFormat width border $ byteStringToJsDataView bytes

{-# INLINABLE glCompressedTexImage2D_bs #-}
glCompressedTexImage2D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage2D_bs target level internalFormat width height border bytes =
  glCompressedTexImage2D_val target level internalFormat width height border $ byteStringToJsDataView bytes

{-# INLINABLE glCompressedTexImage3D_bs #-}
glCompressedTexImage3D_bs :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> B.ByteString -> IO ()
glCompressedTexImage3D_bs target level internalFormat width height depth border bytes =
  glCompressedTexImage3D_val target level internalFormat width height depth border $ byteStringToJsDataView bytes

{-# INLINABLE glTexSubImage1D_bs #-}
glTexSubImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage1D_bs target level xoffset width format type_ bytes =
  glTexSubImage1D_val target level xoffset width format type_ $ byteStringToJsDataView bytes

{-# INLINABLE glTexSubImage2D_bs #-}
glTexSubImage2D_bs :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage2D_bs target level xoffset yoffset width height format type_ bytes =
  glTexSubImage2D_val target level xoffset yoffset width height format type_ $ byteStringToJsDataView bytes

{-# INLINABLE glTexSubImage3D_bs #-}
glTexSubImage3D_bs :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> B.ByteString -> IO ()
glTexSubImage3D_bs target level xoffset yoffset zoffset width height depth format type_ bytes =
  glTexSubImage3D_val target level xoffset yoffset zoffset width height depth format type_ $ byteStringToJsDataView bytes

{-# INLINABLE glCompressedTexSubImage1D_bs #-}
glCompressedTexSubImage1D_bs :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage1D_bs target level xoffset width format bytes =
  glCompressedTexSubImage1D_val target level xoffset width format $ byteStringToJsDataView bytes

{-# INLINABLE glCompressedTexSubImage2D_bs #-}
glCompressedTexSubImage2D_bs :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage2D_bs target level xoffset yoffset width height format bytes =
  glCompressedTexSubImage2D_val target level xoffset yoffset width height format $ byteStringToJsDataView bytes

{-# INLINABLE glCompressedTexSubImage3D_bs #-}
glCompressedTexSubImage3D_bs :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> B.ByteString -> IO ()
glCompressedTexSubImage3D_bs target level xoffset yoffset zoffset width height depth format bytes =
  glCompressedTexSubImage3D_val target level xoffset yoffset zoffset width height depth format $ byteStringToJsDataView bytes

{-# INLINABLE glTexImage2D_null #-}
glTexImage2D_null :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> IO ()
glTexImage2D_null target level internalFormat width height border format type_ =
  glTexImage2D_val target level internalFormat width height border format type_ nullRef


-- Sampler parameter

{-# INLINABLE glSamplerParameterfv_4 #-}
glSamplerParameterfv_4 :: JS_WebGLSampler -> GLenum -> Float4 -> IO ()
glSamplerParameterfv_4 sampler pname (Float4 r g b a) = glSamplerParameterfv_helper sampler pname r g b a

-- doesn't exist in WebGL 1.0/2.0
foreign import javascript unsafe "h$flaw_webgl_context.samplerParameterfv($1, $2, [$3, $4, $5, $6])" glSamplerParameterfv_helper :: JS_WebGLSampler -> GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()


-- Name operations

type BufferName = JS_WebGLBuffer
type TextureName = JS_WebGLTexture
type SamplerName = JS_WebGLSampler
type FramebufferName = JS_WebGLFramebuffer
type VertexArrayName = JS_WebGLVertexArray
type ProgramName = JS_WebGLProgram
type ShaderName = JS_WebGLShader

{-# INLINABLE glAllocBufferName #-}
glAllocBufferName :: IO BufferName
glAllocBufferName = glCreateBuffer

{-# INLINABLE glDeleteBufferName #-}
glDeleteBufferName :: BufferName -> IO ()
glDeleteBufferName = glDeleteBuffer

foreign import javascript unsafe "$r = null" glNullBufferName :: JS_WebGLBuffer

foreign import javascript unsafe "$r = undefined" glUndefinedBufferName :: JS_WebGLBuffer

{-# INLINABLE glAllocTextureName #-}
glAllocTextureName :: IO TextureName
glAllocTextureName = glCreateTexture

{-# INLINABLE glDeleteTextureName #-}
glDeleteTextureName :: TextureName -> IO ()
glDeleteTextureName = glDeleteTexture

foreign import javascript unsafe "$r = null" glNullTextureName :: JS_WebGLTexture

{-# INLINABLE glAllocSamplerName #-}
glAllocSamplerName :: IO SamplerName
glAllocSamplerName = glCreateSampler

{-# INLINABLE glDeleteSamplerName #-}
glDeleteSamplerName :: SamplerName -> IO ()
glDeleteSamplerName = glDeleteSampler

foreign import javascript unsafe "$r = null" glNullSamplerName :: JS_WebGLSampler

{-# INLINABLE glAllocFramebufferName #-}
glAllocFramebufferName :: IO FramebufferName
glAllocFramebufferName = glCreateFramebuffer

{-# INLINABLE glDeleteFramebufferName #-}
glDeleteFramebufferName :: FramebufferName -> IO ()
glDeleteFramebufferName = glDeleteFramebuffer

foreign import javascript unsafe "$r = null" glNullFramebufferName :: JS_WebGLFramebuffer

{-# INLINABLE glAllocVertexArrayName #-}
glAllocVertexArrayName :: IO VertexArrayName
glAllocVertexArrayName = glCreateVertexArray

{-# INLINABLE glDeleteVertexArrayName #-}
glDeleteVertexArrayName :: VertexArrayName -> IO ()
glDeleteVertexArrayName = glDeleteVertexArray

foreign import javascript unsafe "$r = null" glNullVertexArrayName :: JS_WebGLVertexArray

foreign import javascript unsafe "$r = null" glNullProgramName :: JS_WebGLProgram


-- Helpers.

-- | Load texture natively.
glNativeTexture :: B.ByteString -> IO (GLenum, JS_WebGLTexture)
glNativeTexture bytes = do
  objectUrl <- createObjectUrl mempty $ byteStringToJsDataView bytes
  image <- js_loadImage objectUrl
  revokeObjectUrl objectUrl
  jsTexture <- glCreateTexture
  glBindTexture GL_TEXTURE_2D jsTexture
  glTexImage2D_image GL_TEXTURE_2D 0 GL_RGBA GL_RGBA GL_UNSIGNED_BYTE image
  return (GL_TEXTURE_2D, jsTexture)

foreign import javascript interruptible "var image=new Image();image.onload=function(){$c(image);};image.src=$1;" js_loadImage :: JSString -> IO JSVal


type GlOffset = GLintptr

{-# INLINE glIntToOffset #-}
glIntToOffset :: Int -> GlOffset
glIntToOffset = id


-- Stubs.

glProgramBinary :: JS_WebGLProgram -> GLenum -> Ptr a -> GLsizei -> IO ()
glProgramBinary _ _ _ _ = throwIO $ DescribeFirstException "glProgramBinary is not supported in WebGL"

glGetProgramBinary :: JS_WebGLProgram -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr a -> IO ()
glGetProgramBinary _ _ _ _ _ = throwIO $ DescribeFirstException "glGetProgramBinary is not supported in WebGL"

pattern GL_PROGRAM_BINARY_LENGTH = 0x8741

glBindFragDataLocation :: JS_WebGLProgram -> GLuint -> Ptr CChar -> IO ()
glBindFragDataLocation _ _ _ = throwIO $ DescribeFirstException "glBindFragDataLocation is not supported in WebGL"

glBindFragDataLocationIndexed :: JS_WebGLProgram -> GLuint -> GLuint -> Ptr CChar -> IO ()
glBindFragDataLocationIndexed _ _ _ _ = throwIO $ DescribeFirstException "glBindFragDataLocationIndexed is not supported in WebGL"

glVertexAttribFormat :: GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ()
glVertexAttribFormat _ _ _ _ _ = throwIO $ DescribeFirstException "glVertexAttribFormat is not supported in WebGL"

glVertexAttribIFormat :: GLuint -> GLint -> GLenum -> GLuint -> IO ()
glVertexAttribIFormat _ _ _ _ = throwIO $ DescribeFirstException "glVertexAttribIFormat is not supported in WebGL"

glVertexAttribBinding :: GLuint -> GLuint -> IO ()
glVertexAttribBinding _ _ = throwIO $ DescribeFirstException "glVertexAttribBinding is not supported in WebGL"

glVertexBindingDivisor :: GLuint -> GLuint -> IO ()
glVertexBindingDivisor _ _ = throwIO $ DescribeFirstException "glVertexBindingDivisor is not supported in WebGL"

glBindVertexBuffer :: GLuint -> JS_WebGLBuffer -> GLintptr -> GLintptr -> IO ()
glBindVertexBuffer _ _ _ _ = throwIO $ DescribeFirstException "glBindVertexBuffer is not supported in WebGL"

-- glFinish is currently used only by background operations for synchronization.
-- WebGL implementation doesn't have background operations, so glFinish is no-op.
{-# INLINE glFinish #-}
glFinish :: IO ()
glFinish = return ()


-- Constants.

-- Targets
pattern GL_COLOR = 0x1800
pattern GL_DEPTH = 0x1801
pattern GL_STENCIL = 0x1802
pattern GL_UNIFORM_BUFFER = 0x8A11

-- ClearBufferMask
pattern GL_DEPTH_BUFFER_BIT = 0x00000100
pattern GL_STENCIL_BUFFER_BIT = 0x00000400
pattern GL_COLOR_BUFFER_BIT = 0x00004000

-- BeginMode
pattern GL_POINTS = 0x0000
pattern GL_LINES = 0x0001
pattern GL_LINE_LOOP = 0x0002
pattern GL_LINE_STRIP = 0x0003
pattern GL_TRIANGLES = 0x0004
pattern GL_TRIANGLE_STRIP = 0x0005
pattern GL_TRIANGLE_FAN = 0x0006
pattern GL_PATCHES = 0x000E

-- BlendingFactorDest
pattern GL_ZERO = 0
pattern GL_ONE = 1
pattern GL_SRC_COLOR = 0x0300
pattern GL_ONE_MINUS_SRC_COLOR = 0x0301
pattern GL_SRC_ALPHA = 0x0302
pattern GL_ONE_MINUS_SRC_ALPHA = 0x0303
pattern GL_DST_ALPHA = 0x0304
pattern GL_ONE_MINUS_DST_ALPHA = 0x0305
pattern GL_SRC1_ALPHA = 0x8589
pattern GL_SRC1_COLOR = 0x88F9
pattern GL_ONE_MINUS_SRC1_COLOR = 0x88FA
pattern GL_ONE_MINUS_SRC1_ALPHA = 0x88FB

-- BlendingFactorSrc
--      ZERO
--      ONE
pattern GL_DST_COLOR = 0x0306
pattern GL_ONE_MINUS_DST_COLOR = 0x0307
pattern GL_SRC_ALPHA_SATURATE = 0x0308
--      SRC_ALPHA
--      ONE_MINUS_SRC_ALPHA
--      DST_ALPHA
--      ONE_MINUS_DST_ALPHA

-- BlendEquationSeparate
pattern GL_FUNC_ADD = 0x8006
pattern GL_MIN = 0x8007 -- not supported in WebGL 1.0/2.0
pattern GL_MAX = 0x8008 -- not supported in WebGL 1.0/2.0
pattern GL_BLEND_EQUATION = 0x8009
pattern GL_BLEND_EQUATION_RGB = 0x8009
pattern GL_BLEND_EQUATION_ALPHA = 0x883D

-- BlendSubtract
pattern GL_FUNC_SUBTRACT = 0x800A
pattern GL_FUNC_REVERSE_SUBTRACT = 0x800B

-- Separate Blend Functions
pattern GL_BLEND_DST_RGB = 0x80C8
pattern GL_BLEND_SRC_RGB = 0x80C9
pattern GL_BLEND_DST_ALPHA = 0x80CA
pattern GL_BLEND_SRC_ALPHA = 0x80CB
pattern GL_CONSTANT_COLOR = 0x8001
pattern GL_ONE_MINUS_CONSTANT_COLOR = 0x8002
pattern GL_CONSTANT_ALPHA = 0x8003
pattern GL_ONE_MINUS_CONSTANT_ALPHA = 0x8004
pattern GL_BLEND_COLOR = 0x8005

-- Buffer Objects
pattern GL_ARRAY_BUFFER = 0x8892
pattern GL_ELEMENT_ARRAY_BUFFER = 0x8893
pattern GL_ARRAY_BUFFER_BINDING = 0x8894
pattern GL_ELEMENT_ARRAY_BUFFER_BINDING = 0x8895

pattern GL_STREAM_DRAW = 0x88E0
pattern GL_STATIC_DRAW = 0x88E4
pattern GL_DYNAMIC_DRAW = 0x88E8

pattern GL_BUFFER_SIZE = 0x8764
pattern GL_BUFFER_USAGE = 0x8765

pattern GL_CURRENT_VERTEX_ATTRIB = 0x8626

-- CullFaceMode
pattern GL_FRONT = 0x0404
pattern GL_BACK = 0x0405
pattern GL_FRONT_AND_BACK = 0x0408

-- Comparison functions
pattern GL_NEVER = 0x0200
pattern GL_LESS = 0x0201
pattern GL_EQUAL = 0x0202
pattern GL_LEQUAL = 0x0203
pattern GL_GREATER = 0x0204
pattern GL_NOTEQUAL = 0x0205
pattern GL_GEQUAL = 0x0206
pattern GL_ALWAYS = 0x0207

-- Features
pattern GL_CULL_FACE = 0x0B44
pattern GL_BLEND = 0x0BE2
pattern GL_DITHER = 0x0BD0
pattern GL_STENCIL_TEST = 0x0B90
pattern GL_DEPTH_TEST = 0x0B71
pattern GL_SCISSOR_TEST = 0x0C11
pattern GL_POLYGON_OFFSET_FILL = 0x8037
pattern GL_SAMPLE_ALPHA_TO_COVERAGE = 0x809E
pattern GL_SAMPLE_COVERAGE = 0x80A0

-- ErrorCode
pattern GL_NO_ERROR = 0
pattern GL_INVALID_ENUM = 0x0500
pattern GL_INVALID_VALUE = 0x0501
pattern GL_INVALID_OPERATION = 0x0502
pattern GL_OUT_OF_MEMORY = 0x0505

-- FrontFaceDirection
pattern GL_CW = 0x0900
pattern GL_CCW = 0x0901

-- GetPName
pattern GL_LINE_WIDTH = 0x0B21
pattern GL_ALIASED_POINT_SIZE_RANGE = 0x846D
pattern GL_ALIASED_LINE_WIDTH_RANGE = 0x846E
pattern GL_CULL_FACE_MODE = 0x0B45
pattern GL_FRONT_FACE = 0x0B46
pattern GL_DEPTH_RANGE = 0x0B70
pattern GL_DEPTH_WRITEMASK = 0x0B72
pattern GL_DEPTH_CLEAR_VALUE = 0x0B73
pattern GL_DEPTH_FUNC = 0x0B74
pattern GL_STENCIL_CLEAR_VALUE = 0x0B91
pattern GL_STENCIL_FUNC = 0x0B92
pattern GL_STENCIL_FAIL = 0x0B94
pattern GL_STENCIL_PASS_DEPTH_FAIL = 0x0B95
pattern GL_STENCIL_PASS_DEPTH_PASS = 0x0B96
pattern GL_STENCIL_REF = 0x0B97
pattern GL_STENCIL_VALUE_MASK = 0x0B93
pattern GL_STENCIL_WRITEMASK = 0x0B98
pattern GL_STENCIL_BACK_FUNC = 0x8800
pattern GL_STENCIL_BACK_FAIL = 0x8801
pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL = 0x8802
pattern GL_STENCIL_BACK_PASS_DEPTH_PASS = 0x8803
pattern GL_STENCIL_BACK_REF = 0x8CA3
pattern GL_STENCIL_BACK_VALUE_MASK = 0x8CA4
pattern GL_STENCIL_BACK_WRITEMASK = 0x8CA5
pattern GL_VIEWPORT = 0x0BA2
pattern GL_SCISSOR_BOX = 0x0C10
--      SCISSOR_TEST
pattern GL_COLOR_CLEAR_VALUE = 0x0C22
pattern GL_COLOR_WRITEMASK = 0x0C23
pattern GL_UNPACK_ROW_LENGTH = 0x0CF2
pattern GL_UNPACK_SKIP_ROWS = 0x0CF3
pattern GL_UNPACK_SKIP_PIXELS = 0x0CF4
pattern GL_UNPACK_ALIGNMENT = 0x0CF5
pattern GL_UNPACK_IMAGE_HEIGHT = 0x806E
pattern GL_PACK_ROW_LENGTH = 0x0D02
pattern GL_PACK_SKIP_ROWS = 0x0D03
pattern GL_PACK_SKIP_PIXELS = 0x0D04
pattern GL_PACK_ALIGNMENT = 0x0D05
pattern GL_MAX_TEXTURE_SIZE = 0x0D33
pattern GL_MAX_VIEWPORT_DIMS = 0x0D3A
pattern GL_SUBPIXEL_BITS = 0x0D50
pattern GL_RED_BITS = 0x0D52
pattern GL_GREEN_BITS = 0x0D53
pattern GL_BLUE_BITS = 0x0D54
pattern GL_ALPHA_BITS = 0x0D55
pattern GL_DEPTH_BITS = 0x0D56
pattern GL_STENCIL_BITS = 0x0D57
pattern GL_POLYGON_OFFSET_UNITS = 0x2A00
--      POLYGON_OFFSET_FILL
pattern GL_POLYGON_OFFSET_FACTOR = 0x8038
pattern GL_TEXTURE_BINDING_2D = 0x8069
pattern GL_SAMPLE_BUFFERS = 0x80A8
pattern GL_SAMPLES = 0x80A9
pattern GL_SAMPLE_COVERAGE_VALUE = 0x80AA
pattern GL_SAMPLE_COVERAGE_INVERT = 0x80AB

pattern GL_COMPRESSED_TEXTURE_FORMATS = 0x86A3

-- HintMode
pattern GL_DONT_CARE = 0x1100
pattern GL_FASTEST = 0x1101
pattern GL_NICEST = 0x1102

-- HintTarget
pattern GL_GENERATE_MIPMAP_HINT = 0x8192

-- DataType
pattern GL_BYTE = 0x1400
pattern GL_UNSIGNED_BYTE = 0x1401
pattern GL_SHORT = 0x1402
pattern GL_UNSIGNED_SHORT = 0x1403
pattern GL_INT = 0x1404
pattern GL_UNSIGNED_INT = 0x1405
pattern GL_FLOAT = 0x1406
pattern GL_HALF_FLOAT = 0x140B

-- PixelFormat
-- WebGL 1.0 doesn't support a lot of values; replacing them with supported ones.
pattern GL_DEPTH_COMPONENT = 0x1902
pattern GL_RED = GL_LUMINANCE -- (0x1903) not supported in WebGL 1.0
pattern GL_ALPHA = 0x1906
pattern GL_RGB = 0x1907
pattern GL_RGBA = 0x1908
pattern GL_LUMINANCE = 0x1909
pattern GL_LUMINANCE_ALPHA = 0x190A
pattern GL_RGB8 = GL_RGB -- (0x8051) not supported in WebGL 1.0
pattern GL_RGBA8 = GL_RGBA -- (0x8058) not supported in WebGL 1.0
pattern GL_RGBA16 = GL_RGBA -- (0x805B) not supported in WebGL 1.0
pattern GL_RG = 0x8227 -- not supported in WebGL 1.0
pattern GL_R8 = GL_RED -- (0x8229) not supported in WebGL 1.0
pattern GL_R16 = GL_RED -- (0x822A) not supported in WebGL 1.0
pattern GL_RG8 = 0x822B -- not supported in WebGL 1.0
pattern GL_RG16 = 0x822C -- not supported in WebGL 1.0
pattern GL_R16F = GL_RED -- (0x822D) not supported in WebGL 1.0
pattern GL_R32F = GL_RED -- (0x822E) not supported in WebGL 1.0
pattern GL_RG16F = 0x822F -- not supported in WebGL 1.0
pattern GL_RG32F = 0x8230 -- not supported in WebGL 1.0
pattern GL_RGBA32F = GL_RGBA -- (0x8814) not supported in WebGL 1.0
pattern GL_RGB32F = GL_RGB -- (0x8815) not supported in WebGL 1.0
pattern GL_RGBA16F = GL_RGBA -- (0x881A) not supported in WebGL 1.0
pattern GL_R11F_G11F_B10F = GL_RGB -- (0x8C3A) not supported in WebGL 1.0
pattern GL_SRGB8_ALPHA8 = 0x8C42 -- TEST 0x8C43

-- Compressed pixel formats
-- WEBGL_compressed_texture_s3tc extension
pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT = 0x83F0
pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = 0x83F1
pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = 0x83F2
pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = 0x83F3

pattern GL_COMPRESSED_RED = 0x8225
pattern GL_COMPRESSED_RG = 0x8226
pattern GL_COMPRESSED_RGB = 0x84ED
pattern GL_COMPRESSED_RGBA = 0x84EE
pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = 0x8C4C
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = 0x8C4D
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = 0x8C4E
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = 0x8C4F
pattern GL_COMPRESSED_RED_RGTC1 = 0x8DBB
pattern GL_COMPRESSED_SIGNED_RED_RGTC1 = 0x8DBC
pattern GL_COMPRESSED_RG_RGTC2 = 0x8DBD
pattern GL_COMPRESSED_SIGNED_RG_RGTC2 = 0x8DBE

-- PixelType
--      UNSIGNED_BYTE
pattern GL_UNSIGNED_SHORT_4_4_4_4 = 0x8033
pattern GL_UNSIGNED_SHORT_5_5_5_1 = 0x8034
pattern GL_UNSIGNED_SHORT_5_6_5 = 0x8363
pattern GL_UNSIGNED_INT_24_8 = 0x84FA

-- Shaders
pattern GL_FRAGMENT_SHADER = 0x8B30
pattern GL_VERTEX_SHADER = 0x8B31
pattern GL_MAX_VERTEX_ATTRIBS = 0x8869
pattern GL_MAX_VERTEX_UNIFORM_VECTORS = 0x8DFB
pattern GL_MAX_VARYING_VECTORS = 0x8DFC
pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D
pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = 0x8B4C
pattern GL_MAX_TEXTURE_IMAGE_UNITS = 0x8872
pattern GL_MAX_FRAGMENT_UNIFORM_VECTORS = 0x8DFD
pattern GL_SHADER_TYPE = 0x8B4F
pattern GL_DELETE_STATUS = 0x8B80
pattern GL_LINK_STATUS = 0x8B82
pattern GL_VALIDATE_STATUS = 0x8B83
pattern GL_ATTACHED_SHADERS = 0x8B85
pattern GL_ACTIVE_UNIFORMS = 0x8B86
pattern GL_ACTIVE_ATTRIBUTES = 0x8B89
pattern GL_SHADING_LANGUAGE_VERSION = 0x8B8C
pattern GL_CURRENT_PROGRAM = 0x8B8D

-- StencilOp
--      ZERO
pattern GL_KEEP = 0x1E00
pattern GL_REPLACE = 0x1E01
pattern GL_INCR = 0x1E02
pattern GL_DECR = 0x1E03
pattern GL_INVERT = 0x150A
pattern GL_INCR_WRAP = 0x8507
pattern GL_DECR_WRAP = 0x8508

-- StringName
pattern GL_VENDOR = 0x1F00
pattern GL_RENDERER = 0x1F01
pattern GL_VERSION = 0x1F02

-- TextureMagFilter
pattern GL_NEAREST = 0x2600 :: GLenum
pattern GL_LINEAR = 0x2601

-- TextureMinFilter
--      NEAREST
--      LINEAR
pattern GL_NEAREST_MIPMAP_NEAREST = 0x2700
pattern GL_LINEAR_MIPMAP_NEAREST = 0x2701
pattern GL_NEAREST_MIPMAP_LINEAR = 0x2702
pattern GL_LINEAR_MIPMAP_LINEAR = 0x2703

-- TextureParameterName
pattern GL_TEXTURE_BORDER_COLOR = 0x1004 -- not supported in WebGL 1.0/2.0
pattern GL_TEXTURE_MAG_FILTER = 0x2800
pattern GL_TEXTURE_MIN_FILTER = 0x2801
pattern GL_TEXTURE_WRAP_S = 0x2802
pattern GL_TEXTURE_WRAP_T = 0x2803
pattern GL_TEXTURE_WRAP_R = 0x8072
pattern GL_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FE

-- TextureTarget
pattern GL_TEXTURE = 0x1702
pattern GL_TEXTURE_1D = 0x0DE0 -- not supported in WebGL 1.0/2.0
pattern GL_TEXTURE_1D_ARRAY = 0x8C18 -- not supported in WebGL 1.0/2.0
pattern GL_TEXTURE_2D = 0x0DE1
pattern GL_TEXTURE_2D_ARRAY = 0x8C1A
pattern GL_TEXTURE_3D = 0x806F

pattern GL_TEXTURE_CUBE_MAP = 0x8513
pattern GL_TEXTURE_BINDING_CUBE_MAP = 0x8514
pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X = 0x8515
pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X = 0x8516
pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y = 0x8517
pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = 0x8518
pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z = 0x8519
pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = 0x851A
pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE = 0x851C

-- TextureUnit
pattern GL_TEXTURE0 = 0x84C0
pattern GL_TEXTURE1 = 0x84C1
pattern GL_TEXTURE2 = 0x84C2
pattern GL_TEXTURE3 = 0x84C3
pattern GL_TEXTURE4 = 0x84C4
pattern GL_TEXTURE5 = 0x84C5
pattern GL_TEXTURE6 = 0x84C6
pattern GL_TEXTURE7 = 0x84C7
pattern GL_TEXTURE8 = 0x84C8
pattern GL_TEXTURE9 = 0x84C9
pattern GL_TEXTURE10 = 0x84CA
pattern GL_TEXTURE11 = 0x84CB
pattern GL_TEXTURE12 = 0x84CC
pattern GL_TEXTURE13 = 0x84CD
pattern GL_TEXTURE14 = 0x84CE
pattern GL_TEXTURE15 = 0x84CF
pattern GL_TEXTURE16 = 0x84D0
pattern GL_TEXTURE17 = 0x84D1
pattern GL_TEXTURE18 = 0x84D2
pattern GL_TEXTURE19 = 0x84D3
pattern GL_TEXTURE20 = 0x84D4
pattern GL_TEXTURE21 = 0x84D5
pattern GL_TEXTURE22 = 0x84D6
pattern GL_TEXTURE23 = 0x84D7
pattern GL_TEXTURE24 = 0x84D8
pattern GL_TEXTURE25 = 0x84D9
pattern GL_TEXTURE26 = 0x84DA
pattern GL_TEXTURE27 = 0x84DB
pattern GL_TEXTURE28 = 0x84DC
pattern GL_TEXTURE29 = 0x84DD
pattern GL_TEXTURE30 = 0x84DE
pattern GL_TEXTURE31 = 0x84DF
pattern GL_ACTIVE_TEXTURE = 0x84E0

-- TextureWrapMode
pattern GL_REPEAT = 0x2901
pattern GL_CLAMP_TO_BORDER = GL_CLAMP_TO_EDGE -- (0x812D) not supported in WebGL 1.0/2.0
pattern GL_CLAMP_TO_EDGE = 0x812F :: GLenum
pattern GL_MIRRORED_REPEAT = 0x8370

-- Texture Levels
pattern GL_TEXTURE_MIN_LOD = 0x813A
pattern GL_TEXTURE_MAX_LOD = 0x813B
pattern GL_TEXTURE_BASE_LEVEL = 0x813C
pattern GL_TEXTURE_MAX_LEVEL = 0x813D

-- Uniform Types
pattern GL_FLOAT_VEC2 = 0x8B50
pattern GL_FLOAT_VEC3 = 0x8B51
pattern GL_FLOAT_VEC4 = 0x8B52
pattern GL_INT_VEC2 = 0x8B53
pattern GL_INT_VEC3 = 0x8B54
pattern GL_INT_VEC4 = 0x8B55
pattern GL_BOOL = 0x8B56
pattern GL_BOOL_VEC2 = 0x8B57
pattern GL_BOOL_VEC3 = 0x8B58
pattern GL_BOOL_VEC4 = 0x8B59
pattern GL_FLOAT_MAT2 = 0x8B5A
pattern GL_FLOAT_MAT3 = 0x8B5B
pattern GL_FLOAT_MAT4 = 0x8B5C
pattern GL_SAMPLER_2D = 0x8B5E
pattern GL_SAMPLER_CUBE = 0x8B60

-- Vertex Arrays
pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED = 0x8622
pattern GL_VERTEX_ATTRIB_ARRAY_SIZE = 0x8623
pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE = 0x8624
pattern GL_VERTEX_ATTRIB_ARRAY_TYPE = 0x8625
pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = 0x886A
pattern GL_VERTEX_ATTRIB_ARRAY_POINTER = 0x8645
pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F

-- Read Format
pattern GL_IMPLEMENTATION_COLOR_READ_TYPE = 0x8B9A
pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT = 0x8B9B

-- Shader Source
pattern GL_COMPILE_STATUS = 0x8B81

-- Shader Precision-Specified Types
pattern GL_LOW_FLOAT = 0x8DF0
pattern GL_MEDIUM_FLOAT = 0x8DF1
pattern GL_HIGH_FLOAT = 0x8DF2
pattern GL_LOW_INT = 0x8DF3
pattern GL_MEDIUM_INT = 0x8DF4
pattern GL_HIGH_INT = 0x8DF5

-- Framebuffer Object.
pattern GL_FRAMEBUFFER = 0x8D40
pattern GL_RENDERBUFFER = 0x8D41

pattern GL_RGBA4 = 0x8056
pattern GL_RGB5_A1 = 0x8057
pattern GL_RGB565 = 0x8D62
pattern GL_DEPTH_COMPONENT16 = 0x81A5
pattern GL_STENCIL_INDEX = 0x1901
pattern GL_STENCIL_INDEX8 = 0x8D48
pattern GL_DEPTH_STENCIL = 0x84F9
pattern GL_DEPTH24_STENCIL8 = 0x88F0 :: GLenum

pattern GL_RENDERBUFFER_WIDTH = 0x8D42
pattern GL_RENDERBUFFER_HEIGHT = 0x8D43
pattern GL_RENDERBUFFER_INTERNAL_FORMAT = 0x8D44
pattern GL_RENDERBUFFER_RED_SIZE = 0x8D50
pattern GL_RENDERBUFFER_GREEN_SIZE = 0x8D51
pattern GL_RENDERBUFFER_BLUE_SIZE = 0x8D52
pattern GL_RENDERBUFFER_ALPHA_SIZE = 0x8D53
pattern GL_RENDERBUFFER_DEPTH_SIZE = 0x8D54
pattern GL_RENDERBUFFER_STENCIL_SIZE = 0x8D55

pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = 0x8CD0
pattern GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = 0x8CD1
pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = 0x8CD2
pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3

pattern GL_COLOR_ATTACHMENT0 = 0x8CE0
pattern GL_DEPTH_ATTACHMENT = 0x8D00
pattern GL_STENCIL_ATTACHMENT = 0x8D20
pattern GL_DEPTH_STENCIL_ATTACHMENT = 0x821A

pattern GL_NONE = 0

pattern GL_FRAMEBUFFER_COMPLETE = 0x8CD5
pattern GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 0x8CD6
pattern GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7
pattern GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 0x8CD9
pattern GL_FRAMEBUFFER_UNSUPPORTED = 0x8CDD

pattern GL_FRAMEBUFFER_BINDING = 0x8CA6
pattern GL_RENDERBUFFER_BINDING = 0x8CA7
pattern GL_MAX_RENDERBUFFER_SIZE = 0x84E8

pattern GL_INVALID_FRAMEBUFFER_OPERATION = 0x0506

-- WebGL-specific enums
pattern GL_UNPACK_FLIP_Y_WEBGL = 0x9240
pattern GL_UNPACK_PREMULTIPLY_ALPHA_WEBGL = 0x9241
pattern GL_CONTEXT_LOST_WEBGL = 0x9242
pattern GL_UNPACK_COLORSPACE_CONVERSION_WEBGL = 0x9243
pattern GL_BROWSER_DEFAULT_WEBGL = 0x9244
