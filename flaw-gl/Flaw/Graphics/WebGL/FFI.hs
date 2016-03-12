{-|
Module: Flaw.Graphics.WebGL.FFI
Description: Bindings for WebGL API.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI, PatternSynonyms #-}

module Flaw.Graphics.WebGL.FFI
	( JS_WebGLContext(..)
	, JS_WebGLTexture(..)
	, JS_WebGLRenderbuffer(..)
	, JS_WebGLFramebuffer(..)
	, JS_WebGLBuffer(..)
	, JS_WebGLProgram(..)
	, JS_WebGLShader(..)
	, JS_WebGLUniformLocation(..)
	, GLenum
	, GLint
	, GLuint
	, GLsizei
	, GLintptr
	, GLclampf
	, GLboolean
	, GLbitfield
	, js_getWebGLContext
	, js_getExtension
	, js_frontFace
	, js_cullFace
	, js_enable
	, js_disable
	, js_createTexture
	, js_activeTexture
	, js_bindTexture
	, js_texImage2D
	, js_texParameterf
	, js_texParameteri
	, js_createBuffer
	, js_bindBuffer
	, js_bufferData
	, js_enableVertexAttribArray
	, js_disableVertexAttribArray
	, js_vertexAttribPointer
	, js_createShader
	, js_shaderSource
	, js_compileShader
	, js_getShaderParameter
	, js_getShaderInfoLog
	, js_createProgram
	, js_attachShader
	, js_bindAttribLocation
	, js_linkProgram
	, js_getProgramParameter
	, js_useProgram
	, js_getUniformLocation
	, js_uniform1f
	, js_uniform1i
	, js_uniform1fv
	, js_uniform2fv
	, js_uniform3fv
	, js_uniform4fv
	, js_uniformMatrix4fv
	, js_uniform1iv
	, js_clearColor
	, js_clearDepth
	, js_clearStencil
	, js_clear
	, js_viewport
	, js_drawArrays
	, js_drawElements
	, js_drawArraysInstanced
	, js_drawElementsInstanced
	, js_loadImage
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
	, pattern GL_ZERO
	, pattern GL_ONE
	, pattern GL_SRC_COLOR
	, pattern GL_ONE_MINUS_SRC_COLOR
	, pattern GL_SRC_ALPHA
	, pattern GL_ONE_MINUS_SRC_ALPHA
	, pattern GL_DST_ALPHA
	, pattern GL_ONE_MINUS_DST_ALPHA
	, pattern GL_DST_COLOR
	, pattern GL_ONE_MINUS_DST_COLOR
	, pattern GL_SRC_ALPHA_SATURATE
	, pattern GL_FUNC_ADD
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
	, pattern GL_CULL_FACE
	, pattern GL_BLEND
	, pattern GL_DITHER
	, pattern GL_STENCIL_TEST
	, pattern GL_DEPTH_TEST
	, pattern GL_SCISSOR_TEST
	, pattern GL_POLYGON_OFFSET_FILL
	, pattern GL_SAMPLE_ALPHA_TO_COVERAGE
	, pattern GL_SAMPLE_COVERAGE
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
	, pattern GL_UNPACK_ALIGNMENT
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
	, pattern GL_DEPTH_COMPONENT
	, pattern GL_ALPHA
	, pattern GL_RGB
	, pattern GL_RGBA
	, pattern GL_LUMINANCE
	, pattern GL_LUMINANCE_ALPHA
	, pattern GL_UNSIGNED_SHORT_4_4_4_4
	, pattern GL_UNSIGNED_SHORT_5_5_5_1
	, pattern GL_UNSIGNED_SHORT_5_6_5
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
	, pattern GL_NEVER
	, pattern GL_LESS
	, pattern GL_EQUAL
	, pattern GL_LEQUAL
	, pattern GL_GREATER
	, pattern GL_NOTEQUAL
	, pattern GL_GEQUAL
	, pattern GL_ALWAYS
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
	, pattern GL_TEXTURE_MAG_FILTER
	, pattern GL_TEXTURE_MIN_FILTER
	, pattern GL_TEXTURE_WRAP_S
	, pattern GL_TEXTURE_WRAP_T
	, pattern GL_TEXTURE_2D
	, pattern GL_TEXTURE
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
	, pattern GL_CLAMP_TO_EDGE
	, pattern GL_MIRRORED_REPEAT
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

import qualified GHCJS.DOM.Element as DOM
import GHCJS.Types

-- | Placeholders for WebGL javascript types.

newtype JS_WebGLContext = JS_WebGLContext JSVal
newtype JS_WebGLTexture = JS_WebGLTexture JSVal
newtype JS_WebGLRenderbuffer = JS_WebGLRenderbuffer JSVal
newtype JS_WebGLFramebuffer = JS_WebGLFramebuffer JSVal
newtype JS_WebGLBuffer = JS_WebGLBuffer JSVal
newtype JS_WebGLProgram = JS_WebGLProgram JSVal
newtype JS_WebGLShader = JS_WebGLShader JSVal
newtype JS_WebGLUniformLocation = JS_WebGLUniformLocation JSVal

-- WebGL API

type GLenum = Word
type GLfloat = Float
type GLint = Int
type GLuint = Word
type GLsizei = Int
type GLintptr = Int
type GLclampf = Float
type GLboolean = Bool
type GLbitfield = Word

foreign import javascript unsafe " \
	\ var settings = \
	\ { alpha: false \
	\ , depth: $2 \
	\ , stencil: false \
	\ , antialias: true \
	\ }; \
	\ $r = $1.getContext('webgl',settings) || $1.getContext('experimental-webgl', settings); \
	\" js_getWebGLContext :: DOM.Element -> Bool -> IO JS_WebGLContext

foreign import javascript unsafe "$1.getExtension($2)" js_getExtension :: JS_WebGLContext -> JSString -> IO JSVal

foreign import javascript unsafe "$1.frontFace($2)" js_frontFace :: JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.cullFace($2)" js_cullFace :: JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.enable($2)" js_enable :: JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.disable($2)" js_disable :: JS_WebGLContext -> GLenum -> IO ()

foreign import javascript unsafe "$1.createTexture()" js_createTexture :: JS_WebGLContext -> IO JS_WebGLTexture
foreign import javascript unsafe "$1.activeTexture($2)" js_activeTexture :: JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.bindTexture($2, $3)" js_bindTexture :: JS_WebGLContext -> GLenum -> JS_WebGLTexture -> IO ()
foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7)" js_texImage2D :: JS_WebGLContext -> GLenum -> GLint -> GLenum -> GLenum -> GLenum -> JSVal -> IO ()
foreign import javascript unsafe "$1.texParameterf($2, $3, $4)" js_texParameterf :: JS_WebGLContext -> GLenum -> GLenum -> GLfloat -> IO ()
foreign import javascript unsafe "$1.texParameteri($2, $3, $4)" js_texParameteri :: JS_WebGLContext -> GLenum -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1.createBuffer()" js_createBuffer :: JS_WebGLContext -> IO JS_WebGLBuffer
foreign import javascript unsafe "$1.bindBuffer($2, $3)" js_bindBuffer :: JS_WebGLContext -> GLenum -> JS_WebGLBuffer -> IO ()
foreign import javascript unsafe "$1.bufferData($2, $3, $4)" js_bufferData :: JS_WebGLContext -> GLenum -> JSVal -> GLenum -> IO ()

foreign import javascript unsafe "$1.enableVertexAttribArray($2)" js_enableVertexAttribArray :: JS_WebGLContext -> GLuint -> IO ()
foreign import javascript unsafe "$1.disableVertexAttribArray($2)" js_disableVertexAttribArray :: JS_WebGLContext -> GLuint -> IO ()
foreign import javascript unsafe "$1.vertexAttribPointer($2, $3, $4, $5, $6, $7)" js_vertexAttribPointer :: JS_WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()

foreign import javascript unsafe "$1.createShader($2)" js_createShader :: JS_WebGLContext -> GLenum -> IO JS_WebGLShader
foreign import javascript unsafe "$1.shaderSource($2, $3)" js_shaderSource :: JS_WebGLContext -> JS_WebGLShader -> JSString -> IO ()
foreign import javascript unsafe "$1.compileShader($2)" js_compileShader :: JS_WebGLContext -> JS_WebGLShader -> IO ()
foreign import javascript unsafe "$1.getShaderParameter($2, $3)" js_getShaderParameter :: JS_WebGLContext -> JS_WebGLShader -> GLenum -> IO JSVal
foreign import javascript unsafe "$1.getShaderInfoLog($2)" js_getShaderInfoLog :: JS_WebGLContext -> JS_WebGLShader -> IO JSString
foreign import javascript unsafe "$1.createProgram()" js_createProgram :: JS_WebGLContext -> IO JS_WebGLProgram
foreign import javascript unsafe "$1.attachShader($2, $3)" js_attachShader :: JS_WebGLContext -> JS_WebGLProgram -> JS_WebGLShader -> IO ()
foreign import javascript unsafe "$1.bindAttribLocation($2, $3, $4)" js_bindAttribLocation :: JS_WebGLContext -> JS_WebGLProgram -> GLuint -> JSString -> IO ()
foreign import javascript unsafe "$1.linkProgram($2)" js_linkProgram :: JS_WebGLContext -> JS_WebGLProgram -> IO ()
foreign import javascript unsafe "$1.getProgramParameter($2, $3)" js_getProgramParameter :: JS_WebGLContext -> JS_WebGLProgram -> GLenum -> IO JSVal
foreign import javascript unsafe "$1.useProgram($2)" js_useProgram :: JS_WebGLContext -> JS_WebGLProgram -> IO ()
foreign import javascript unsafe "$1.getUniformLocation($2, $3)" js_getUniformLocation :: JS_WebGLContext -> JS_WebGLProgram -> JSString -> IO JS_WebGLUniformLocation

foreign import javascript unsafe "$1.uniform1f($2, $3)" js_uniform1f :: JS_WebGLContext -> JS_WebGLUniformLocation -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniform1i($2, $3)" js_uniform1i :: JS_WebGLContext -> JS_WebGLUniformLocation -> GLint -> IO ()
foreign import javascript unsafe "$1.uniform1fv($2, $3)" js_uniform1fv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "$1.uniform2fv($2, $3)" js_uniform2fv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "$1.uniform3fv($2, $3)" js_uniform3fv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "$1.uniform4fv($2, $3)" js_uniform4fv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "$1.uniformMatrix4fv($2, false, $3)" js_uniformMatrix4fv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()
foreign import javascript unsafe "$1.uniform1iv($2, $3)" js_uniform1iv :: JS_WebGLContext -> JS_WebGLUniformLocation -> JSVal -> IO ()

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)" js_clearColor :: JS_WebGLContext -> GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ()
foreign import javascript unsafe "$1.clearDepth($2)" js_clearDepth :: JS_WebGLContext -> GLclampf -> IO ()
foreign import javascript unsafe "$1.clearStencil($2)" js_clearStencil :: JS_WebGLContext -> GLint -> IO ()
foreign import javascript unsafe "$1.clear($2)" js_clear :: JS_WebGLContext -> GLbitfield -> IO ()

foreign import javascript unsafe "$1.viewport($2, $3, $4, $5)" js_viewport :: JS_WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "$1.drawArrays($2, $3, $4)" js_drawArrays :: JS_WebGLContext -> GLenum -> GLint -> GLsizei -> IO ()
foreign import javascript unsafe "$1.drawElements($2, $3, $4, $5)" js_drawElements :: JS_WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> IO ()
foreign import javascript unsafe "$1.drawArraysInstanced($2, $3, $4, $5)" js_drawArraysInstanced :: JS_WebGLContext -> GLenum -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "$1.drawElementsInstanced($2, $3, $4, $5, $6)" js_drawElementsInstanced :: JS_WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> GLsizei -> IO ()

-- Helpers.

foreign import javascript interruptible "var image=new Image();image.onload=function(){$c(image);};image.src=$1;" js_loadImage :: JSString -> IO JSVal

-- Constants.

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

-- AlphaFunction (not supported in ES20)
--      NEVER
--      LESS
--      EQUAL
--      LEQUAL
--      GREATER
--      NOTEQUAL
--      GEQUAL
--      ALWAYS

-- BlendingFactorDest
pattern GL_ZERO = 0
pattern GL_ONE = 1
pattern GL_SRC_COLOR = 0x0300
pattern GL_ONE_MINUS_SRC_COLOR = 0x0301
pattern GL_SRC_ALPHA = 0x0302
pattern GL_ONE_MINUS_SRC_ALPHA = 0x0303
pattern GL_DST_ALPHA = 0x0304
pattern GL_ONE_MINUS_DST_ALPHA = 0x0305

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

-- DepthFunction
--      NEVER
--      LESS
--      EQUAL
--      LEQUAL
--      GREATER
--      NOTEQUAL
--      GEQUAL
--      ALWAYS

-- EnableCap
-- TEXTURE_2D
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
pattern GL_UNPACK_ALIGNMENT = 0x0CF5
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

-- GetTextureParameter
--      TEXTURE_MAG_FILTER
--      TEXTURE_MIN_FILTER
--      TEXTURE_WRAP_S
--      TEXTURE_WRAP_T

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

-- PixelFormat
pattern GL_DEPTH_COMPONENT = 0x1902
pattern GL_ALPHA = 0x1906
pattern GL_RGB = 0x1907
pattern GL_RGBA = 0x1908
pattern GL_LUMINANCE = 0x1909
pattern GL_LUMINANCE_ALPHA = 0x190A

-- PixelType
--      UNSIGNED_BYTE
pattern GL_UNSIGNED_SHORT_4_4_4_4 = 0x8033
pattern GL_UNSIGNED_SHORT_5_5_5_1 = 0x8034
pattern GL_UNSIGNED_SHORT_5_6_5 = 0x8363

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

-- StencilFunction
pattern GL_NEVER = 0x0200
pattern GL_LESS = 0x0201
pattern GL_EQUAL = 0x0202
pattern GL_LEQUAL = 0x0203
pattern GL_GREATER = 0x0204
pattern GL_NOTEQUAL = 0x0205
pattern GL_GEQUAL = 0x0206
pattern GL_ALWAYS = 0x0207

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
pattern GL_NEAREST = 0x2600
pattern GL_LINEAR = 0x2601

-- TextureMinFilter
--      NEAREST
--      LINEAR
pattern GL_NEAREST_MIPMAP_NEAREST = 0x2700
pattern GL_LINEAR_MIPMAP_NEAREST = 0x2701
pattern GL_NEAREST_MIPMAP_LINEAR = 0x2702
pattern GL_LINEAR_MIPMAP_LINEAR = 0x2703

-- TextureParameterName
pattern GL_TEXTURE_MAG_FILTER = 0x2800
pattern GL_TEXTURE_MIN_FILTER = 0x2801
pattern GL_TEXTURE_WRAP_S = 0x2802
pattern GL_TEXTURE_WRAP_T = 0x2803

-- TextureTarget
pattern GL_TEXTURE_2D = 0x0DE1
pattern GL_TEXTURE = 0x1702

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
pattern GL_CLAMP_TO_EDGE = 0x812F
pattern GL_MIRRORED_REPEAT = 0x8370

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
