{-|
Module: Flaw.Graphics.WebGL.FFI
Description: Bindings for WebGL API.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI #-}

module Flaw.Graphics.WebGL.FFI
	( JS_WebGLContext
	, JS_WebGLTexture
	, JS_WebGLRenderbuffer
	, JS_WebGLFramebuffer
	, JS_WebGLBuffer
	, JS_WebGLProgram
	, JS_WebGLShader
	, JS_WebGLUniformLocation
	, GLenum
	, GLint
	, GLuint
	, GLsizei
	, GLintptr
	, GLclampf
	, GLboolean
	, GLbitfield
	, js_getWebGLContext
	, js_frontFace
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
	, js_clearColor
	, js_clearDepth
	, js_clearStencil
	, js_clear
	, js_viewport
	, js_drawArrays
	, js_drawElements
	, js_loadImage
	, webgl_DEPTH_BUFFER_BIT
	, webgl_STENCIL_BUFFER_BIT
	, webgl_COLOR_BUFFER_BIT
	, webgl_POINTS
	, webgl_LINES
	, webgl_LINE_LOOP
	, webgl_LINE_STRIP
	, webgl_TRIANGLES
	, webgl_TRIANGLE_STRIP
	, webgl_TRIANGLE_FAN
	, webgl_ZERO
	, webgl_ONE
	, webgl_SRC_COLOR
	, webgl_ONE_MINUS_SRC_COLOR
	, webgl_SRC_ALPHA
	, webgl_ONE_MINUS_SRC_ALPHA
	, webgl_DST_ALPHA
	, webgl_ONE_MINUS_DST_ALPHA
	, webgl_DST_COLOR
	, webgl_ONE_MINUS_DST_COLOR
	, webgl_SRC_ALPHA_SATURATE
	, webgl_FUNC_ADD
	, webgl_BLEND_EQUATION
	, webgl_BLEND_EQUATION_RGB
	, webgl_BLEND_EQUATION_ALPHA
	, webgl_FUNC_SUBTRACT
	, webgl_FUNC_REVERSE_SUBTRACT
	, webgl_BLEND_DST_RGB
	, webgl_BLEND_SRC_RGB
	, webgl_BLEND_DST_ALPHA
	, webgl_BLEND_SRC_ALPHA
	, webgl_CONSTANT_COLOR
	, webgl_ONE_MINUS_CONSTANT_COLOR
	, webgl_CONSTANT_ALPHA
	, webgl_ONE_MINUS_CONSTANT_ALPHA
	, webgl_BLEND_COLOR
	, webgl_ARRAY_BUFFER
	, webgl_ELEMENT_ARRAY_BUFFER
	, webgl_ARRAY_BUFFER_BINDING
	, webgl_ELEMENT_ARRAY_BUFFER_BINDING
	, webgl_STREAM_DRAW
	, webgl_STATIC_DRAW
	, webgl_DYNAMIC_DRAW
	, webgl_BUFFER_SIZE
	, webgl_BUFFER_USAGE
	, webgl_CURRENT_VERTEX_ATTRIB
	, webgl_FRONT
	, webgl_BACK
	, webgl_FRONT_AND_BACK
	, webgl_CULL_FACE
	, webgl_BLEND
	, webgl_DITHER
	, webgl_STENCIL_TEST
	, webgl_DEPTH_TEST
	, webgl_SCISSOR_TEST
	, webgl_POLYGON_OFFSET_FILL
	, webgl_SAMPLE_ALPHA_TO_COVERAGE
	, webgl_SAMPLE_COVERAGE
	, webgl_NO_ERROR
	, webgl_INVALID_ENUM
	, webgl_INVALID_VALUE
	, webgl_INVALID_OPERATION
	, webgl_OUT_OF_MEMORY
	, webgl_CW
	, webgl_CCW
	, webgl_LINE_WIDTH
	, webgl_ALIASED_POINT_SIZE_RANGE
	, webgl_ALIASED_LINE_WIDTH_RANGE
	, webgl_CULL_FACE_MODE
	, webgl_FRONT_FACE
	, webgl_DEPTH_RANGE
	, webgl_DEPTH_WRITEMASK
	, webgl_DEPTH_CLEAR_VALUE
	, webgl_DEPTH_FUNC
	, webgl_STENCIL_CLEAR_VALUE
	, webgl_STENCIL_FUNC
	, webgl_STENCIL_FAIL
	, webgl_STENCIL_PASS_DEPTH_FAIL
	, webgl_STENCIL_PASS_DEPTH_PASS
	, webgl_STENCIL_REF
	, webgl_STENCIL_VALUE_MASK
	, webgl_STENCIL_WRITEMASK
	, webgl_STENCIL_BACK_FUNC
	, webgl_STENCIL_BACK_FAIL
	, webgl_STENCIL_BACK_PASS_DEPTH_FAIL
	, webgl_STENCIL_BACK_PASS_DEPTH_PASS
	, webgl_STENCIL_BACK_REF
	, webgl_STENCIL_BACK_VALUE_MASK
	, webgl_STENCIL_BACK_WRITEMASK
	, webgl_VIEWPORT
	, webgl_SCISSOR_BOX
	, webgl_COLOR_CLEAR_VALUE
	, webgl_COLOR_WRITEMASK
	, webgl_UNPACK_ALIGNMENT
	, webgl_PACK_ALIGNMENT
	, webgl_MAX_TEXTURE_SIZE
	, webgl_MAX_VIEWPORT_DIMS
	, webgl_SUBPIXEL_BITS
	, webgl_RED_BITS
	, webgl_GREEN_BITS
	, webgl_BLUE_BITS
	, webgl_ALPHA_BITS
	, webgl_DEPTH_BITS
	, webgl_STENCIL_BITS
	, webgl_POLYGON_OFFSET_UNITS
	, webgl_POLYGON_OFFSET_FACTOR
	, webgl_TEXTURE_BINDING_2D
	, webgl_SAMPLE_BUFFERS
	, webgl_SAMPLES
	, webgl_SAMPLE_COVERAGE_VALUE
	, webgl_SAMPLE_COVERAGE_INVERT
	, webgl_COMPRESSED_TEXTURE_FORMATS
	, webgl_DONT_CARE
	, webgl_FASTEST
	, webgl_NICEST
	, webgl_GENERATE_MIPMAP_HINT
	, webgl_BYTE
	, webgl_UNSIGNED_BYTE
	, webgl_SHORT
	, webgl_UNSIGNED_SHORT
	, webgl_INT
	, webgl_UNSIGNED_INT
	, webgl_FLOAT
	, webgl_DEPTH_COMPONENT
	, webgl_ALPHA
	, webgl_RGB
	, webgl_RGBA
	, webgl_LUMINANCE
	, webgl_LUMINANCE_ALPHA
	, webgl_UNSIGNED_SHORT_4_4_4_4
	, webgl_UNSIGNED_SHORT_5_5_5_1
	, webgl_UNSIGNED_SHORT_5_6_5
	, webgl_FRAGMENT_SHADER
	, webgl_VERTEX_SHADER
	, webgl_MAX_VERTEX_ATTRIBS
	, webgl_MAX_VERTEX_UNIFORM_VECTORS
	, webgl_MAX_VARYING_VECTORS
	, webgl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	, webgl_MAX_VERTEX_TEXTURE_IMAGE_UNITS
	, webgl_MAX_TEXTURE_IMAGE_UNITS
	, webgl_MAX_FRAGMENT_UNIFORM_VECTORS
	, webgl_SHADER_TYPE
	, webgl_DELETE_STATUS
	, webgl_LINK_STATUS
	, webgl_VALIDATE_STATUS
	, webgl_ATTACHED_SHADERS
	, webgl_ACTIVE_UNIFORMS
	, webgl_ACTIVE_ATTRIBUTES
	, webgl_SHADING_LANGUAGE_VERSION
	, webgl_CURRENT_PROGRAM
	, webgl_NEVER
	, webgl_LESS
	, webgl_EQUAL
	, webgl_LEQUAL
	, webgl_GREATER
	, webgl_NOTEQUAL
	, webgl_GEQUAL
	, webgl_ALWAYS
	, webgl_KEEP
	, webgl_REPLACE
	, webgl_INCR
	, webgl_DECR
	, webgl_INVERT
	, webgl_INCR_WRAP
	, webgl_DECR_WRAP
	, webgl_VENDOR
	, webgl_RENDERER
	, webgl_VERSION
	, webgl_NEAREST
	, webgl_LINEAR
	, webgl_NEAREST_MIPMAP_NEAREST
	, webgl_LINEAR_MIPMAP_NEAREST
	, webgl_NEAREST_MIPMAP_LINEAR
	, webgl_LINEAR_MIPMAP_LINEAR
	, webgl_TEXTURE_MAG_FILTER
	, webgl_TEXTURE_MIN_FILTER
	, webgl_TEXTURE_WRAP_S
	, webgl_TEXTURE_WRAP_T
	, webgl_TEXTURE_2D
	, webgl_TEXTURE
	, webgl_TEXTURE_CUBE_MAP
	, webgl_TEXTURE_BINDING_CUBE_MAP
	, webgl_TEXTURE_CUBE_MAP_POSITIVE_X
	, webgl_TEXTURE_CUBE_MAP_NEGATIVE_X
	, webgl_TEXTURE_CUBE_MAP_POSITIVE_Y
	, webgl_TEXTURE_CUBE_MAP_NEGATIVE_Y
	, webgl_TEXTURE_CUBE_MAP_POSITIVE_Z
	, webgl_TEXTURE_CUBE_MAP_NEGATIVE_Z
	, webgl_MAX_CUBE_MAP_TEXTURE_SIZE
	, webgl_TEXTURE0
	, webgl_TEXTURE1
	, webgl_TEXTURE2
	, webgl_TEXTURE3
	, webgl_TEXTURE4
	, webgl_TEXTURE5
	, webgl_TEXTURE6
	, webgl_TEXTURE7
	, webgl_TEXTURE8
	, webgl_TEXTURE9
	, webgl_TEXTURE10
	, webgl_TEXTURE11
	, webgl_TEXTURE12
	, webgl_TEXTURE13
	, webgl_TEXTURE14
	, webgl_TEXTURE15
	, webgl_TEXTURE16
	, webgl_TEXTURE17
	, webgl_TEXTURE18
	, webgl_TEXTURE19
	, webgl_TEXTURE20
	, webgl_TEXTURE21
	, webgl_TEXTURE22
	, webgl_TEXTURE23
	, webgl_TEXTURE24
	, webgl_TEXTURE25
	, webgl_TEXTURE26
	, webgl_TEXTURE27
	, webgl_TEXTURE28
	, webgl_TEXTURE29
	, webgl_TEXTURE30
	, webgl_TEXTURE31
	, webgl_ACTIVE_TEXTURE
	, webgl_REPEAT
	, webgl_CLAMP_TO_EDGE
	, webgl_MIRRORED_REPEAT
	, webgl_FLOAT_VEC2
	, webgl_FLOAT_VEC3
	, webgl_FLOAT_VEC4
	, webgl_INT_VEC2
	, webgl_INT_VEC3
	, webgl_INT_VEC4
	, webgl_BOOL
	, webgl_BOOL_VEC2
	, webgl_BOOL_VEC3
	, webgl_BOOL_VEC4
	, webgl_FLOAT_MAT2
	, webgl_FLOAT_MAT3
	, webgl_FLOAT_MAT4
	, webgl_SAMPLER_2D
	, webgl_SAMPLER_CUBE
	, webgl_VERTEX_ATTRIB_ARRAY_ENABLED
	, webgl_VERTEX_ATTRIB_ARRAY_SIZE
	, webgl_VERTEX_ATTRIB_ARRAY_STRIDE
	, webgl_VERTEX_ATTRIB_ARRAY_TYPE
	, webgl_VERTEX_ATTRIB_ARRAY_NORMALIZED
	, webgl_VERTEX_ATTRIB_ARRAY_POINTER
	, webgl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
	, webgl_IMPLEMENTATION_COLOR_READ_TYPE
	, webgl_IMPLEMENTATION_COLOR_READ_FORMAT
	, webgl_COMPILE_STATUS
	, webgl_LOW_FLOAT
	, webgl_MEDIUM_FLOAT
	, webgl_HIGH_FLOAT
	, webgl_LOW_INT
	, webgl_MEDIUM_INT
	, webgl_HIGH_INT
	, webgl_FRAMEBUFFER
	, webgl_RENDERBUFFER
	, webgl_RGBA4
	, webgl_RGB5_A1
	, webgl_RGB565
	, webgl_DEPTH_COMPONENT16
	, webgl_STENCIL_INDEX
	, webgl_STENCIL_INDEX8
	, webgl_DEPTH_STENCIL
	, webgl_RENDERBUFFER_WIDTH
	, webgl_RENDERBUFFER_HEIGHT
	, webgl_RENDERBUFFER_INTERNAL_FORMAT
	, webgl_RENDERBUFFER_RED_SIZE
	, webgl_RENDERBUFFER_GREEN_SIZE
	, webgl_RENDERBUFFER_BLUE_SIZE
	, webgl_RENDERBUFFER_ALPHA_SIZE
	, webgl_RENDERBUFFER_DEPTH_SIZE
	, webgl_RENDERBUFFER_STENCIL_SIZE
	, webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
	, webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
	, webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
	, webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
	, webgl_COLOR_ATTACHMENT0
	, webgl_DEPTH_ATTACHMENT
	, webgl_STENCIL_ATTACHMENT
	, webgl_DEPTH_STENCIL_ATTACHMENT
	, webgl_NONE
	, webgl_FRAMEBUFFER_COMPLETE
	, webgl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
	, webgl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
	, webgl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
	, webgl_FRAMEBUFFER_UNSUPPORTED
	, webgl_FRAMEBUFFER_BINDING
	, webgl_RENDERBUFFER_BINDING
	, webgl_MAX_RENDERBUFFER_SIZE
	, webgl_INVALID_FRAMEBUFFER_OPERATION
	, webgl_UNPACK_FLIP_Y_WEBGL
	, webgl_UNPACK_PREMULTIPLY_ALPHA_WEBGL
	, webgl_CONTEXT_LOST_WEBGL
	, webgl_UNPACK_COLORSPACE_CONVERSION_WEBGL
	, webgl_BROWSER_DEFAULT_WEBGL
	) where

import qualified GHCJS.DOM.Element as DOM
import GHCJS.Types

-- | Placeholders for WebGL javascript types.

data JS_WebGLContext
data JS_WebGLTexture
data JS_WebGLRenderbuffer
data JS_WebGLFramebuffer
data JS_WebGLBuffer
data JS_WebGLProgram
data JS_WebGLShader
data JS_WebGLUniformLocation

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
	\" js_getWebGLContext :: JSRef DOM.Element -> Bool -> IO (JSRef JS_WebGLContext)

foreign import javascript unsafe "$1.frontFace($2)" js_frontFace :: JSRef JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.enable($2)" js_enable :: JSRef JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.disable($2)" js_disable :: JSRef JS_WebGLContext -> GLenum -> IO ()

foreign import javascript unsafe "$1.createTexture()" js_createTexture :: JSRef JS_WebGLContext -> IO (JSRef JS_WebGLTexture)
foreign import javascript unsafe "$1.activeTexture($2)" js_activeTexture :: JSRef JS_WebGLContext -> GLenum -> IO ()
foreign import javascript unsafe "$1.bindTexture($2, $3)" js_bindTexture :: JSRef JS_WebGLContext -> GLenum -> JSRef JS_WebGLTexture -> IO ()
foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7)" js_texImage2D :: JSRef JS_WebGLContext -> GLenum -> GLint -> GLenum -> GLenum -> GLenum -> JSRef () -> IO ()
foreign import javascript unsafe "$1.texParameterf($2, $3, $4)" js_texParameterf :: JSRef JS_WebGLContext -> GLenum -> GLenum -> GLfloat -> IO ()
foreign import javascript unsafe "$1.texParameteri($2, $3, $4)" js_texParameteri :: JSRef JS_WebGLContext -> GLenum -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1.createBuffer()" js_createBuffer :: JSRef JS_WebGLContext -> IO (JSRef JS_WebGLBuffer)
foreign import javascript unsafe "$1.bindBuffer($2, $3)" js_bindBuffer :: JSRef JS_WebGLContext -> GLenum -> JSRef JS_WebGLBuffer -> IO ()
foreign import javascript unsafe "$1.bufferData($2, $3, $4)" js_bufferData :: JSRef JS_WebGLContext -> GLenum -> JSRef () -> GLenum -> IO ()

foreign import javascript unsafe "$1.enableVertexAttribArray($2)" js_enableVertexAttribArray :: JSRef JS_WebGLContext -> GLuint -> IO ()
foreign import javascript unsafe "$1.disableVertexAttribArray($2)" js_disableVertexAttribArray :: JSRef JS_WebGLContext -> GLuint -> IO ()
foreign import javascript unsafe "$1.vertexAttribPointer($2, $3, $4, $5, $6, $7)" js_vertexAttribPointer :: JSRef JS_WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()

foreign import javascript unsafe "$1.createShader($2)" js_createShader :: JSRef JS_WebGLContext -> GLenum -> IO (JSRef JS_WebGLShader)
foreign import javascript unsafe "$1.shaderSource($2, $3)" js_shaderSource :: JSRef JS_WebGLContext -> JSRef JS_WebGLShader -> JSString -> IO ()
foreign import javascript unsafe "$1.compileShader($2)" js_compileShader :: JSRef JS_WebGLContext -> JSRef JS_WebGLShader -> IO ()
foreign import javascript unsafe "$1.getShaderParameter($2, $3)" js_getShaderParameter :: JSRef JS_WebGLContext -> JSRef JS_WebGLShader -> GLenum -> IO (JSRef ())
foreign import javascript unsafe "$1.getShaderInfoLog($2)" js_getShaderInfoLog :: JSRef JS_WebGLContext -> JSRef JS_WebGLShader -> IO JSString
foreign import javascript unsafe "$1.createProgram()" js_createProgram :: JSRef JS_WebGLContext -> IO (JSRef JS_WebGLProgram)
foreign import javascript unsafe "$1.attachShader($2, $3)" js_attachShader :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> JSRef JS_WebGLShader -> IO ()
foreign import javascript unsafe "$1.bindAttribLocation($2, $3, $4)" js_bindAttribLocation :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> GLuint -> JSString -> IO ()
foreign import javascript unsafe "$1.linkProgram($2)" js_linkProgram :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> IO ()
foreign import javascript unsafe "$1.getProgramParameter($2, $3)" js_getProgramParameter :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> GLenum -> IO (JSRef ())
foreign import javascript unsafe "$1.useProgram($2)" js_useProgram :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> IO ()
foreign import javascript unsafe "$1.getUniformLocation($2, $3)" js_getUniformLocation :: JSRef JS_WebGLContext -> JSRef JS_WebGLProgram -> JSString -> IO (JSRef JS_WebGLUniformLocation)

foreign import javascript unsafe "$1.uniform1f($2, $3)" js_uniform1f :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniform1i($2, $3)" js_uniform1i :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLint -> IO ()
foreign import javascript unsafe "$1.uniform1fv($2, [$3])" js_uniform1fv :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniform2fv($2, [$3, $4])" js_uniform2fv :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLfloat -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniform3fv($2, [$3, $4, $5])" js_uniform3fv :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniform4fv($2, [$3, $4, $5, $6])" js_uniform4fv :: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import javascript unsafe "$1.uniformMatrix4fv($2, false, [$3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18])" js_uniformMatrix4fv
	:: JSRef JS_WebGLContext -> JSRef JS_WebGLUniformLocation
	-> GLfloat -> GLfloat -> GLfloat -> GLfloat
	-> GLfloat -> GLfloat -> GLfloat -> GLfloat
	-> GLfloat -> GLfloat -> GLfloat -> GLfloat
	-> GLfloat -> GLfloat -> GLfloat -> GLfloat
	-> IO ()

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)" js_clearColor :: JSRef JS_WebGLContext -> GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ()
foreign import javascript unsafe "$1.clearDepth($2)" js_clearDepth :: JSRef JS_WebGLContext -> GLclampf -> IO ()
foreign import javascript unsafe "$1.clearStencil($2)" js_clearStencil :: JSRef JS_WebGLContext -> GLint -> IO ()
foreign import javascript unsafe "$1.clear($2)" js_clear :: JSRef JS_WebGLContext -> GLbitfield -> IO ()

foreign import javascript unsafe "$1.viewport($2, $3, $4, $5)" js_viewport :: JSRef JS_WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import javascript unsafe "$1.drawArrays($2, $3, $4)" js_drawArrays :: JSRef JS_WebGLContext -> GLenum -> GLint -> GLsizei -> IO ()
foreign import javascript unsafe "$1.drawElements($2, $3, $4, $5)" js_drawElements :: JSRef JS_WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> IO ()

-- Helpers.

foreign import javascript interruptible "var image=new Image();image.onload=function(){$c(image);};image.src=$1;" js_loadImage :: JSString -> IO (JSRef ())

-- Constants.

-- ClearBufferMask
webgl_DEPTH_BUFFER_BIT :: GLenum
webgl_DEPTH_BUFFER_BIT = 0x00000100
webgl_STENCIL_BUFFER_BIT :: GLenum
webgl_STENCIL_BUFFER_BIT = 0x00000400
webgl_COLOR_BUFFER_BIT :: GLenum
webgl_COLOR_BUFFER_BIT = 0x00004000

-- BeginMode
webgl_POINTS :: GLenum
webgl_POINTS = 0x0000
webgl_LINES :: GLenum
webgl_LINES = 0x0001
webgl_LINE_LOOP :: GLenum
webgl_LINE_LOOP = 0x0002
webgl_LINE_STRIP :: GLenum
webgl_LINE_STRIP = 0x0003
webgl_TRIANGLES :: GLenum
webgl_TRIANGLES = 0x0004
webgl_TRIANGLE_STRIP :: GLenum
webgl_TRIANGLE_STRIP = 0x0005
webgl_TRIANGLE_FAN :: GLenum
webgl_TRIANGLE_FAN = 0x0006

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
webgl_ZERO :: GLenum
webgl_ZERO = 0
webgl_ONE :: GLenum
webgl_ONE = 1
webgl_SRC_COLOR :: GLenum
webgl_SRC_COLOR = 0x0300
webgl_ONE_MINUS_SRC_COLOR :: GLenum
webgl_ONE_MINUS_SRC_COLOR = 0x0301
webgl_SRC_ALPHA :: GLenum
webgl_SRC_ALPHA = 0x0302
webgl_ONE_MINUS_SRC_ALPHA :: GLenum
webgl_ONE_MINUS_SRC_ALPHA = 0x0303
webgl_DST_ALPHA :: GLenum
webgl_DST_ALPHA = 0x0304
webgl_ONE_MINUS_DST_ALPHA :: GLenum
webgl_ONE_MINUS_DST_ALPHA = 0x0305

-- BlendingFactorSrc
--      ZERO
--      ONE
webgl_DST_COLOR :: GLenum
webgl_DST_COLOR = 0x0306
webgl_ONE_MINUS_DST_COLOR :: GLenum
webgl_ONE_MINUS_DST_COLOR = 0x0307
webgl_SRC_ALPHA_SATURATE :: GLenum
webgl_SRC_ALPHA_SATURATE = 0x0308
--      SRC_ALPHA
--      ONE_MINUS_SRC_ALPHA
--      DST_ALPHA
--      ONE_MINUS_DST_ALPHA

-- BlendEquationSeparate
webgl_FUNC_ADD :: GLenum
webgl_FUNC_ADD = 0x8006
webgl_BLEND_EQUATION :: GLenum
webgl_BLEND_EQUATION = 0x8009
webgl_BLEND_EQUATION_RGB :: GLenum
webgl_BLEND_EQUATION_RGB = 0x8009
webgl_BLEND_EQUATION_ALPHA :: GLenum
webgl_BLEND_EQUATION_ALPHA = 0x883D

-- BlendSubtract
webgl_FUNC_SUBTRACT :: GLenum
webgl_FUNC_SUBTRACT = 0x800A
webgl_FUNC_REVERSE_SUBTRACT :: GLenum
webgl_FUNC_REVERSE_SUBTRACT = 0x800B

-- Separate Blend Functions
webgl_BLEND_DST_RGB :: GLenum
webgl_BLEND_DST_RGB = 0x80C8
webgl_BLEND_SRC_RGB :: GLenum
webgl_BLEND_SRC_RGB = 0x80C9
webgl_BLEND_DST_ALPHA :: GLenum
webgl_BLEND_DST_ALPHA = 0x80CA
webgl_BLEND_SRC_ALPHA :: GLenum
webgl_BLEND_SRC_ALPHA = 0x80CB
webgl_CONSTANT_COLOR :: GLenum
webgl_CONSTANT_COLOR = 0x8001
webgl_ONE_MINUS_CONSTANT_COLOR :: GLenum
webgl_ONE_MINUS_CONSTANT_COLOR = 0x8002
webgl_CONSTANT_ALPHA :: GLenum
webgl_CONSTANT_ALPHA = 0x8003
webgl_ONE_MINUS_CONSTANT_ALPHA :: GLenum
webgl_ONE_MINUS_CONSTANT_ALPHA = 0x8004
webgl_BLEND_COLOR :: GLenum
webgl_BLEND_COLOR = 0x8005

-- Buffer Objects
webgl_ARRAY_BUFFER :: GLenum
webgl_ARRAY_BUFFER = 0x8892
webgl_ELEMENT_ARRAY_BUFFER :: GLenum
webgl_ELEMENT_ARRAY_BUFFER = 0x8893
webgl_ARRAY_BUFFER_BINDING :: GLenum
webgl_ARRAY_BUFFER_BINDING = 0x8894
webgl_ELEMENT_ARRAY_BUFFER_BINDING :: GLenum
webgl_ELEMENT_ARRAY_BUFFER_BINDING = 0x8895

webgl_STREAM_DRAW :: GLenum
webgl_STREAM_DRAW = 0x88E0
webgl_STATIC_DRAW :: GLenum
webgl_STATIC_DRAW = 0x88E4
webgl_DYNAMIC_DRAW :: GLenum
webgl_DYNAMIC_DRAW = 0x88E8

webgl_BUFFER_SIZE :: GLenum
webgl_BUFFER_SIZE = 0x8764
webgl_BUFFER_USAGE :: GLenum
webgl_BUFFER_USAGE = 0x8765

webgl_CURRENT_VERTEX_ATTRIB :: GLenum
webgl_CURRENT_VERTEX_ATTRIB = 0x8626

-- CullFaceMode
webgl_FRONT :: GLenum
webgl_FRONT = 0x0404
webgl_BACK :: GLenum
webgl_BACK = 0x0405
webgl_FRONT_AND_BACK :: GLenum
webgl_FRONT_AND_BACK = 0x0408

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
webgl_CULL_FACE :: GLenum
webgl_CULL_FACE = 0x0B44
webgl_BLEND :: GLenum
webgl_BLEND = 0x0BE2
webgl_DITHER :: GLenum
webgl_DITHER = 0x0BD0
webgl_STENCIL_TEST :: GLenum
webgl_STENCIL_TEST = 0x0B90
webgl_DEPTH_TEST :: GLenum
webgl_DEPTH_TEST = 0x0B71
webgl_SCISSOR_TEST :: GLenum
webgl_SCISSOR_TEST = 0x0C11
webgl_POLYGON_OFFSET_FILL :: GLenum
webgl_POLYGON_OFFSET_FILL = 0x8037
webgl_SAMPLE_ALPHA_TO_COVERAGE :: GLenum
webgl_SAMPLE_ALPHA_TO_COVERAGE = 0x809E
webgl_SAMPLE_COVERAGE :: GLenum
webgl_SAMPLE_COVERAGE = 0x80A0

-- ErrorCode
webgl_NO_ERROR :: GLenum
webgl_NO_ERROR = 0
webgl_INVALID_ENUM :: GLenum
webgl_INVALID_ENUM = 0x0500
webgl_INVALID_VALUE :: GLenum
webgl_INVALID_VALUE = 0x0501
webgl_INVALID_OPERATION :: GLenum
webgl_INVALID_OPERATION = 0x0502
webgl_OUT_OF_MEMORY :: GLenum
webgl_OUT_OF_MEMORY = 0x0505

-- FrontFaceDirection
webgl_CW :: GLenum
webgl_CW = 0x0900
webgl_CCW :: GLenum
webgl_CCW = 0x0901

-- GetPName
webgl_LINE_WIDTH :: GLenum
webgl_LINE_WIDTH = 0x0B21
webgl_ALIASED_POINT_SIZE_RANGE :: GLenum
webgl_ALIASED_POINT_SIZE_RANGE = 0x846D
webgl_ALIASED_LINE_WIDTH_RANGE :: GLenum
webgl_ALIASED_LINE_WIDTH_RANGE = 0x846E
webgl_CULL_FACE_MODE :: GLenum
webgl_CULL_FACE_MODE = 0x0B45
webgl_FRONT_FACE :: GLenum
webgl_FRONT_FACE = 0x0B46
webgl_DEPTH_RANGE :: GLenum
webgl_DEPTH_RANGE = 0x0B70
webgl_DEPTH_WRITEMASK :: GLenum
webgl_DEPTH_WRITEMASK = 0x0B72
webgl_DEPTH_CLEAR_VALUE :: GLenum
webgl_DEPTH_CLEAR_VALUE = 0x0B73
webgl_DEPTH_FUNC :: GLenum
webgl_DEPTH_FUNC = 0x0B74
webgl_STENCIL_CLEAR_VALUE :: GLenum
webgl_STENCIL_CLEAR_VALUE = 0x0B91
webgl_STENCIL_FUNC :: GLenum
webgl_STENCIL_FUNC = 0x0B92
webgl_STENCIL_FAIL :: GLenum
webgl_STENCIL_FAIL = 0x0B94
webgl_STENCIL_PASS_DEPTH_FAIL :: GLenum
webgl_STENCIL_PASS_DEPTH_FAIL = 0x0B95
webgl_STENCIL_PASS_DEPTH_PASS :: GLenum
webgl_STENCIL_PASS_DEPTH_PASS = 0x0B96
webgl_STENCIL_REF :: GLenum
webgl_STENCIL_REF = 0x0B97
webgl_STENCIL_VALUE_MASK :: GLenum
webgl_STENCIL_VALUE_MASK = 0x0B93
webgl_STENCIL_WRITEMASK :: GLenum
webgl_STENCIL_WRITEMASK = 0x0B98
webgl_STENCIL_BACK_FUNC :: GLenum
webgl_STENCIL_BACK_FUNC = 0x8800
webgl_STENCIL_BACK_FAIL :: GLenum
webgl_STENCIL_BACK_FAIL = 0x8801
webgl_STENCIL_BACK_PASS_DEPTH_FAIL :: GLenum
webgl_STENCIL_BACK_PASS_DEPTH_FAIL = 0x8802
webgl_STENCIL_BACK_PASS_DEPTH_PASS :: GLenum
webgl_STENCIL_BACK_PASS_DEPTH_PASS = 0x8803
webgl_STENCIL_BACK_REF :: GLenum
webgl_STENCIL_BACK_REF = 0x8CA3
webgl_STENCIL_BACK_VALUE_MASK :: GLenum
webgl_STENCIL_BACK_VALUE_MASK = 0x8CA4
webgl_STENCIL_BACK_WRITEMASK :: GLenum
webgl_STENCIL_BACK_WRITEMASK = 0x8CA5
webgl_VIEWPORT :: GLenum
webgl_VIEWPORT = 0x0BA2
webgl_SCISSOR_BOX :: GLenum
webgl_SCISSOR_BOX = 0x0C10
--      SCISSOR_TEST
webgl_COLOR_CLEAR_VALUE :: GLenum
webgl_COLOR_CLEAR_VALUE = 0x0C22
webgl_COLOR_WRITEMASK :: GLenum
webgl_COLOR_WRITEMASK = 0x0C23
webgl_UNPACK_ALIGNMENT :: GLenum
webgl_UNPACK_ALIGNMENT = 0x0CF5
webgl_PACK_ALIGNMENT :: GLenum
webgl_PACK_ALIGNMENT = 0x0D05
webgl_MAX_TEXTURE_SIZE :: GLenum
webgl_MAX_TEXTURE_SIZE = 0x0D33
webgl_MAX_VIEWPORT_DIMS :: GLenum
webgl_MAX_VIEWPORT_DIMS = 0x0D3A
webgl_SUBPIXEL_BITS :: GLenum
webgl_SUBPIXEL_BITS = 0x0D50
webgl_RED_BITS :: GLenum
webgl_RED_BITS = 0x0D52
webgl_GREEN_BITS :: GLenum
webgl_GREEN_BITS = 0x0D53
webgl_BLUE_BITS :: GLenum
webgl_BLUE_BITS = 0x0D54
webgl_ALPHA_BITS :: GLenum
webgl_ALPHA_BITS = 0x0D55
webgl_DEPTH_BITS :: GLenum
webgl_DEPTH_BITS = 0x0D56
webgl_STENCIL_BITS :: GLenum
webgl_STENCIL_BITS = 0x0D57
webgl_POLYGON_OFFSET_UNITS :: GLenum
webgl_POLYGON_OFFSET_UNITS = 0x2A00
--      POLYGON_OFFSET_FILL
webgl_POLYGON_OFFSET_FACTOR :: GLenum
webgl_POLYGON_OFFSET_FACTOR = 0x8038
webgl_TEXTURE_BINDING_2D :: GLenum
webgl_TEXTURE_BINDING_2D = 0x8069
webgl_SAMPLE_BUFFERS :: GLenum
webgl_SAMPLE_BUFFERS = 0x80A8
webgl_SAMPLES :: GLenum
webgl_SAMPLES = 0x80A9
webgl_SAMPLE_COVERAGE_VALUE :: GLenum
webgl_SAMPLE_COVERAGE_VALUE = 0x80AA
webgl_SAMPLE_COVERAGE_INVERT :: GLenum
webgl_SAMPLE_COVERAGE_INVERT = 0x80AB

-- GetTextureParameter
--      TEXTURE_MAG_FILTER
--      TEXTURE_MIN_FILTER
--      TEXTURE_WRAP_S
--      TEXTURE_WRAP_T

webgl_COMPRESSED_TEXTURE_FORMATS :: GLenum
webgl_COMPRESSED_TEXTURE_FORMATS = 0x86A3

-- HintMode
webgl_DONT_CARE :: GLenum
webgl_DONT_CARE = 0x1100
webgl_FASTEST :: GLenum
webgl_FASTEST = 0x1101
webgl_NICEST :: GLenum
webgl_NICEST = 0x1102

-- HintTarget
webgl_GENERATE_MIPMAP_HINT :: GLenum
webgl_GENERATE_MIPMAP_HINT = 0x8192

-- DataType
webgl_BYTE :: GLenum
webgl_BYTE = 0x1400
webgl_UNSIGNED_BYTE :: GLenum
webgl_UNSIGNED_BYTE = 0x1401
webgl_SHORT :: GLenum
webgl_SHORT = 0x1402
webgl_UNSIGNED_SHORT :: GLenum
webgl_UNSIGNED_SHORT = 0x1403
webgl_INT :: GLenum
webgl_INT = 0x1404
webgl_UNSIGNED_INT :: GLenum
webgl_UNSIGNED_INT = 0x1405
webgl_FLOAT :: GLenum
webgl_FLOAT = 0x1406

-- PixelFormat
webgl_DEPTH_COMPONENT :: GLenum
webgl_DEPTH_COMPONENT = 0x1902
webgl_ALPHA :: GLenum
webgl_ALPHA = 0x1906
webgl_RGB :: GLenum
webgl_RGB = 0x1907
webgl_RGBA :: GLenum
webgl_RGBA = 0x1908
webgl_LUMINANCE :: GLenum
webgl_LUMINANCE = 0x1909
webgl_LUMINANCE_ALPHA :: GLenum
webgl_LUMINANCE_ALPHA = 0x190A

-- PixelType
--      UNSIGNED_BYTE
webgl_UNSIGNED_SHORT_4_4_4_4 :: GLenum
webgl_UNSIGNED_SHORT_4_4_4_4 = 0x8033
webgl_UNSIGNED_SHORT_5_5_5_1 :: GLenum
webgl_UNSIGNED_SHORT_5_5_5_1 = 0x8034
webgl_UNSIGNED_SHORT_5_6_5 :: GLenum
webgl_UNSIGNED_SHORT_5_6_5 = 0x8363

-- Shaders
webgl_FRAGMENT_SHADER :: GLenum
webgl_FRAGMENT_SHADER = 0x8B30
webgl_VERTEX_SHADER :: GLenum
webgl_VERTEX_SHADER = 0x8B31
webgl_MAX_VERTEX_ATTRIBS :: GLenum
webgl_MAX_VERTEX_ATTRIBS = 0x8869
webgl_MAX_VERTEX_UNIFORM_VECTORS :: GLenum
webgl_MAX_VERTEX_UNIFORM_VECTORS = 0x8DFB
webgl_MAX_VARYING_VECTORS :: GLenum
webgl_MAX_VARYING_VECTORS = 0x8DFC
webgl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: GLenum
webgl_MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D
webgl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: GLenum
webgl_MAX_VERTEX_TEXTURE_IMAGE_UNITS = 0x8B4C
webgl_MAX_TEXTURE_IMAGE_UNITS :: GLenum
webgl_MAX_TEXTURE_IMAGE_UNITS = 0x8872
webgl_MAX_FRAGMENT_UNIFORM_VECTORS :: GLenum
webgl_MAX_FRAGMENT_UNIFORM_VECTORS = 0x8DFD
webgl_SHADER_TYPE :: GLenum
webgl_SHADER_TYPE = 0x8B4F
webgl_DELETE_STATUS :: GLenum
webgl_DELETE_STATUS = 0x8B80
webgl_LINK_STATUS :: GLenum
webgl_LINK_STATUS = 0x8B82
webgl_VALIDATE_STATUS :: GLenum
webgl_VALIDATE_STATUS = 0x8B83
webgl_ATTACHED_SHADERS :: GLenum
webgl_ATTACHED_SHADERS = 0x8B85
webgl_ACTIVE_UNIFORMS :: GLenum
webgl_ACTIVE_UNIFORMS = 0x8B86
webgl_ACTIVE_ATTRIBUTES :: GLenum
webgl_ACTIVE_ATTRIBUTES = 0x8B89
webgl_SHADING_LANGUAGE_VERSION :: GLenum
webgl_SHADING_LANGUAGE_VERSION = 0x8B8C
webgl_CURRENT_PROGRAM :: GLenum
webgl_CURRENT_PROGRAM = 0x8B8D

-- StencilFunction
webgl_NEVER :: GLenum
webgl_NEVER = 0x0200
webgl_LESS :: GLenum
webgl_LESS = 0x0201
webgl_EQUAL :: GLenum
webgl_EQUAL = 0x0202
webgl_LEQUAL :: GLenum
webgl_LEQUAL = 0x0203
webgl_GREATER :: GLenum
webgl_GREATER = 0x0204
webgl_NOTEQUAL :: GLenum
webgl_NOTEQUAL = 0x0205
webgl_GEQUAL :: GLenum
webgl_GEQUAL = 0x0206
webgl_ALWAYS :: GLenum
webgl_ALWAYS = 0x0207

-- StencilOp
--      ZERO
webgl_KEEP :: GLenum
webgl_KEEP = 0x1E00
webgl_REPLACE :: GLenum
webgl_REPLACE = 0x1E01
webgl_INCR :: GLenum
webgl_INCR = 0x1E02
webgl_DECR :: GLenum
webgl_DECR = 0x1E03
webgl_INVERT :: GLenum
webgl_INVERT = 0x150A
webgl_INCR_WRAP :: GLenum
webgl_INCR_WRAP = 0x8507
webgl_DECR_WRAP :: GLenum
webgl_DECR_WRAP = 0x8508

-- StringName
webgl_VENDOR :: GLenum
webgl_VENDOR = 0x1F00
webgl_RENDERER :: GLenum
webgl_RENDERER = 0x1F01
webgl_VERSION :: GLenum
webgl_VERSION = 0x1F02

-- TextureMagFilter
webgl_NEAREST :: GLenum
webgl_NEAREST = 0x2600
webgl_LINEAR :: GLenum
webgl_LINEAR = 0x2601

-- TextureMinFilter
--      NEAREST
--      LINEAR
webgl_NEAREST_MIPMAP_NEAREST :: GLenum
webgl_NEAREST_MIPMAP_NEAREST = 0x2700
webgl_LINEAR_MIPMAP_NEAREST :: GLenum
webgl_LINEAR_MIPMAP_NEAREST = 0x2701
webgl_NEAREST_MIPMAP_LINEAR :: GLenum
webgl_NEAREST_MIPMAP_LINEAR = 0x2702
webgl_LINEAR_MIPMAP_LINEAR :: GLenum
webgl_LINEAR_MIPMAP_LINEAR = 0x2703

-- TextureParameterName
webgl_TEXTURE_MAG_FILTER :: GLenum
webgl_TEXTURE_MAG_FILTER = 0x2800
webgl_TEXTURE_MIN_FILTER :: GLenum
webgl_TEXTURE_MIN_FILTER = 0x2801
webgl_TEXTURE_WRAP_S :: GLenum
webgl_TEXTURE_WRAP_S = 0x2802
webgl_TEXTURE_WRAP_T :: GLenum
webgl_TEXTURE_WRAP_T = 0x2803

-- TextureTarget
webgl_TEXTURE_2D :: GLenum
webgl_TEXTURE_2D = 0x0DE1
webgl_TEXTURE :: GLenum
webgl_TEXTURE = 0x1702

webgl_TEXTURE_CUBE_MAP :: GLenum
webgl_TEXTURE_CUBE_MAP = 0x8513
webgl_TEXTURE_BINDING_CUBE_MAP :: GLenum
webgl_TEXTURE_BINDING_CUBE_MAP = 0x8514
webgl_TEXTURE_CUBE_MAP_POSITIVE_X :: GLenum
webgl_TEXTURE_CUBE_MAP_POSITIVE_X = 0x8515
webgl_TEXTURE_CUBE_MAP_NEGATIVE_X :: GLenum
webgl_TEXTURE_CUBE_MAP_NEGATIVE_X = 0x8516
webgl_TEXTURE_CUBE_MAP_POSITIVE_Y :: GLenum
webgl_TEXTURE_CUBE_MAP_POSITIVE_Y = 0x8517
webgl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: GLenum
webgl_TEXTURE_CUBE_MAP_NEGATIVE_Y = 0x8518
webgl_TEXTURE_CUBE_MAP_POSITIVE_Z :: GLenum
webgl_TEXTURE_CUBE_MAP_POSITIVE_Z = 0x8519
webgl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: GLenum
webgl_TEXTURE_CUBE_MAP_NEGATIVE_Z = 0x851A
webgl_MAX_CUBE_MAP_TEXTURE_SIZE :: GLenum
webgl_MAX_CUBE_MAP_TEXTURE_SIZE = 0x851C

-- TextureUnit
webgl_TEXTURE0 :: GLenum
webgl_TEXTURE0 = 0x84C0
webgl_TEXTURE1 :: GLenum
webgl_TEXTURE1 = 0x84C1
webgl_TEXTURE2 :: GLenum
webgl_TEXTURE2 = 0x84C2
webgl_TEXTURE3 :: GLenum
webgl_TEXTURE3 = 0x84C3
webgl_TEXTURE4 :: GLenum
webgl_TEXTURE4 = 0x84C4
webgl_TEXTURE5 :: GLenum
webgl_TEXTURE5 = 0x84C5
webgl_TEXTURE6 :: GLenum
webgl_TEXTURE6 = 0x84C6
webgl_TEXTURE7 :: GLenum
webgl_TEXTURE7 = 0x84C7
webgl_TEXTURE8 :: GLenum
webgl_TEXTURE8 = 0x84C8
webgl_TEXTURE9 :: GLenum
webgl_TEXTURE9 = 0x84C9
webgl_TEXTURE10 :: GLenum
webgl_TEXTURE10 = 0x84CA
webgl_TEXTURE11 :: GLenum
webgl_TEXTURE11 = 0x84CB
webgl_TEXTURE12 :: GLenum
webgl_TEXTURE12 = 0x84CC
webgl_TEXTURE13 :: GLenum
webgl_TEXTURE13 = 0x84CD
webgl_TEXTURE14 :: GLenum
webgl_TEXTURE14 = 0x84CE
webgl_TEXTURE15 :: GLenum
webgl_TEXTURE15 = 0x84CF
webgl_TEXTURE16 :: GLenum
webgl_TEXTURE16 = 0x84D0
webgl_TEXTURE17 :: GLenum
webgl_TEXTURE17 = 0x84D1
webgl_TEXTURE18 :: GLenum
webgl_TEXTURE18 = 0x84D2
webgl_TEXTURE19 :: GLenum
webgl_TEXTURE19 = 0x84D3
webgl_TEXTURE20 :: GLenum
webgl_TEXTURE20 = 0x84D4
webgl_TEXTURE21 :: GLenum
webgl_TEXTURE21 = 0x84D5
webgl_TEXTURE22 :: GLenum
webgl_TEXTURE22 = 0x84D6
webgl_TEXTURE23 :: GLenum
webgl_TEXTURE23 = 0x84D7
webgl_TEXTURE24 :: GLenum
webgl_TEXTURE24 = 0x84D8
webgl_TEXTURE25 :: GLenum
webgl_TEXTURE25 = 0x84D9
webgl_TEXTURE26 :: GLenum
webgl_TEXTURE26 = 0x84DA
webgl_TEXTURE27 :: GLenum
webgl_TEXTURE27 = 0x84DB
webgl_TEXTURE28 :: GLenum
webgl_TEXTURE28 = 0x84DC
webgl_TEXTURE29 :: GLenum
webgl_TEXTURE29 = 0x84DD
webgl_TEXTURE30 :: GLenum
webgl_TEXTURE30 = 0x84DE
webgl_TEXTURE31 :: GLenum
webgl_TEXTURE31 = 0x84DF
webgl_ACTIVE_TEXTURE :: GLenum
webgl_ACTIVE_TEXTURE = 0x84E0

-- TextureWrapMode
webgl_REPEAT :: GLenum
webgl_REPEAT = 0x2901
webgl_CLAMP_TO_EDGE :: GLenum
webgl_CLAMP_TO_EDGE = 0x812F
webgl_MIRRORED_REPEAT :: GLenum
webgl_MIRRORED_REPEAT = 0x8370

-- Uniform Types
webgl_FLOAT_VEC2 :: GLenum
webgl_FLOAT_VEC2 = 0x8B50
webgl_FLOAT_VEC3 :: GLenum
webgl_FLOAT_VEC3 = 0x8B51
webgl_FLOAT_VEC4 :: GLenum
webgl_FLOAT_VEC4 = 0x8B52
webgl_INT_VEC2 :: GLenum
webgl_INT_VEC2 = 0x8B53
webgl_INT_VEC3 :: GLenum
webgl_INT_VEC3 = 0x8B54
webgl_INT_VEC4 :: GLenum
webgl_INT_VEC4 = 0x8B55
webgl_BOOL :: GLenum
webgl_BOOL = 0x8B56
webgl_BOOL_VEC2 :: GLenum
webgl_BOOL_VEC2 = 0x8B57
webgl_BOOL_VEC3 :: GLenum
webgl_BOOL_VEC3 = 0x8B58
webgl_BOOL_VEC4 :: GLenum
webgl_BOOL_VEC4 = 0x8B59
webgl_FLOAT_MAT2 :: GLenum
webgl_FLOAT_MAT2 = 0x8B5A
webgl_FLOAT_MAT3 :: GLenum
webgl_FLOAT_MAT3 = 0x8B5B
webgl_FLOAT_MAT4 :: GLenum
webgl_FLOAT_MAT4 = 0x8B5C
webgl_SAMPLER_2D :: GLenum
webgl_SAMPLER_2D = 0x8B5E
webgl_SAMPLER_CUBE :: GLenum
webgl_SAMPLER_CUBE = 0x8B60

-- Vertex Arrays
webgl_VERTEX_ATTRIB_ARRAY_ENABLED :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_ENABLED = 0x8622
webgl_VERTEX_ATTRIB_ARRAY_SIZE :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_SIZE = 0x8623
webgl_VERTEX_ATTRIB_ARRAY_STRIDE :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_STRIDE = 0x8624
webgl_VERTEX_ATTRIB_ARRAY_TYPE :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_TYPE = 0x8625
webgl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_NORMALIZED = 0x886A
webgl_VERTEX_ATTRIB_ARRAY_POINTER :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_POINTER = 0x8645
webgl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: GLenum
webgl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F

-- Read Format
webgl_IMPLEMENTATION_COLOR_READ_TYPE :: GLenum
webgl_IMPLEMENTATION_COLOR_READ_TYPE = 0x8B9A
webgl_IMPLEMENTATION_COLOR_READ_FORMAT :: GLenum
webgl_IMPLEMENTATION_COLOR_READ_FORMAT = 0x8B9B

-- Shader Source
webgl_COMPILE_STATUS :: GLenum
webgl_COMPILE_STATUS = 0x8B81

-- Shader Precision-Specified Types
webgl_LOW_FLOAT :: GLenum
webgl_LOW_FLOAT = 0x8DF0
webgl_MEDIUM_FLOAT :: GLenum
webgl_MEDIUM_FLOAT = 0x8DF1
webgl_HIGH_FLOAT :: GLenum
webgl_HIGH_FLOAT = 0x8DF2
webgl_LOW_INT :: GLenum
webgl_LOW_INT = 0x8DF3
webgl_MEDIUM_INT :: GLenum
webgl_MEDIUM_INT = 0x8DF4
webgl_HIGH_INT :: GLenum
webgl_HIGH_INT = 0x8DF5

-- Framebuffer Object.
webgl_FRAMEBUFFER :: GLenum
webgl_FRAMEBUFFER = 0x8D40
webgl_RENDERBUFFER :: GLenum
webgl_RENDERBUFFER = 0x8D41

webgl_RGBA4 :: GLenum
webgl_RGBA4 = 0x8056
webgl_RGB5_A1 :: GLenum
webgl_RGB5_A1 = 0x8057
webgl_RGB565 :: GLenum
webgl_RGB565 = 0x8D62
webgl_DEPTH_COMPONENT16 :: GLenum
webgl_DEPTH_COMPONENT16 = 0x81A5
webgl_STENCIL_INDEX :: GLenum
webgl_STENCIL_INDEX = 0x1901
webgl_STENCIL_INDEX8 :: GLenum
webgl_STENCIL_INDEX8 = 0x8D48
webgl_DEPTH_STENCIL :: GLenum
webgl_DEPTH_STENCIL = 0x84F9

webgl_RENDERBUFFER_WIDTH :: GLenum
webgl_RENDERBUFFER_WIDTH = 0x8D42
webgl_RENDERBUFFER_HEIGHT :: GLenum
webgl_RENDERBUFFER_HEIGHT = 0x8D43
webgl_RENDERBUFFER_INTERNAL_FORMAT :: GLenum
webgl_RENDERBUFFER_INTERNAL_FORMAT = 0x8D44
webgl_RENDERBUFFER_RED_SIZE :: GLenum
webgl_RENDERBUFFER_RED_SIZE = 0x8D50
webgl_RENDERBUFFER_GREEN_SIZE :: GLenum
webgl_RENDERBUFFER_GREEN_SIZE = 0x8D51
webgl_RENDERBUFFER_BLUE_SIZE :: GLenum
webgl_RENDERBUFFER_BLUE_SIZE = 0x8D52
webgl_RENDERBUFFER_ALPHA_SIZE :: GLenum
webgl_RENDERBUFFER_ALPHA_SIZE = 0x8D53
webgl_RENDERBUFFER_DEPTH_SIZE :: GLenum
webgl_RENDERBUFFER_DEPTH_SIZE = 0x8D54
webgl_RENDERBUFFER_STENCIL_SIZE :: GLenum
webgl_RENDERBUFFER_STENCIL_SIZE = 0x8D55

webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: GLenum
webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = 0x8CD0
webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: GLenum
webgl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = 0x8CD1
webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: GLenum
webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = 0x8CD2
webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: GLenum
webgl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3

webgl_COLOR_ATTACHMENT0 :: GLenum
webgl_COLOR_ATTACHMENT0 = 0x8CE0
webgl_DEPTH_ATTACHMENT :: GLenum
webgl_DEPTH_ATTACHMENT = 0x8D00
webgl_STENCIL_ATTACHMENT :: GLenum
webgl_STENCIL_ATTACHMENT = 0x8D20
webgl_DEPTH_STENCIL_ATTACHMENT :: GLenum
webgl_DEPTH_STENCIL_ATTACHMENT = 0x821A

webgl_NONE :: GLenum
webgl_NONE = 0

webgl_FRAMEBUFFER_COMPLETE :: GLenum
webgl_FRAMEBUFFER_COMPLETE = 0x8CD5
webgl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: GLenum
webgl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 0x8CD6
webgl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: GLenum
webgl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7
webgl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: GLenum
webgl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 0x8CD9
webgl_FRAMEBUFFER_UNSUPPORTED :: GLenum
webgl_FRAMEBUFFER_UNSUPPORTED = 0x8CDD

webgl_FRAMEBUFFER_BINDING :: GLenum
webgl_FRAMEBUFFER_BINDING = 0x8CA6
webgl_RENDERBUFFER_BINDING :: GLenum
webgl_RENDERBUFFER_BINDING = 0x8CA7
webgl_MAX_RENDERBUFFER_SIZE :: GLenum
webgl_MAX_RENDERBUFFER_SIZE = 0x84E8

webgl_INVALID_FRAMEBUFFER_OPERATION :: GLenum
webgl_INVALID_FRAMEBUFFER_OPERATION = 0x0506

-- WebGL-specific enums
webgl_UNPACK_FLIP_Y_WEBGL :: GLenum
webgl_UNPACK_FLIP_Y_WEBGL = 0x9240
webgl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: GLenum
webgl_UNPACK_PREMULTIPLY_ALPHA_WEBGL = 0x9241
webgl_CONTEXT_LOST_WEBGL :: GLenum
webgl_CONTEXT_LOST_WEBGL = 0x9242
webgl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: GLenum
webgl_UNPACK_COLORSPACE_CONVERSION_WEBGL = 0x9243
webgl_BROWSER_DEFAULT_WEBGL :: GLenum
webgl_BROWSER_DEFAULT_WEBGL = 0x9244
