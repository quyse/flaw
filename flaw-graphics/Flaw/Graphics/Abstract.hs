{-|
Module: Flaw.Graphics.Abstract
Description: Abstract graphics interface.
License: MIT
-}

module Flaw.Graphics.Abstract
	( ShaderType(..)
	, VertexLayoutDataType(..)
	, VertexLayoutElement(..)
	, VertexLayoutInfo(..)
	) where

data ShaderType
	= ShaderTypeFloat
	| ShaderTypeVec2f
	| ShaderTypeVec3f
	| ShaderTypeVec4f
	| ShaderTypeMat3x3f
	| ShaderTypeMat4x4f
	| ShaderTypeUint
	| ShaderTypeVec2u
	| ShaderTypeVec3u
	| ShaderTypeVec4u
	| ShaderTypeInt
	| ShaderTypeVec2i
	| ShaderTypeVec3i
	| ShaderTypeVec4i
	| ShaderTypeBool
	| ShaderTypeVec2b
	| ShaderTypeVec3b
	| ShaderTypeVec4b

data VertexLayoutDataType
	= VertexLayoutFloat32
	| VertexLayoutUint32
	| VertexLayoutUint16
	| VertexLayoutUint8
	| VertexLayoutInt32
	| VertexLayoutInt16
	| VertexLayoutInt8

data VertexLayoutElement = VertexLayoutElement
	{ vertexLayoutElementShaderType :: ShaderType
	, vertexLayoutElementLayoutDataType :: VertexLayoutDataType
	, vertexLayoutElementOffset :: Int
	}

data VertexLayoutInfo = VertexLayoutInfo
	{ vertexLayoutElements :: [VertexLayoutElement]
	, vertexLayoutStride :: Int
	}
