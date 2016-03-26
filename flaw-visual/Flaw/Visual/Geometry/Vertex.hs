{-|
Module: Flaw.Visual.Geometry.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Visual.Geometry.Vertex
	(
	-- * Attributes
	  HasPositionAttribute(..)
	, HasNormalAttribute(..)
	, HasTexcoordAttribute(..)
	, HasWeightsBonesAttributes(..)
	, vertexAttribute
	-- * Predefined vertices
	, QuadVertex(..)
	-- ** Predefined Collada vertices
	, ColladaVertex(..)
	, VertexPT(..)
	, VertexPNT(..)
	, VertexPNTWB(..)
	) where

import qualified Data.Vector as V

import Flaw.Asset.Collada
import Flaw.FFI
import Flaw.Graphics.Program
import Flaw.Math

class HasPositionAttribute v where
	-- | Get offset and format of position attribute.
	vertexPositionAttribute :: v -> (Int, AttributeFormat Float3)

class HasNormalAttribute v where
	-- | Get offset and format of normal attribute.
	vertexNormalAttribute :: v -> (Int, AttributeFormat Float3)

class HasTexcoordAttribute v where
	-- | Get offset and format of texcoord attribute.
	vertexTexcoordAttribute :: v -> (Int, AttributeFormat Float2)

-- | Class of vertex having 4 bones and weights for skeletal animation.
class HasWeightsBonesAttributes v where
	-- | Get offset and format of weights attribute.
	vertexWeightsAttribute :: v -> (Int, AttributeFormat Float4)
	-- | Get offset and format of bones attribute.
	vertexBonesAttribute :: v -> (Int, AttributeFormat Word32_4)

-- | Create attribute from pair (offset, format) provided with slot and divisor.
vertexAttribute :: OfAttributeType a => Int -> Int -> (Int, AttributeFormat a) -> Program (Node a)
vertexAttribute slot divisor (offset, format) = attribute slot offset divisor format

genStruct "QuadVertex"
	[ ([t| Int8_4 |], "position")
	, ([t| Int8_2 |], "texcoord")
	]

class ColladaVertex q where
	createColladaVertices :: ColladaVerticesData -> ColladaM (V.Vector q)

genStruct "VertexPT"
	[ ([t| Float3 |], "position")
	, ([t| Float2 |], "texcoord")
	]

deriving instance Eq VertexPT
deriving instance Ord VertexPT

instance HasPositionAttribute VertexPT where
	vertexPositionAttribute _ = (0, AttributeVec3 AttributeFloat32)

instance HasTexcoordAttribute VertexPT where
	vertexTexcoordAttribute _ = (12, AttributeVec2 AttributeFloat32)

instance ColladaVertex VertexPT where
	createColladaVertices verticesData = do
		positions <- cvdPositions verticesData
		texcoords <- cvdTexcoords verticesData
		return $ V.generate (cvdCount verticesData) $ \i -> VertexPT
			{ f_VertexPT_position = positions V.! i
			, f_VertexPT_texcoord = let Vec3 tx ty _tz = texcoords V.! i in Vec2 tx (1 - ty)
			}

genStruct "VertexPNT"
	[ ([t| Float3 |], "position")
	, ([t| Float3 |], "normal")
	, ([t| Float2 |], "texcoord")
	]

deriving instance Eq VertexPNT
deriving instance Ord VertexPNT

instance HasPositionAttribute VertexPNT where
	vertexPositionAttribute _ = (0, AttributeVec3 AttributeFloat32)

instance HasNormalAttribute VertexPNT where
	vertexNormalAttribute _ = (12, AttributeVec3 AttributeFloat32)

instance HasTexcoordAttribute VertexPNT where
	vertexTexcoordAttribute _ = (24, AttributeVec2 AttributeFloat32)

instance ColladaVertex VertexPNT where
	createColladaVertices verticesData = do
		positions <- cvdPositions verticesData
		normals <- cvdNormals verticesData
		texcoords <- cvdTexcoords verticesData
		return $ V.generate (cvdCount verticesData) $ \i -> VertexPNT
			{ f_VertexPNT_position = positions V.! i
			, f_VertexPNT_normal = normals V.! i
			, f_VertexPNT_texcoord = let Vec3 tx ty _tz = texcoords V.! i in Vec2 tx (1 - ty)
			}

genStruct "VertexPNTWB"
	[ ([t| Float3 |], "position")
	, ([t| Float3 |], "normal")
	, ([t| Float2 |], "texcoord")
	, ([t| Float4 |], "weights")
	, ([t| Word8_4 |], "bones")
	]

deriving instance Eq VertexPNTWB
deriving instance Ord VertexPNTWB

instance HasPositionAttribute VertexPNTWB where
	vertexPositionAttribute _ = (0, AttributeVec3 AttributeFloat32)

instance HasNormalAttribute VertexPNTWB where
	vertexNormalAttribute _ = (12, AttributeVec3 AttributeFloat32)

instance HasTexcoordAttribute VertexPNTWB where
	vertexTexcoordAttribute _ = (24, AttributeVec2 AttributeFloat32)

instance HasWeightsBonesAttributes VertexPNTWB where
	vertexWeightsAttribute _ = (32, AttributeVec4 AttributeFloat32)
	vertexBonesAttribute _ = (48, AttributeVec4 (AttributeUint8 NonNormalized))

instance ColladaVertex VertexPNTWB where
	createColladaVertices verticesData = do
		positions <- cvdPositions verticesData
		normals <- cvdNormals verticesData
		texcoords <- cvdTexcoords verticesData
		bones <- cvdBones verticesData
		weights <- cvdWeights verticesData
		return $ V.generate (cvdCount verticesData) $ \i -> VertexPNTWB
			{ f_VertexPNTWB_position = positions V.! i
			, f_VertexPNTWB_normal = normals V.! i
			, f_VertexPNTWB_texcoord = let Vec3 tx ty _tz = texcoords V.! i in Vec2 tx (1 - ty)
			, f_VertexPNTWB_bones = bones V.! i
			, f_VertexPNTWB_weights = weights V.! i
			}
