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
	, VertexP(..)
	, VertexPT(..)
	, VertexPNT(..)
	, VertexPNTWB(..)
	) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Foreign.Storable

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
	[ ([t| Float4 |], "position")
	]

class Storable q => ColladaVertex q where
	createColladaVertices :: ColladaVerticesData -> ColladaM (VS.Vector q)

-- | Replace empty vector with sequence of fallback values.
fallbackVertexData :: VG.Vector v a => Int -> a -> v a -> v a
fallbackVertexData count fallbackValue inputVector = if VG.null inputVector then VG.replicate count fallbackValue else inputVector

genStruct "VertexP"
	[ ([t| Float3 |], "position")
	]

deriving instance Eq VertexP
deriving instance Ord VertexP

instance HasPositionAttribute VertexP where
	vertexPositionAttribute _ = (0, AttributeVec3 AttributeFloat32)

instance ColladaVertex VertexP where
	createColladaVertices verticesData@ColladaVerticesData
		{ cvdCount = count
		} = do
		positions <- cvdPositions verticesData
		return $ VG.generate count $ \i -> VertexP
			{ f_VertexP_position = positions VG.! i
			}

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
	createColladaVertices verticesData@ColladaVerticesData
		{ cvdCount = count
		} = do
		positions <- cvdPositions verticesData
		texcoords <- fallbackVertexData count (Vec2 0 0) <$> cvdTexcoords verticesData
		return $ VG.generate count $ \i -> VertexPT
			{ f_VertexPT_position = positions VG.! i
			, f_VertexPT_texcoord = let Vec2 tx ty = texcoords VG.! i in Vec2 tx (1 - ty)
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
	createColladaVertices verticesData@ColladaVerticesData
		{ cvdCount = count
		} = do
		positions <- cvdPositions verticesData
		normals <- cvdNormals verticesData
		texcoords <- fallbackVertexData count (Vec2 0 0) <$> cvdTexcoords verticesData
		return $ VG.generate count $ \i -> VertexPNT
			{ f_VertexPNT_position = positions VG.! i
			, f_VertexPNT_normal = normals VG.! i
			, f_VertexPNT_texcoord = let Vec2 tx ty = texcoords VG.! i in Vec2 tx (1 - ty)
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
	vertexBonesAttribute _ = (48, AttributeVec4 AttributeUint8)

instance ColladaVertex VertexPNTWB where
	createColladaVertices verticesData@ColladaVerticesData
		{ cvdCount = count
		} = do
		positions <- cvdPositions verticesData
		normals <- cvdNormals verticesData
		texcoords <- fallbackVertexData count (Vec2 0 0) <$> cvdTexcoords verticesData
		bones <- cvdBones verticesData
		weights <- cvdWeights verticesData
		return $ VG.generate count $ \i -> VertexPNTWB
			{ f_VertexPNTWB_position = positions VG.! i
			, f_VertexPNTWB_normal = normals VG.! i
			, f_VertexPNTWB_texcoord = let Vec2 tx ty = texcoords VG.! i in Vec2 tx (1 - ty)
			, f_VertexPNTWB_bones = bones VG.! i
			, f_VertexPNTWB_weights = weights VG.! i
			}
