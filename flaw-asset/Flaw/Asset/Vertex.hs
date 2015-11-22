{-|
Module: Flaw.Asset.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Asset.Vertex
	( ColladaVertex(..)
	, VertexPNT(..)
	, VertexPNTWB(..)
	) where

import qualified Data.Vector as V
import Data.Word

import Flaw.Asset.Collada
import Flaw.FFI
import Flaw.Math

class ColladaVertex q where
	createColladaVertices :: ColladaVerticesData -> ColladaM (V.Vector q)

genStruct "VertexPNT"
	[ ([t| Vec3f |], "position")
	, ([t| Vec3f |], "normal")
	, ([t| Vec2f |], "texcoord")
	]

deriving instance Eq VertexPNT
deriving instance Ord VertexPNT

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
	[ ([t| Vec3f |], "position")
	, ([t| Vec3f |], "normal")
	, ([t| Vec2f |], "texcoord")
	, ([t| Vec4f |], "weights")
	, ([t| Vec4 Word8 |], "bones")
	]

deriving instance Eq VertexPNTWB
deriving instance Ord VertexPNTWB

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
			, f_VertexPNTWB_texcoord = xy__ $ texcoords V.! i
			, f_VertexPNTWB_bones = bones V.! i
			, f_VertexPNTWB_weights = weights V.! i
			}
