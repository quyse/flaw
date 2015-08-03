{-|
Module: Flaw.Asset.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Asset.Vertex
	( ColladaVertex(..)
	, VertexPNT(..)
	, VertexPNTBW(..)
	) where

import qualified Data.Vector as V

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
			, f_VertexPNT_texcoord = xy__ $ texcoords V.! i
			}

genStruct "VertexPNTBW"
	[ ([t| Vec3f |], "position")
	, ([t| Vec3f |], "normal")
	, ([t| Vec2f |], "texcoord")
	, ([t| Vec4i |], "bones")
	, ([t| Vec4f |], "weights")
	]

deriving instance Eq VertexPNTBW
deriving instance Ord VertexPNTBW

instance ColladaVertex VertexPNTBW where
	createColladaVertices verticesData = do
		positions <- cvdPositions verticesData
		normals <- cvdNormals verticesData
		texcoords <- cvdTexcoords verticesData
		bones <- cvdBones verticesData
		weights <- cvdWeights verticesData
		return $ V.generate (cvdCount verticesData) $ \i -> VertexPNTBW
			{ f_VertexPNTBW_position = positions V.! i
			, f_VertexPNTBW_normal = normals V.! i
			, f_VertexPNTBW_texcoord = xy__ $ texcoords V.! i
			, f_VertexPNTBW_bones = bones V.! i
			, f_VertexPNTBW_weights = weights V.! i
			}
