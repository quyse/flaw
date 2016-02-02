{-|
Module: Flaw.Visual.Geometry.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Visual.Geometry.Vertex
	( ColladaVertex(..)
	, VertexPT(..)
	, VertexPNT(..)
	, VertexPNTWB(..)
	) where

import qualified Data.Vector as V

import Flaw.Asset.Collada
import Flaw.FFI
import Flaw.Math

class ColladaVertex q where
	createColladaVertices :: ColladaVerticesData -> ColladaM (V.Vector q)

genStruct "VertexPT"
	[ ([t| Float3 |], "position")
	, ([t| Float2 |], "texcoord")
	]

deriving instance Eq VertexPT
deriving instance Ord VertexPT

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
