{-|
Module: Flaw.Asset.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Asset.Vertex
	( ColladaVertex(..)
	, loadColladaVertices
	, VertexPNT(..)
	) where

import qualified Data.Vector as V
import qualified Text.XML.Light as XML

import Flaw.Asset.Collada
import Flaw.FFI
import Flaw.Math

class ColladaVertex q where
	createColladaVertices :: ColladaVerticesData -> ColladaM (V.Vector q)

loadColladaVertices :: ColladaVertex a => XML.Element -> ColladaM (V.Vector a)
loadColladaVertices element = createColladaVertices =<< parseGeometry element

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
