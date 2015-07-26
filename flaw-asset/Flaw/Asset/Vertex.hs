{-|
Module: Flaw.Asset.Vertex
Description: Useful vertex structures with support for Collada.
License: MIT
-}

{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Flaw.Asset.Vertex
	( ColladaVertexConstructor(..)
	, loadColladaVertices
	, VertexPNT(..)
	) where

import Control.Monad
import Control.Monad.State
import qualified Data.Vector as V
import qualified Text.XML.Light as XML

import Flaw.Asset.Collada
import Flaw.FFI
import Flaw.Math

class ColladaVertexConstructor q where
	colladaVertexConstructor :: (forall a v. Parse a v => String -> ColladaM (v a, Int)) -> ColladaM (V.Vector q)

loadColladaVertices :: ColladaVertexConstructor a => XML.Element -> ColladaM (V.Vector a)
loadColladaVertices element = parseGeometry colladaVertexConstructor element

genStruct "VertexPNT"
	[ ([t| Vec3f |], "position")
	, ([t| Vec3f |], "normal")
	, ([t| Vec2f |], "texcoord")
	]

deriving instance Eq VertexPNT
deriving instance Ord VertexPNT

instance ColladaVertexConstructor VertexPNT where
	colladaVertexConstructor f = do
		unit <- liftM (csUnit . ccSettings) get
		positions <- liftM chunks3stride $ f "VERTEX"
		normals <- liftM chunks3stride $ f "NORMAL"
		texcoords <- liftM chunks3stride $ f "TEXCOORD"
		let create ((px, py, pz), (nx, ny, nz), (tx, ty, _tz)) = VertexPNT
			{ f_VertexPNT_position = Vec3 (px * unit) (py * unit) (pz * unit)
			, f_VertexPNT_normal = Vec3 nx ny nz
			, f_VertexPNT_texcoord = Vec2 tx (1 - ty)
			}
		return $ V.map create $ V.zip3 positions normals texcoords
