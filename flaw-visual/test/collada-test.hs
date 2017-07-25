{-|
Module: Main
Description: Collada import test.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
	( main
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import System.Exit

import Flaw.Asset.Collada
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Simplification
import Flaw.Visual.Geometry.Vertex
import Flaw.Math
import Flaw.Math.Transform

main :: IO ()
main = do
	bytes <- BL.readFile "assets/beaver-anim.dae"
	let e = runCollada $ do
		initColladaCache bytes
		geomVert <- createColladaVertices =<< parseGeometry =<< getElementById "geom-Beaver"
		[animation] <- mapM parseAnimation =<< getAllElementsByTag "animation"
		skeleton <- parseSkeleton =<< getElementById "node-Body"
		animateSkel <- animateSkeleton skeleton animation
		(skinVerticesData, skin) <- parseSkin skeleton =<< getSingleChildWithTag "skin" =<< getElementById "geom-Beaver-skin1"
		skinVert <- createColladaVertices skinVerticesData
		return (geomVert, skeleton, animateSkel, skin :: ColladaSkin Float4x4, skinVert)

	(geomVert, skeleton, animateSkel, skin, skinVert) <- case e of
		Right q -> return q
		Left err -> do
			print err
			exitFailure

	void $ evaluate $ packGeometry geomVert
	void $ evaluate $ uncurry packIndexedGeometry $ uncurry (simplifyGeometry 10) $ indexGeometryVertices $ VG.map f_VertexPNT_position geomVert
	void $ evaluate $ packGeometry skinVert
	void $ evaluate $ uncurry packIndexedGeometry $ uncurry (simplifyGeometry 10) $ indexGeometryVertices $ VG.map f_VertexPNTWB_position skinVert
	forM_ (animateSkel identityTransform 0 :: V.Vector Float4x4) evaluate
	void $ evaluate skeleton
	void $ evaluate skin
