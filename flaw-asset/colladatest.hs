{-|
Module: Main
Description: Tests Collada import.
License: MIT
-}

module Main
	( main
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Time.Clock
import System.Exit

import Flaw.Asset.Collada
import Flaw.Asset.Util
import Flaw.Asset.Vertex
import Flaw.Math
import Flaw.Math.Transform

main :: IO ()
main = do
	timeBefore <- getCurrentTime

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
			putStrLn err
			exitFailure

	_ <- liftM evaluate $ packGeometry (geomVert :: V.Vector VertexPNT)
	_ <- liftM evaluate $ packGeometry (skinVert :: V.Vector VertexPNTWB)
	forM_ (animateSkel identityTransform 0 :: V.Vector Float4x4) $ void . evaluate
	_ <- evaluate skeleton
	_ <- evaluate skin

	timeAfter <- getCurrentTime

	putStrLn $ "colladatest: import time (μs): " ++ show ((floor $ diffUTCTime timeAfter timeBefore) :: Int)
