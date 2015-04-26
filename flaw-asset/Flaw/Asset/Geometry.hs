{-|
Module: Flaw.Asset.Geometry
Description: Some geometry helpers.
License: MIT
-}

module Flaw.Asset.Geometry
	( indexVertices
	) where

import Control.Monad.Primitive
import Data.Foldable
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

unique :: (PrimMonad m, Eq a, VGM.MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
unique v = if VGM.null v then return v else do
	let f p i = do
		a <- VGM.unsafeRead v i
		b <- VGM.unsafeRead v p
		if a == b then return p
		else do
			let q = p + 1
			VGM.write v q a
			return q
	end <- foldlM f 0 [0..(VGM.length v - 1)]
	return $ VGM.slice 0 (end + 1) v

-- | Index vertices.
indexVertices :: (PrimMonad m, Ord a) => V.Vector a -> m (V.Vector a, VU.Vector Int)
indexVertices vertices = do
	mVertices <- VG.thaw vertices
	VAI.sort mVertices
	uniqueVertices <- unique mVertices
	indices <- VG.mapM (VAS.binarySearchL uniqueVertices) vertices
	resultVertices <- VG.freeze uniqueVertices
	return (resultVertices, VG.convert indices)
