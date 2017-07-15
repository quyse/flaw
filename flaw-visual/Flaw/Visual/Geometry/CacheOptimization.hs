{-|
Module: Flaw.Visual.Geometry.CacheOptimization
Description: Geometry cache optimization algorithms.
License: MIT

Two cache optimization algorithms are implemented so far.
In preferred order of application:

* 'optimizeGeometryIndicesLocality' reorders triangles trying to use post-transform
vertex cache as efficiently as possible.
* 'optimizeGeometryVerticesLocality' reorders vertices (remapping indices accordingly, but not reordering triangles)
trying to optimize pre-transform vertex cache usage.
-}

module Flaw.Visual.Geometry.CacheOptimization
	( optimizeGeometryIndicesLocality
	, optimizeGeometryVerticesLocality
	) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

cacheWeights :: VU.Vector Float
cacheWeights = VU.fromList $ 0.75 : 0.75 : 0.75 : [pwr (1 - (i - 3) / 28) | i <- [3..31]] where
	pwr i = i * sqrt(i)

-- | Optimize usage of vertex post-transform cache by manipulating order of triangles.
-- "Linear-Speed Vertex Cache Optimisation" by Tom Forsyth
-- https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html
optimizeGeometryIndicesLocality :: (VG.Vector v i, Integral i) => v i -> v i
optimizeGeometryIndicesLocality indices = runST $ do

	let indicesCount = VG.length indices
	let trianglesCount = indicesCount `quot` 3
	let verticesCount = fromIntegral $ VG.maximum indices + 1

	-- count triangles for each vertex
	verticesTrianglesCounts <- VUM.replicate verticesCount 0
	let
		f i = when (i < indicesCount) $ do
			VGM.unsafeModify verticesTrianglesCounts (+ 1) $ fromIntegral $ indices VG.! i
			f $ i + 1
		in f 0

	-- create lists of triangles per vertex
	verticesTrianglesPtrs <- VUM.new verticesCount
	let
		f p i = when (i < verticesCount) $ do
			VGM.unsafeWrite verticesTrianglesPtrs i p
			vertexTrianglesCount <- VGM.unsafeRead verticesTrianglesCounts i
			f (p + vertexTrianglesCount) (i + 1)
		in f 0 0
	verticesTrianglesOffsets <- VU.freeze verticesTrianglesPtrs
	verticesTriangles <- VUM.new indicesCount
	let
		f i = when (i < indicesCount) $ do
			let v = fromIntegral $ indices VG.! i
			o <- VGM.unsafeRead verticesTrianglesPtrs v
			VGM.unsafeWrite verticesTriangles o (i `quot` 3)
			VGM.unsafeWrite verticesTrianglesPtrs v $ o + 1
			f $ i + 1
		in f 0

	-- post-transform vertex cache
	let cacheSize = VG.length cacheWeights
	cache <- VUM.replicate (cacheSize + 3) (-1)

	let vertexValenceBoost i = do
		vertexTrianglesCount <- VGM.unsafeRead verticesTrianglesCounts i
		return $ if vertexTrianglesCount > 0 then 2 / (sqrt $ fromIntegral vertexTrianglesCount) else 0

	-- initial vertex scores
	verticesScores <- VUM.new verticesCount
	let
		f i = when (i < verticesCount) $ do
			VUM.unsafeWrite verticesScores i =<< vertexValenceBoost i
			f $ i + 1
		in f 0

	-- initial triangle scores
	trianglesScores <- VUM.new trianglesCount
	let updateTriangleScore i = do
		a <- VGM.unsafeRead verticesScores $ fromIntegral $ indices VG.! (i * 3)
		b <- VGM.unsafeRead verticesScores $ fromIntegral $ indices VG.! (i * 3 + 1)
		c <- VGM.unsafeRead verticesScores $ fromIntegral $ indices VG.! (i * 3 + 2)
		VGM.unsafeWrite trianglesScores i $ a + b + c
	let
		f i = when (i < trianglesCount) $ do
			updateTriangleScore i
			f $ i + 1
		in f 0

	-- interval tree of triangles
	-- calculate size of the last level which is nearest power of two for triangles count
	let intervalTreeSize = let
		f i = if i < trianglesCount then f $ i * 2 else i
		in f 2
	intervalTree <- VUM.replicate (intervalTreeSize - 1) (-1)
	-- update node function
	let updateIntervalTreeNode i = do
		let lastLevel = i * 2 + 1 >= intervalTreeSize - 1
		a <-
			if lastLevel then let
				t = i * 2 + 1 - intervalTreeSize + 1
				in return $ if t < trianglesCount then t else (-1)
			else VGM.unsafeRead intervalTree (i * 2 + 1)
		b <-
			if lastLevel then let
				t = i * 2 + 2 - intervalTreeSize + 1
				in return $ if t < trianglesCount then t else (-1)
			else VGM.unsafeRead intervalTree (i * 2 + 2)
		as <- if a >= 0 then VGM.unsafeRead trianglesScores a else return 0
		bs <- if b >= 0 then VGM.unsafeRead trianglesScores b else return 0
		VGM.unsafeWrite intervalTree i $ if as > bs then a else b
	-- initial update
	let
		f i = when (i >= 0) $ do
			updateIntervalTreeNode i
			f $ i - 1
		in f $ intervalTreeSize - 2

	-- emit new indices
	newIndices <- VGM.new $ trianglesCount * 3
	let
		step triangleIndex = when (triangleIndex < trianglesCount) $ do
			-- get triangle with highest score
			triangle <- VGM.unsafeRead intervalTree 0

			-- set triangle score to 0, so it's not selected anymore
			VGM.unsafeWrite trianglesScores triangle 0
			-- emit indices
			let
				a = indices VG.! (triangle * 3)
				b = indices VG.! (triangle * 3 + 1)
				c = indices VG.! (triangle * 3 + 2)
			VGM.unsafeWrite newIndices (triangleIndex * 3) a
			VGM.unsafeWrite newIndices (triangleIndex * 3 + 1) b
			VGM.unsafeWrite newIndices (triangleIndex * 3 + 2) c
			-- remove triangle from triangle lists of its vertices
			let removeTriangleFromVertexTriangles v = do
				let vertexTrianglesOffset = verticesTrianglesOffsets VG.! v
				vertexTrianglesCount <- VGM.unsafeRead verticesTrianglesCounts v
				let
					f i tc = if i >= tc then return tc else do
						t <- VGM.unsafeRead verticesTriangles $ vertexTrianglesOffset + i
						if t == triangle then do
							when (i < tc - 1) $
								VGM.unsafeWrite verticesTriangles (vertexTrianglesOffset + i) =<< VGM.unsafeRead verticesTriangles (vertexTrianglesOffset + tc - 1)
							f i (tc - 1)
						else f (i + 1) tc
					in VGM.unsafeWrite verticesTrianglesCounts v =<< f 0 vertexTrianglesCount
			removeTriangleFromVertexTriangles $ fromIntegral a
			removeTriangleFromVertexTriangles $ fromIntegral b
			removeTriangleFromVertexTriangles $ fromIntegral c
			-- make room in cache
			let
				f i = when (i >= 0) $ do
					VGM.unsafeWrite cache (i + 3) =<< VGM.unsafeRead cache i
					f $ i - 1
				in f $ cacheSize - 1
			-- put triangle's vertices on top
			VGM.unsafeWrite cache 0 $ fromIntegral a
			VGM.unsafeWrite cache 1 $ fromIntegral b
			VGM.unsafeWrite cache 2 $ fromIntegral c
			-- remove triangle's vertices from the rest of the cache (in case they were in the cache already)
			cacheEnd <- let
				f p i = if i >= cacheSize + 3 then return p else do
					v <- VGM.unsafeRead cache i
					if v == fromIntegral a || v == fromIntegral b || v == fromIntegral c then f p (i + 1)
					else do
						when (p < i) $ VGM.unsafeWrite cache p v
						f (p + 1) (i + 1)
				in f 3 3
			-- update cached vertices' and their's triangles' scores and nodes
			let
				f i = when (i < cacheEnd) $ do
					v <- VGM.unsafeRead cache i
					when (v >= 0) $ do
						valenceBoost <- vertexValenceBoost v
						VGM.unsafeWrite verticesScores v $ (if i < cacheSize then cacheWeights VG.! i else 0) + valenceBoost
						vertexTrianglesCount <- VGM.unsafeRead verticesTrianglesCounts v
						let vertexTrianglesOffset = verticesTrianglesOffsets VG.! v
						let
							u j = when (j < vertexTrianglesCount) $ do
								t <- VGM.unsafeRead verticesTriangles $ vertexTrianglesOffset + j
								updateTriangleScore t
								let
									g k = do
										updateIntervalTreeNode k
										when (k > 0) $ g $ (k - 1) `quot` 2
									in g $ (t + intervalTreeSize - 1 - 1) `quot` 2
								u $ j + 1
							in u 0
				in f 0
			-- update removed triangle's nodes
			let
				g k = do
					updateIntervalTreeNode k
					when (k > 0) $ g $ (k - 1) `quot` 2
				in g $ (triangle + intervalTreeSize - 1 - 1) `quot` 2

			-- repeat
			step $ triangleIndex + 1
		in step 0

	VG.unsafeFreeze newIndices

-- | Optimize usage of vertex pre-transform cache by manipulating order of vertices.
optimizeGeometryVerticesLocality :: (VG.Vector va a, VG.Vector vi i, Integral i) => va a -> vi i -> (va a, vi i)
optimizeGeometryVerticesLocality vertices indices = runST $ do
	let verticesCount = VG.length vertices
	let indicesCount = VG.length indices
	vertexMap <- VUM.replicate verticesCount verticesCount
	newVertices <- VGM.new verticesCount
	newIndices <- VGM.new indicesCount
	let
		f p i = when (i < indicesCount) $ do
			let v = fromIntegral $ indices VG.! i
			remappedVertex <- VGM.unsafeRead vertexMap v
			if remappedVertex < verticesCount then do
				VGM.unsafeWrite newIndices i $ fromIntegral remappedVertex
				f p (i + 1)
			else do
				VGM.unsafeWrite vertexMap v p
				VGM.unsafeWrite newVertices p $ vertices VG.! v
				VGM.unsafeWrite newIndices i $ fromIntegral p
				f (p + 1) (i + 1)
		in f 0 0
	freezedNewVertices <- VG.unsafeFreeze newVertices
	freezedNewIndices <- VG.unsafeFreeze newIndices
	return (freezedNewVertices, freezedNewIndices)
