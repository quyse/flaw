{-|
Module: Flaw.Visual.Geometry.Simplification
Description: Geometry simplification algorithm.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

module Flaw.Visual.Geometry.Simplification
	( simplifyGeometry
	) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Flaw.Math

data PairKey = PairKey {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 deriving (Eq, Ord)

data Pair = Pair
	{ pair_vertex0 :: {-# UNPACK #-} !Word32
	, pair_vertex1 :: {-# UNPACK #-} !Word32
	, pair_optimalVertex :: {-# UNPACK #-} !Float3
	, pair_cost :: {-# UNPACK #-} !Float
	}

instance Storable Pair where
	sizeOf _ = 24
	alignment _ = 4
	peek ptr = Pair
		<$> peek (castPtr ptr)
		<*> peek (castPtr ptr `plusPtr` 4)
		<*> peek (castPtr ptr `plusPtr` 8)
		<*> peek (castPtr ptr `plusPtr` 20)
	poke ptr (Pair v0 v1 ov c) = do
		poke (castPtr ptr) v0
		poke (castPtr ptr `plusPtr` 4) v1
		poke (castPtr ptr `plusPtr` 8) ov
		poke (castPtr ptr `plusPtr` 20) c

-- | Simplify geometry.
-- Algorithm: http://cseweb.ucsd.edu/~ravir/190/2016/garland97.pdf
simplifyGeometry :: Int -> VS.Vector Float3 -> VS.Vector Word32 -> (VS.Vector Float3, VS.Vector Word32)
simplifyGeometry iterationsCount vertices indices = runST $ do
	let
		verticesCount = VG.length vertices
		indicesCount = VG.length indices
		trianglesCount = indicesCount `quot` 3

	-- allocate memory
	vertexPositions <- VS.thaw vertices
	vertexParents <- VUM.new verticesCount
	let
		f i = when (i < verticesCount) $ do
			VGM.unsafeWrite vertexParents i i
			f $ i + 1
		in f 0
	vertexRanks <- VUM.replicate verticesCount (1 :: Word32)
	vertexQuadrics <- VSM.replicate verticesCount 0
	pairHeap <- VSM.new indicesCount
	pairHeapSizeRef <- newSTRef 0
	pairHeapIndexByKeyRef <- newSTRef M.empty

	let calculatePair (PairKey v0 v1) = do
		quadric0 <- VGM.unsafeRead vertexQuadrics $ fromIntegral v0
		quadric1 <- VGM.unsafeRead vertexQuadrics $ fromIntegral v1
		let
			quadricSum@(Float4x4
				q11 q12 q13 q14
				q21 q22 q23 q24
				q31 q32 q33 q34
				_q41 _q42 _q43 _q44
				) = quadric0 + quadric1
			q_12_12 = q11 * q22 - q21 * q12
			q_12_13 = q11 * q23 - q21 * q13
			q_12_14 = q11 * q24 - q21 * q14
			q_12_23 = q12 * q23 - q22 * q13
			q_12_24 = q12 * q24 - q22 * q14
			q_12_34 = q13 * q24 - q23 * q14
			q_123_123 = q_12_12 * q33 - q_12_13 * q32 + q_12_23 * q31
			q_123_124 = q_12_12 * q34 - q_12_14 * q32 + q_12_24 * q31
			q_123_134 = q_12_13 * q34 - q_12_14 * q33 + q_12_34 * q31
			q_123_234 = q_12_23 * q34 - q_12_24 * q33 + q_12_34 * q32
			-- q_1234_1234 = q_123_123 * q44 - q_123_124 * q43 + q_123_134 * q42 - q_123_234 * q41
			aff :: Float3 -> Float4
			aff (Float3 x y z) = Float4 x y z 1
			cost :: Float4x4 -> Float3 -> Float
			cost q (aff -> p) = p `dot` (q `mul` p)
		pv0 <- VGM.unsafeRead vertexPositions $ fromIntegral v0
		pv1 <- VGM.unsafeRead vertexPositions $ fromIntegral v1
		let (oc, ov) =
			-- try global minimum
			if abs q_123_123 > 1e-4 then let
				v = Float3
					(negate $ q_123_234 / q_123_123)
					(q_123_134 / q_123_123)
					(negate $ q_123_124 / q_123_123)
				in (cost quadricSum v, v)
			-- try minimum along the edge
			else let
				v10 = aff pv1 - aff pv0
				u = quadricSum `mul` v10
				h = u `dot` v10
				in if abs h > 1e-4 then let
					hinv = 1 / h
					v = pv0 * vecFromScalar ((u `dot` aff pv1) * hinv) - pv1 * vecFromScalar ((u `dot` aff pv0) * hinv)
					in (cost quadricSum v, v)
				-- try minimum from ends and the middle
				else let
					pm = (pv0 + pv1) * 0.5
					in min (cost quadricSum pm, pm) $ min (cost quadricSum pv0, pv0) (cost quadricSum pv1, pv1)

		return Pair
			{ pair_vertex0 = v0
			, pair_vertex1 = v1
			, pair_optimalVertex = ov
			, pair_cost = oc - cost quadric0 pv0 - cost quadric1 pv1
			}

	-- calculate initial vertex quadrics
	let f i = when (i < trianglesCount) $ do
		let
			i1 = fromIntegral $ indices VG.! (i * 3)
			i2 = fromIntegral $ indices VG.! (i * 3 + 1)
			i3 = fromIntegral $ indices VG.! (i * 3 + 2)
			p1 = vertices VG.! i1
			p2 = vertices VG.! i2
			p3 = vertices VG.! i3
			normal@(Float3 a b c) = normalize $ cross (p2 - p1) (p3 - p1)
			d = negate $ dot normal p1
			aa = a * a
			ab = a * b
			ac = a * c
			ad = a * d
			bb = b * b
			bc = b * c
			bd = b * d
			cc = c * c
			cd = c * d
			dd = d * d
			q = Float4x4
				aa ab ac ad
				ab bb bc bd
				ac bc cc cd
				ad bd cd dd
		VGM.unsafeModify vertexQuadrics (+ q) i1
		VGM.unsafeModify vertexQuadrics (+ q) i2
		VGM.unsafeModify vertexQuadrics (+ q) i3
		f $ i + 1
		in f 0

	-- add initial pairs
	let f i = when (i < trianglesCount) $ do
		let
			i1 = indices VG.! (i * 3)
			i2 = indices VG.! (i * 3 + 1)
			i3 = indices VG.! (i * 3 + 2)
			addPair v0 v1 = unless (v0 == v1) $ do
				let key = PairKey (min v0 v1) (max v0 v1)
				pairHeapIndexByKey <- readSTRef pairHeapIndexByKeyRef
				unless (M.member key pairHeapIndexByKey) $ do
					pairHeapSize <- readSTRef pairHeapSizeRef
					VGM.unsafeWrite pairHeap pairHeapSize =<< calculatePair key
					writeSTRef pairHeapSizeRef $! pairHeapSize + 1
					writeSTRef pairHeapIndexByKeyRef $! M.insert key pairHeapSize pairHeapIndexByKey
		addPair i1 i2
		addPair i2 i3
		addPair i1 i3
		f $ i + 1
		in f 0

	-- add reverse pairs in index too
	do
		pairs <- readSTRef pairHeapIndexByKeyRef
		writeSTRef pairHeapIndexByKeyRef $! foldr (\(PairKey v0 v1, p) -> M.insert (PairKey v1 v0) p) pairs (M.toList pairs)

	-- find open edges and add additional costs for vertices
	do
		pairHeapIndexByKey <- readSTRef pairHeapIndexByKeyRef
		pairHeapSize <- readSTRef pairHeapSizeRef
		-- count triangles for every edge
		pairTriangleCounts <- VUM.replicate pairHeapSize (0 :: Word32)
		let
			f i = when (i < trianglesCount) $ do
				let
					i1 = indices VG.! (i * 3)
					i2 = indices VG.! (i * 3 + 1)
					i3 = indices VG.! (i * 3 + 2)
					accountForPair a b = case M.lookup (PairKey a b) pairHeapIndexByKey of
						Just h -> VGM.unsafeModify pairTriangleCounts (+ 1) h
						Nothing -> return ()
				accountForPair i1 i2
				accountForPair i2 i3
				accountForPair i1 i3
				f $ i + 1
			in f 0
		-- for every open edge add penalty to its vertices for moving from the edge
		let
			f i = when (i < pairHeapSize) $ do
				triangleCount <- VGM.unsafeRead pairTriangleCounts i
				when (triangleCount == 1) $ do
					Pair
						{ pair_vertex0 = v0
						, pair_vertex1 = v1
						} <- VGM.unsafeRead pairHeap i
					pv0 <- VGM.unsafeRead vertexPositions $ fromIntegral v0
					pv1 <- VGM.unsafeRead vertexPositions $ fromIntegral v1
					when (norm2 (pv1 - pv0) > 1e-8) $ do
						let
							r@(Float3 rx ry rz) = normalize $ pv1 - pv0
							rxx = rx * rx
							rxy = rx * ry
							rxz = rx * rz
							ryy = ry * ry
							ryz = ry * rz
							rzz = rz * rz
							a = Float3 (rxx - 1) rxy rxz
							b = Float3 rxy (ryy - 1) ryz
							c = Float3 rxz ryz (rzz - 1)
							d = pv0 - r * vecFromScalar (dot r pv0)
							aa = dot a a
							ab = dot a b
							ac = dot a c
							ad = dot a d
							bb = dot b b
							bc = dot b c
							bd = dot b d
							cc = dot c c
							cd = dot c d
							dd = dot d d
							q = Float4x4
								aa ab ac ad
								ab bb bc bd
								ac bc cc cd
								ad bd cd dd
						VGM.unsafeModify vertexQuadrics (+ q) $ fromIntegral v0
						VGM.unsafeModify vertexQuadrics (+ q) $ fromIntegral v1
				f $ i + 1
			in f 0

	-- vertex disjoint-set functions

	-- get vertex parent
	let vertexParent a = do
		p <- VGM.unsafeRead vertexParents a
		if p == a then return p else do
			pp <- vertexParent p
			VGM.unsafeWrite vertexParents a pp
			return pp
	-- union vertices
	let unionVertices a b = do
		pa <- vertexParent a
		pb <- vertexParent b
		if pa == pb then return pa else do
			ra <- VGM.unsafeRead vertexRanks pa
			rb <- VGM.unsafeRead vertexRanks pb
			if ra > rb then do
				VGM.unsafeWrite vertexParents pb pa
				VGM.unsafeWrite vertexRanks pa $ ra + rb
				return pa
			else do
				VGM.unsafeWrite vertexParents pa pb
				VGM.unsafeWrite vertexRanks pb $ ra + rb
				return pb

	-- heap functions

	-- swap elements
	let heapSwap a pa@Pair
		{ pair_vertex0 = a0
		, pair_vertex1 = a1
		} b pb@Pair
		{ pair_vertex0 = b0
		, pair_vertex1 = b1
		} = do
		VGM.unsafeWrite pairHeap b pa
		VGM.unsafeWrite pairHeap a pb
		modifySTRef' pairHeapIndexByKeyRef
			$ M.insert (PairKey a0 a1) b
			. M.insert (PairKey a1 a0) b
			. M.insert (PairKey b0 b1) a
			. M.insert (PairKey b1 b0) a

	-- sift down
	let heapSiftDown i = do
		pairHeapSize <- readSTRef pairHeapSizeRef
		p <- VGM.unsafeRead pairHeap i
		let
			l = i * 2 + 1
			r = i * 2 + 2
			swap m mp = do
				heapSwap i p m mp
				heapSiftDown m
		lp <- if l < pairHeapSize then VGM.unsafeRead pairHeap l else return p
		rp <- if r < pairHeapSize then VGM.unsafeRead pairHeap r else return p
		if pair_cost lp < pair_cost p && pair_cost lp <= pair_cost rp then swap l lp
		else if pair_cost rp < pair_cost p && pair_cost rp <= pair_cost lp then swap r rp
		else return ()

	-- sift up
	let heapSiftUp i = when (i > 0) $ do
		p <- VGM.unsafeRead pairHeap i
		let m = (i - 1) `quot` 2
		mp <- VGM.unsafeRead pairHeap m
		when (pair_cost p < pair_cost mp) $ do
			heapSwap i p m mp
			heapSiftUp m

	-- update
	let heapUpdate i =
		if i > 0 then do
			p <- VGM.unsafeRead pairHeap i
			let m = (i - 1) `quot` 2
			mp <- VGM.unsafeRead pairHeap m
			case compare (pair_cost p) (pair_cost mp) of
				LT -> heapSiftUp i
				GT -> heapSiftDown i
				EQ -> return ()
		else heapSiftDown i

	-- delete
	let heapDelete i = do
		pairHeapSize <- readSTRef pairHeapSizeRef
		let m = pairHeapSize - 1
		writeSTRef pairHeapSizeRef $! m
		Pair
			{ pair_vertex0 = v0
			, pair_vertex1 = v1
			} <- VGM.unsafeRead pairHeap i
		if (i < m) then do
			mp@Pair
				{ pair_vertex0 = mv0
				, pair_vertex1 = mv1
				} <- VGM.unsafeRead pairHeap m
			VGM.unsafeWrite pairHeap i mp
			modifySTRef' pairHeapIndexByKeyRef
				$ M.insert (PairKey mv0 mv1) i
				. M.insert (PairKey mv1 mv0) i
				. M.delete (PairKey v0 v1)
				. M.delete (PairKey v1 v0)
			heapUpdate i
		else modifySTRef' pairHeapIndexByKeyRef
			$ M.delete (PairKey v0 v1)
			. M.delete (PairKey v1 v0)

	-- make actual heap from pairs
	do
		pairHeapSize <- readSTRef pairHeapSizeRef
		let f i = when (i >= 0) $ do
			heapSiftDown i
			f $ i - 1
		f $ pairHeapSize - 1

	-- contraction step
	let contraction = do
		-- get minimal cost pair
		Pair
			{ pair_vertex0 = pv0
			, pair_vertex1 = pv1
			, pair_optimalVertex = vo
			} <- VGM.unsafeRead pairHeap 0
		-- union vertices
		v <- unionVertices (fromIntegral pv0) (fromIntegral pv1)
		-- update position
		VGM.unsafeWrite vertexPositions v vo
		-- delete minimal cost pair
		heapDelete 0
		-- figure out what vertex got replaced
		let (v0, v1) = case (fromIntegral v == pv0, fromIntegral v == pv1) of
			(True, False) -> (pv0, pv1)
			(False, True) -> (pv1, pv0)
			_ -> error $ show ("impossible pair", v, pv0, pv1)
		-- sum up quadrics
		v1q <- VGM.unsafeRead vertexQuadrics (fromIntegral v1)
		VGM.unsafeModify vertexQuadrics (+ v1q) (fromIntegral v0)
		-- update all pairs with v1 to use v0
		pairsWithV1
			<-  M.takeWhileAntitone (\(PairKey a _) -> a == v1)
			.   M.dropWhileAntitone (\(PairKey a _) -> a < v1)
			<$> readSTRef pairHeapIndexByKeyRef
		forM_ (M.keys pairsWithV1) $ \oldKey@(PairKey _v1 b) -> unless (v0 == b) $ do
			let newKey = PairKey v0 b
			pairHeapIndex <- readSTRef pairHeapIndexByKeyRef
			Just h <- return $ M.lookup oldKey pairHeapIndex
			-- if contraction made double edge, remove it
			if M.member newKey pairHeapIndex then heapDelete h
			-- else fix edge to point to v0
			else do
				writeSTRef pairHeapIndexByKeyRef $
					( M.insert newKey h
					. M.insert (PairKey b v0) h
					. M.delete oldKey
					. M.delete (PairKey b v1)
					) pairHeapIndex
				VGM.unsafeModify pairHeap (\p -> p
					{ pair_vertex0 = min b v0
					, pair_vertex1 = max b v0
					}) h
				heapUpdate h
		-- update all pairs using v0
		pairsWithV0
			<-  M.takeWhileAntitone (\(PairKey a _) -> a == v0)
			.   M.dropWhileAntitone (\(PairKey a _) -> a < v0)
			<$> readSTRef pairHeapIndexByKeyRef
		forM_ (M.keys pairsWithV0) $ \(PairKey _v0 b) -> do
			Just h <- M.lookup (PairKey v0 b) <$> readSTRef pairHeapIndexByKeyRef
			VGM.unsafeWrite pairHeap h =<< calculatePair (PairKey (min v0 b) (max v0 b))
			heapUpdate h

	-- perform contractions
	replicateM_ iterationsCount contraction

	-- filter out indices with degenerate triangles
	newIndices <- VSM.new indicesCount
	end <- let
		f p i =
			if i < trianglesCount then do
				i1 <- vertexParent $ fromIntegral $ indices VG.! (i * 3)
				i2 <- vertexParent $ fromIntegral $ indices VG.! (i * 3 + 1)
				i3 <- vertexParent $ fromIntegral $ indices VG.! (i * 3 + 2)
				if (i1 == i2 || i1 == i3 || i2 == i3) then f p (i + 1) else do
					VSM.unsafeWrite newIndices p $ fromIntegral i1
					VSM.unsafeWrite newIndices (p + 1) $ fromIntegral i2
					VSM.unsafeWrite newIndices (p + 2) $ fromIntegral i3
					f (p + 3) (i + 1)
			else return p
		in f 0 0

	resultVertices <- VG.unsafeFreeze vertexPositions
	resultIndices <- VG.unsafeFreeze $ VGM.slice 0 end newIndices

	return (resultVertices, resultIndices)
