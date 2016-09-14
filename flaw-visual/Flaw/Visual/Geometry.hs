{-|
Module: Flaw.Visual.Geometry
Description: Geometry.
License: MIT
-}

{-# LANGUAGE DeriveGeneric #-}

module Flaw.Visual.Geometry
	( Geometry(..)
	, PackedGeometry(..)
	, packGeometry
	, packIndexedGeometry
	, loadPackedGeometry
	, emitGeometryAsset
	, loadGeometryAsset
	, indexGeometryVertices
	) where

import Control.Exception
import Control.Monad.Primitive
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.Storable
import GHC.Generics(Generic)
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Book
import Flaw.Build
import Flaw.Exception
import Flaw.Graphics
import Flaw.Visual.Geometry.Vertex

data Geometry d = Geometry
	{ geometryVertexBuffer :: !(VertexBufferId d)
	, geometryIndexBuffer :: !(IndexBufferId d)
	, geometryIndicesCount :: {-# UNPACK #-} !Int
	}

data PackedGeometry = PackedGeometry
	{ packedGeometryVerticesBytes :: !B.ByteString
	, packedGeometryIndicesBytes :: !B.ByteString
	, packedGeometryIndicesCount :: {-# UNPACK #-} !Int
	, packedGeometryVertexStride :: {-# UNPACK #-} !Int
	, packedGeometryIsIndices32Bit :: !Bool
	} deriving Generic

instance S.Serialize PackedGeometry

-- | Pack raw vertices.
packGeometry :: (Ord a, Storable a) => V.Vector a -> IO PackedGeometry
packGeometry rawVertices = uncurry packIndexedGeometry =<< indexGeometryVertices rawVertices

-- | Pack geometry with indices.
-- Chooses indices format.
packIndexedGeometry :: Storable a => V.Vector a -> VU.Vector Int -> IO PackedGeometry
packIndexedGeometry vertices indices = do
	verticesBytes <- packVector vertices
	(isIndices32Bit, indicesBytes) <-
		if length vertices > 0x10000 then do
			indicesBytes <- packVector (VG.map fromIntegral indices :: VU.Vector Word32)
			return (True, indicesBytes)
		else do
			indicesBytes <- packVector (VG.map fromIntegral indices :: VU.Vector Word16)
			return (False, indicesBytes)
	return PackedGeometry
		{ packedGeometryVerticesBytes = verticesBytes
		, packedGeometryIndicesBytes = indicesBytes
		, packedGeometryIndicesCount = VG.length indices
		, packedGeometryVertexStride = sizeOf (VG.head vertices)
		, packedGeometryIsIndices32Bit = isIndices32Bit
		}

-- | Load geometry into device.
loadPackedGeometry :: Device d => d -> PackedGeometry -> IO (Geometry d, IO ())
loadPackedGeometry device PackedGeometry
	{ packedGeometryVerticesBytes = verticesBytes
	, packedGeometryIndicesBytes = indicesBytes
	, packedGeometryIndicesCount = indicesCount
	, packedGeometryVertexStride = vertexStride
	, packedGeometryIsIndices32Bit = isIndices32Bit
	} = withSpecialBook $ \bk -> do
	vertexBuffer <- book bk $ createStaticVertexBuffer device verticesBytes vertexStride
	indexBuffer <- book bk $ createStaticIndexBuffer device indicesBytes isIndices32Bit
	return Geometry
		{ geometryVertexBuffer = vertexBuffer
		, geometryIndexBuffer = indexBuffer
		, geometryIndicesCount = indicesCount
		}

-- | Pack geometry into bytestring.
emitGeometryAsset :: FilePath -> ColladaM ColladaElement -> Q B.ByteString
emitGeometryAsset fileName getElement = do
	bytes <- loadFile fileName
	let eitherVertices = runCollada $ do
		initColladaCache bytes
		createColladaVertices =<< parseGeometry =<< getElement
	case eitherVertices of
		Right vertices -> fmap S.encode $ runIO $ packGeometry (vertices :: V.Vector VertexPNT)
		Left err -> do
			let msg = "failed to emit geometry asset " ++ fileName ++ ": " ++ T.unpack err
			reportError msg
			return B.empty

-- | Load geometry from bytestring.
loadGeometryAsset :: Device d => d -> B.ByteString -> IO (Geometry d, IO ())
loadGeometryAsset device bytes = case S.decode bytes of
	Right packedGeometry -> loadPackedGeometry device packedGeometry
	Left err -> throwIO $ DescribeFirstException $ "failed to load geometry asset: " ++ err

-- | Create indices for raw vertices.
indexGeometryVertices :: (PrimMonad m, Ord a) => V.Vector a -> m (V.Vector a, VU.Vector Int)
indexGeometryVertices vertices = do
	mVertices <- VG.thaw vertices
	VAI.sort mVertices
	uniqueVertices <- unique mVertices
	indices <- VG.mapM (VAS.binarySearchL uniqueVertices) vertices
	resultVertices <- VG.freeze uniqueVertices
	return (resultVertices, VG.convert indices)

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
