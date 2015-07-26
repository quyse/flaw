{-|
Module: Flaw.Asset.Util
Description: Useful loading functions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Asset.Util
	( Geometry(..)
	, packGeometry
	, loadGeometry
	) where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.Storable

import Flaw.Asset.Geometry
import Flaw.Book
import Flaw.Build
import Flaw.Graphics

data Geometry = Geometry
	{ geometryVerticesBytes :: !B.ByteString
	, geometryIndicesBytes :: !B.ByteString
	, geometryIndicesCount :: !Int
	, geometryVertexStride :: !Int
	, geometryIsIndices32Bit :: !Bool
	}

genEmbed ''Geometry

packGeometry :: (Ord a, Storable a) => V.Vector a -> IO Geometry
packGeometry rawVertices = do
	(vertices, indices) <- indexVertices rawVertices
	verticesBytes <- packVector vertices
	(isIndices32Bit, indicesBytes) <- do
		if length vertices > 0x10000 then do
			indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word32)
			return (True, indicesBytes)
		else do
			indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word16)
			return (False, indicesBytes)
	return Geometry
		{ geometryVerticesBytes = verticesBytes
		, geometryIndicesBytes = indicesBytes
		, geometryIndicesCount = VG.length indices
		, geometryVertexStride = sizeOf (VG.head rawVertices)
		, geometryIsIndices32Bit = isIndices32Bit
		}

loadGeometry :: Device d => d -> Geometry -> IO ((VertexBufferId d, IndexBufferId d, Int), IO ())
loadGeometry device Geometry
	{ geometryVerticesBytes = verticesBytes
	, geometryIndicesBytes = indicesBytes
	, geometryIndicesCount = indicesCount
	, geometryVertexStride = vertexStride
	, geometryIsIndices32Bit = isIndices32Bit
	} = do
	bk <- newBook
	vertexBuffer <- book bk $ createStaticVertexBuffer device verticesBytes vertexStride
	indexBuffer <- book bk $ createStaticIndexBuffer device indicesBytes isIndices32Bit
	return ((vertexBuffer, indexBuffer, indicesCount), freeBook bk)
