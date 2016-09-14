{-|
Module: Flaw.Graphics.Font.Util
Description: Helper functions for fonts and glyphs.
License: MIT
-}

module Flaw.Graphics.Font.Util
	( GlyphUnionConfig(..)
	, uniteGlyphs
	, makeScaledGlyphs
	) where

import Codec.Picture
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Font
import Flaw.Graphics.Texture

data GlyphUnionConfig = GlyphUnionConfig
	{ glyphUnionConfigWidth :: !Int
	, glyphUnionConfigBorder :: !Int
	, glyphUnionConfigHeightIsPowerOfTwo :: !Bool
	}

uniteGlyphs :: V.Vector (Image Pixel8, GlyphInfo) -> GlyphUnionConfig -> IO (Image Pixel8, V.Vector GlyphInfo)
uniteGlyphs glyphImagesAndInfos unionConfig = do
	(unitedImage, positions) <- unite (V.map fst glyphImagesAndInfos) unionConfig
	let setPosition (_image, info) (x, y) = info
		{ glyphLeftTopX = x
		, glyphLeftTopY = y
		}
	return (unitedImage, V.zipWith setPosition glyphImagesAndInfos positions)

makeScaledGlyphs :: (Int -> Int -> IO (V.Vector (Image Pixel8, GlyphInfo))) -> Int -> Int -> GlyphUnionConfig -> IO Glyphs
makeScaledGlyphs createGlyphsAndInfos halfScaleX halfScaleY unionConfig = do
	glyphsAndInfos <- createGlyphsAndInfos halfScaleX halfScaleY
	(Image
		{ imageWidth = width
		, imageHeight = height
		, imageData = pixels
		}, infos) <- uniteGlyphs glyphsAndInfos unionConfig
	textureData <- VS.unsafeWith pixels $ \pixelsPtr -> B.packCStringLen (castPtr pixelsPtr, VS.length pixels * sizeOf (VS.head pixels))
	return Glyphs
		{ glyphsTextureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelR
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel8bit
				, textureFormatColorSpace = LinearColorSpace
				}
			, textureCount = 0
			}
		, glyphsTextureData = textureData
		, glyphsInfos = infos
		, glyphsScaleX = 1 + halfScaleX * 2
		, glyphsScaleY = 1 + halfScaleY * 2
		}

data UnionState = UnionState
	{ stateCurrentX :: !Int -- ^ X of left-top corner of next image.
	, stateCurrentY :: !Int -- ^ Y of left-top corner of next image.
	, stateCurrentRowHeight :: !Int -- ^ Height of current row.
	}

unite :: Pixel a => V.Vector (Image a) -> GlyphUnionConfig -> IO (Image a, V.Vector (Int, Int))
unite images GlyphUnionConfig
	{ glyphUnionConfigWidth = resultWidth
	, glyphUnionConfigBorder = border
	, glyphUnionConfigHeightIsPowerOfTwo = heightIsPowerOfTwo
	} = result where
	-- sort images by height
	sortedImages = sortOn (\i -> imageHeight $ images V.! i) [0..(V.length images - 1)]
	-- fold function to calculate positions
	calcPosition imageIndex (restPositions, state) = (newPosition : restPositions, newState) where
		Image
			{ imageWidth = width
			, imageHeight = height
			} = images V.! imageIndex
		UnionState
			{ stateCurrentX = currentX
			, stateCurrentY = currentY
			, stateCurrentRowHeight = currentRowHeight
			} = state
		overflow = currentX + width + border > resultWidth
		newState = UnionState
			{ stateCurrentX = if overflow then border + width + border else currentX + width + border
			, stateCurrentY = if overflow then currentY + currentRowHeight + border else currentY
			, stateCurrentRowHeight = if overflow then height else max currentRowHeight height
			}
		newPosition = if overflow then (border, currentY + currentRowHeight + border) else (currentX, currentY)
	(positions, UnionState
		{ stateCurrentY = lastCurrentY
		, stateCurrentRowHeight = lastRowHeight
		}) = foldr calcPosition ([], UnionState
		{ stateCurrentX = border
		, stateCurrentY = border
		, stateCurrentRowHeight = 0
		}) sortedImages
	rawResultHeight = if lastRowHeight > 0 then lastCurrentY + lastRowHeight + border else lastCurrentY
	resultHeight = if heightIsPowerOfTwo then powerOfTwo rawResultHeight 1 else ((rawResultHeight + 3) `quot` 4) * 4
	powerOfTwo n p = if n <= p then p else powerOfTwo n $ p * 2
	result = do
		-- create united image and reorder positions to initial (unsorted) order
		resultImageData <- VSM.replicate (resultWidth * resultHeight) 0
		resultPositions <- VM.new (V.length images)
		VSM.unsafeWith resultImageData $ \resultPtr ->
			forM_ (zip sortedImages positions) $ \(imageIndex, position@(positionX, positionY)) -> do
				let Image
					{ imageWidth = width
					, imageHeight = height
					, imageData = sourceImageData
					} = images V.! imageIndex
				VS.unsafeWith sourceImageData $ \sourcePtr ->
					-- put image to result
					forM_ [0..(height - 1)] $ \i ->
						copyArray
							(advancePtr resultPtr ((positionY + i) * resultWidth + positionX)) -- destination
							(advancePtr sourcePtr (i * width)) -- source
							width -- count
				VM.write resultPositions imageIndex position
		freezedResultImageData <- VS.unsafeFreeze resultImageData
		freezedResultPositions <- V.unsafeFreeze resultPositions

		return (Image
			{ imageWidth = resultWidth
			, imageHeight = resultHeight
			, imageData = freezedResultImageData
			}, freezedResultPositions)
