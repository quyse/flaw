{-|
Module: Flaw.Graphics.Font.Util
Description: Helper functions for fonts and glyphs.
License: MIT
-}

{-# LANGUAGE ViewPatterns #-}

module Flaw.Graphics.Font.Util
	( GlyphUnionConfig(..)
	, makeScaledGlyphs
	) where

import Codec.Picture
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Font
import Flaw.Graphics.Texture

data GlyphUnionConfig = GlyphUnionConfig
	{ glyphUnionConfigWidth :: {-# UNPACK #-} !Int
	, glyphUnionConfigBorderX :: {-# UNPACK #-} !Int
	, glyphUnionConfigBorderY :: {-# UNPACK #-} !Int
	, glyphUnionConfigHeightIsPowerOfTwo :: !Bool
	}

makeScaledGlyphs :: (Int -> Int -> [Int] -> IO (HM.HashMap Int (Image Pixel8, GlyphInfo))) -> Int -> Int -> GlyphUnionConfig -> [Int] -> IO Glyphs
makeScaledGlyphs createGlyphsAndInfos halfScaleX halfScaleY unionConfig glyphsIndices = do
	glyphsAndInfos <- createGlyphsAndInfos halfScaleX halfScaleY glyphsIndices
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
	{ stateCurrentX :: {-# UNPACK #-} !Int -- ^ X of left-top corner of next image.
	, stateCurrentY :: {-# UNPACK #-} !Int -- ^ Y of left-top corner of next image.
	, stateCurrentRowHeight :: {-# UNPACK #-} !Int -- ^ Height of current row.
	}

uniteGlyphs :: HM.HashMap Int (Image Pixel8, GlyphInfo) -> GlyphUnionConfig -> IO (Image Pixel8, HM.HashMap Int GlyphInfo)
uniteGlyphs images GlyphUnionConfig
	{ glyphUnionConfigWidth = resultWidth
	, glyphUnionConfigBorderX = borderX
	, glyphUnionConfigBorderY = borderY
	, glyphUnionConfigHeightIsPowerOfTwo = heightIsPowerOfTwo
	} = result where
	-- sort images by height
	sortedImages = sortOn (\(_, (imageHeight -> h, _)) -> h) $ HM.toList images
	-- fold function to calculate glyph infos with corrected positions
	calcPosition UnionState
			{ stateCurrentX = currentX
			, stateCurrentY = currentY
			, stateCurrentRowHeight = currentRowHeight
			} ((imageIndex, (image@Image
			{ imageWidth = width
			, imageHeight = height
			}, glyphInfo)) : restImages) = ((imageIndex, (image, newGlyphInfo)) : nextPositions, lastState) where
		(nextPositions, lastState) = calcPosition newState restImages
		overflow = currentX + width + borderX > resultWidth
		newState = UnionState
			{ stateCurrentX = if overflow then borderX + width + borderX else currentX + width + borderX
			, stateCurrentY = if overflow then currentY + currentRowHeight + borderY else currentY
			, stateCurrentRowHeight = if overflow then height else max currentRowHeight height
			}
		newGlyphInfo = glyphInfo
			{ glyphLeftTopX = if overflow then borderX else currentX
			, glyphLeftTopY = if overflow then currentY + currentRowHeight + borderY else currentY
			}
	calcPosition state [] = ([], state)
	(orderedImages, UnionState
		{ stateCurrentY = lastCurrentY
		, stateCurrentRowHeight = lastRowHeight
		}) = calcPosition UnionState
		{ stateCurrentX = borderX
		, stateCurrentY = borderY
		, stateCurrentRowHeight = 0
		} sortedImages
	rawResultHeight = if lastRowHeight > 0 then lastCurrentY + lastRowHeight + borderY else lastCurrentY
	resultHeight = if heightIsPowerOfTwo then powerOfTwo rawResultHeight 1 else ((rawResultHeight + 3) `quot` 4) * 4
	powerOfTwo n p = if n <= p then p else powerOfTwo n $ p * 2
	result = do
		-- create united image and final hashmap
		resultImageData <- VSM.replicate (resultWidth * resultHeight) 0
		resultGlyphs <- VSM.unsafeWith resultImageData $ \resultPtr ->
			forM orderedImages $ \(imageIndex, (Image
				{ imageWidth = width
				, imageHeight = height
				, imageData = sourceImageData
				}, glyphInfo@GlyphInfo
				{ glyphLeftTopX = positionX
				, glyphLeftTopY = positionY
				})) -> do
				VS.unsafeWith sourceImageData $ \sourcePtr ->
					-- put image to result
					forM_ [0..(height - 1)] $ \i ->
						copyArray
							(advancePtr resultPtr ((positionY + i) * resultWidth + positionX)) -- destination
							(advancePtr sourcePtr (i * width)) -- source
							width -- count
				return (imageIndex, glyphInfo)
		freezedResultImageData <- VS.unsafeFreeze resultImageData

		return (Image
			{ imageWidth = resultWidth
			, imageHeight = resultHeight
			, imageData = freezedResultImageData
			}, HM.fromList resultGlyphs)
