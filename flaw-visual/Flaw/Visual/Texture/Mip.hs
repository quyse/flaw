{-|
Module: Flaw.Visual.Texture.Mip
Description: Mipmap generation support.
License: MIT
-}

{-# LANGUAGE Strict #-}

module Flaw.Visual.Texture.Mip
  ( generateMips
  ) where

import Control.Monad
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Unsafe as B
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Visual.Texture.Internal

-- | Generate specified amount (0 means full chain) of mipmap levels for a texture.
-- All existing mipmap levels except top one are removed.
generateMips :: Int -> PackedTexture -> PackedTexture
generateMips mipsRequested PackedTexture
  { packedTextureBytes = bytes
  , packedTextureInfo = textureInfo@TextureInfo
    { textureWidth = width
    , textureHeight = height
    , textureDepth = depth
    , textureFormat = UncompressedTextureFormat
      { textureFormatComponents = pixelComponents
      , textureFormatValueType = pixelValueType
      , textureFormatPixelSize = pixelSize
      }
    , textureCount = count
    }
  } = PackedTexture
  { packedTextureBytes = newBytes
  , packedTextureInfo = newTextureInfo
  } where
  -- mips to generate
  mips = if mipsRequested > 0 then mipsRequested else let
    maxMip mip = if (width `shiftR` mip) > 1 || (height `shiftR` mip) > 1 || (depth `shiftR` mip) > 1 then maxMip (mip + 1) else mip + 1
    in maxMip 0

  -- new texture info
  newTextureInfo = textureInfo
    { textureMips = mips
    }

  -- calculate metrics
  TextureMetrics
    { textureImageSize = imageSize
    } = calcTextureMetrics textureInfo
  TextureMetrics
    { textureImageSize = newImageSize
    , textureMipsMetrics = mipsMetrics@(TextureMipMetrics
      { textureMipWidth = topMipWidth
      , textureMipHeight = topMipHeight
      , textureMipDepth = topMipDepth
      , textureMipLinePitch = topMipLinePitch
      , textureMipSlicePitch = topMipSlicePitch
      } : _)
    } = calcTextureMetrics newTextureInfo
  pixelPitch = pixelSizeByteSize pixelSize

  -- number of images
  ncount = if count > 0 then count else 1

  -- downscale function
  downscale :: (Storable a, Num b) => (a -> b) -> (Int -> b -> a) -> TextureMipMetrics -> Ptr a -> Ptr a -> IO ()
  downscale fromSource toSource TextureMipMetrics
    { textureMipWidth = mipWidth
    , textureMipHeight = mipHeight
    , textureMipDepth = mipDepth
    , textureMipLinePitch = mipLinePitch
    , textureMipSlicePitch = mipSlicePitch
    } sourcePtr destPtr = let
    zLoop z = if z >= mipDepth then return () else let
      z1 = (z * topMipDepth) `quot` mipDepth
      z2 = ((z + 1) * topMipDepth) `quot` mipDepth
      yLoop y = if y >= mipHeight then return () else let
        y1 = (y * topMipHeight) `quot` mipHeight
        y2 = ((y + 1) * topMipHeight) `quot` mipHeight
        xLoop x = if x >= mipWidth then return () else let
          x1 = (x * topMipWidth) `quot` mipWidth
          x2 = ((x + 1) * topMipWidth) `quot` mipWidth
          szLoop sz tz = if sz >= z2 then return tz else let
            syLoop sy ty = if sy >= y2 then return ty else let
              sxLoop sx tx = if sx >= x2 then return tx else do
                s <- peekByteOff sourcePtr $ sz * topMipSlicePitch + sy * topMipLinePitch + sx * pixelPitch
                sxLoop (sx + 1) (tx + fromSource s)
              in syLoop (sy + 1) =<< sxLoop x1 ty
            in szLoop (sz + 1) =<< syLoop y1 tz
          in do
            s <- szLoop z1 0
            pokeByteOff destPtr (z * mipSlicePitch + y * mipLinePitch + x * pixelPitch) $ toSource ((x2 - x1) * (y2 - y1) * (z2 - z1)) s
            xLoop $ x + 1
        in do
          xLoop 0
          yLoop $ y + 1
      in do
        yLoop 0
        zLoop $ z + 1
    in zLoop 0

  -- poly-format mip generation function
  genMip mipMetrics sourcePtr destPtr = case (pixelComponents, pixelValueType, pixelSize) of
    (PixelR, PixelUint, Pixel8bit) -> downscale (fromIntegral :: Word8 -> Word32) (\n b -> fromIntegral (b `quot` fromIntegral n)) mipMetrics (castPtr sourcePtr) (castPtr destPtr)
    (PixelRG, PixelUint, Pixel16bit) -> downscale (vecfmap fromIntegral :: Word8_2 -> Word32_2) (\n -> vecfmap (fromIntegral . (`quot` fromIntegral n))) mipMetrics (castPtr sourcePtr) (castPtr destPtr)
    (PixelRGB, PixelUint, Pixel24bit) -> downscale (vecfmap fromIntegral :: Word8_3 -> Word32_3) (\n -> vecfmap (fromIntegral . (`quot` fromIntegral n))) mipMetrics (castPtr sourcePtr) (castPtr destPtr)
    (PixelRGBA, PixelUint, Pixel32bit) -> downscale (vecfmap fromIntegral :: Word8_4 -> Word32_4) (\n -> vecfmap (fromIntegral . (`quot` fromIntegral n))) mipMetrics (castPtr sourcePtr) (castPtr destPtr)
    _ -> error $ "unsupported texture format for mipmap generation: " ++ show (pixelComponents, pixelValueType, pixelSize)

  -- bytes
  newBytes = BA.allocAndFreeze (newImageSize * ncount) $ \newBytesPtr -> B.unsafeUseAsCString bytes $ \bytesPtr ->
    -- loop for textures in the array
    forM_ [0..(ncount - 1)] $ \c -> do
      let imageSourcePtr = bytesPtr `plusPtr` (c * imageSize)
      let imageDestPtr = newBytesPtr `plusPtr` (c * newImageSize)
      -- loop for mips
      forM_ mipsMetrics $ \mipMetrics@TextureMipMetrics
        { textureMipOffset = mipOffset
        } -> genMip mipMetrics imageSourcePtr (imageDestPtr `plusPtr` mipOffset)

generateMips _ _ = error "compressed textures are not supported for mipmap generation"
