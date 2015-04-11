{-|
Module: Flaw.Asset.Texture
Description: Texture support.
License: MIT
-}

module Flaw.Asset.Texture
	( loadPngTexture
	) where

import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Texture

convertDynamicImage :: DynamicImage -> IO (TextureInfo, B.ByteString)
convertDynamicImage dynamicImage = case dynamicImage of
	ImageRGBA8 Image
		{ imageWidth = width
		, imageHeight = height
		, imageData = v
		} -> do
		let l = V.length v
		p <- mallocArray l
		let vl = V.toList v
		pokeArray p vl
		bytes <- B.unsafePackMallocCStringLen (castPtr p, l * sizeOf (head vl))
		let textureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelRGBA
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel32bit
				, textureFormatColorSpace = StandardColorSpace
				}
			, textureCount = 0
			}
		return (textureInfo, bytes)
	_ -> fail "unsupported dynamic image format"

loadPngTexture :: B.ByteString -> IO (TextureInfo, B.ByteString)
loadPngTexture bytes = do
	case decodePng bytes of
		Right dynamicImage -> convertDynamicImage dynamicImage
		Left err -> fail err
