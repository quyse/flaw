{-|
Module: Flaw.Visual.Texture
Description: Texture support.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Flaw.Visual.Texture
	( PackedTexture(..)
	, loadTexture
	, convertTextureToLinearRG
	, emitTextureAsset
	, emitDxtCompressedTextureAsset
	, loadTextureAsset
	) where

import Codec.Picture
import Codec.Picture.Types
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import qualified Data.Serialize as S
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics(Generic)
import Language.Haskell.TH
import System.IO.Unsafe

import Flaw.Asset.Texture.Dxt
import Flaw.Build
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture

data PackedTexture = PackedTexture
	{ packedTextureBytes :: !B.ByteString
	, packedTextureInfo :: !TextureInfo
	} deriving Generic

instance S.Serialize PackedTexture

loadDynamicImageWithFormat :: Storable (PixelBaseComponent a) => Image a -> TextureFormat -> IO PackedTexture
loadDynamicImageWithFormat Image
	{ imageWidth = width
	, imageHeight = height
	, imageData = dataVector
	} format = do
	bytes <- packVector dataVector
	return PackedTexture
		{ packedTextureBytes = bytes
		, packedTextureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = format
			, textureCount = 0
			}
		}

loadDynamicImage :: DynamicImage -> IO PackedTexture
loadDynamicImage dynamicImage = case dynamicImage of
	ImageY8 pixel8Image -> loadDynamicImageWithFormat pixel8Image UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel8bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageY16 pixel16Image -> loadDynamicImageWithFormat pixel16Image UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel16bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYF pixelFImage -> loadDynamicImageWithFormat pixelFImage UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelFloat
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYA8 pixelYA8Image -> loadDynamicImage $ ImageRGBA8 $ promoteImage pixelYA8Image
	ImageYA16 pixelYA16Image -> loadDynamicImage $ ImageRGBA16 $ promoteImage pixelYA16Image
	ImageRGB8 pixelRGB8Image -> loadDynamicImageWithFormat pixelRGB8Image UncompressedTextureFormat
		{ textureFormatComponents = PixelRGB
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel24bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageRGB16 pixelRGB16Image -> loadDynamicImage $ ImageRGBA16 $ promoteImage pixelRGB16Image
	ImageRGBF pixelRGBFImage -> loadDynamicImageWithFormat pixelRGBFImage UncompressedTextureFormat
		{ textureFormatComponents = PixelRGB
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel96bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageRGBA8 pixelRGBA8Image -> loadDynamicImageWithFormat pixelRGBA8Image UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageRGBA16 pixelRGBA16Image -> loadDynamicImageWithFormat pixelRGBA16Image UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel64bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYCbCr8 pixelYCbCr8Image -> loadDynamicImage $ ImageRGB8 $ convertImage pixelYCbCr8Image
	ImageCMYK8 pixelCMYK8Image -> loadDynamicImage $ ImageRGB8 $ convertImage pixelCMYK8Image
	ImageCMYK16 pixelCMYK16Image -> loadDynamicImage $ ImageRGB16 $ convertImage pixelCMYK16Image

loadTexture :: B.ByteString -> IO PackedTexture
loadTexture bytes = do
	case decodeImage bytes of
		Right dynamicImage -> loadDynamicImage dynamicImage
		Left err -> fail err

-- | Convert texture from whatever format to linear RG.
-- Useful for normal map textures.
-- Actual standard->linear conversion is not happening,
-- it's presumed that texture data is in linear space already.
convertTextureToLinearRG :: PackedTexture -> Maybe PackedTexture
convertTextureToLinearRG packedTexture = case packedTexture of
	-- texture is uncompressed RG: just ensure linear color space
	PackedTexture
		{ packedTextureInfo = info@TextureInfo
			{ textureFormat = format@UncompressedTextureFormat
				{ textureFormatComponents = PixelRG
				}
			}
		} -> Just packedTexture
		{ packedTextureInfo = info
			{ textureFormat = format
				{ textureFormatColorSpace = LinearColorSpace
				}
			}
		}
	-- texture is compressed RG: just ensure linear color space
	PackedTexture
		{ packedTextureInfo = info@TextureInfo
			{ textureFormat = format@CompressedTextureFormat
				{ textureFormatCompression = compression
				}
			}
		} | compression == TextureCompressionBC5 || compression == TextureCompressionBC5Signed -> Just packedTexture
		{ packedTextureInfo = info
			{ textureFormat = format
				{ textureFormatColorSpace = LinearColorSpace
				}
			}
		}
	-- texture is uncompressed RGB or RGBA
	PackedTexture
		{ packedTextureBytes = bytes
		, packedTextureInfo = info@TextureInfo
			{ textureFormat = format@UncompressedTextureFormat
				{ textureFormatComponents = components
				, textureFormatPixelSize = pixelSize
				}
			}
		} -> let
		newBytes inSize outSize = unsafePerformIO $ B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> do
			let pixelsCount = len `quot` inSize
			let newLen = pixelsCount * outSize
			newPtr <- mallocBytes newLen
			let loop typedPtr typedNewPtr i = when (i < pixelsCount) $ do
				pixel <- peek typedPtr
				poke typedNewPtr pixel
				loop (typedPtr `plusPtr` inSize) (typedNewPtr `plusPtr` (sizeOf pixel)) $ i + 1
			case outSize of
				2 -> loop (castPtr ptr :: Ptr Word16) (castPtr newPtr) 0
				4 -> loop (castPtr ptr :: Ptr Word32) (castPtr newPtr) 0
				8 -> loop (castPtr ptr :: Ptr Word64) (castPtr newPtr) 0
				_ -> undefined -- cannot happen
			B.unsafePackMallocCStringLen (newPtr, newLen)
		newTexture inSize outSize outPixelSize = packedTexture
			{ packedTextureBytes = newBytes inSize outSize
			, packedTextureInfo = info
				{ textureFormat = format
					{ textureFormatComponents = PixelRG
					, textureFormatPixelSize = outPixelSize
					, textureFormatColorSpace = LinearColorSpace
					}
				}
			}
		in case (components, pixelSize) of
			(PixelRGB, Pixel24bit) -> Just $ newTexture 3 2 Pixel16bit
			(PixelRGB, Pixel96bit) -> Just $ newTexture 12 8 Pixel64bit
			(PixelRGBA, Pixel32bit) -> Just $ newTexture 4 2 Pixel16bit
			(PixelRGBA, Pixel64bit) -> Just $ newTexture 8 4 Pixel32bit
			(PixelRGBA, Pixel128bit) -> Just $ newTexture 16 8 Pixel64bit
			_ -> Nothing
	-- others are unsupported
	_ -> Nothing

emitTextureAsset :: FilePath -> Q B.ByteString
emitTextureAsset fileName = do
	bytes <- loadFile fileName
	fmap S.encode $ runIO $ loadTexture $ BL.toStrict bytes

emitDxtCompressedTextureAsset :: FilePath -> Q B.ByteString
emitDxtCompressedTextureAsset fileName = do
	bytes <- loadFile fileName
	PackedTexture
		{ packedTextureBytes = textureBytes
		, packedTextureInfo = textureInfo
		} <- runIO $ loadTexture $ BL.toStrict bytes
	(compressedTextureInfo, compressedTextureBytes) <- runIO $ dxtCompressTexture textureInfo textureBytes
	return $ S.encode PackedTexture
		{ packedTextureBytes = compressedTextureBytes
		, packedTextureInfo = compressedTextureInfo
		}

loadTextureAsset :: Device d => d -> B.ByteString -> IO (TextureId d, IO ())
loadTextureAsset device bytes = case S.decode bytes of
	Right PackedTexture
		{ packedTextureBytes = textureBytes
		, packedTextureInfo = textureInfo
		} -> createStaticTexture device textureInfo defaultSamplerStateInfo textureBytes
	Left err -> throwIO $ DescribeFirstException $ "failed to load texture asset: " ++ err
