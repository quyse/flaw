{-|
Module: Flaw.Visual.Texture
Description: Texture support.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Flaw.Visual.Texture
	( PackedTexture(..)
	, loadTexture
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
import qualified Data.Serialize as S
import Foreign.Storable
import GHC.Generics(Generic)
import Language.Haskell.TH

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

emitTextureAsset :: FilePath -> Q B.ByteString
emitTextureAsset fileName = do
	bytes <- loadFile fileName
	liftM S.encode $ runIO $ loadTexture $ BL.toStrict bytes

emitDxtCompressedTextureAsset :: FilePath -> Q B.ByteString
emitDxtCompressedTextureAsset fileName = do
	bytes <- loadFile fileName
	PackedTexture
		{ packedTextureBytes = textureBytes
		, packedTextureInfo = textureInfo
		} <- runIO $ loadTexture $ BL.toStrict bytes
	liftM S.encode $ runIO $ dxtCompressTexture textureInfo textureBytes

loadTextureAsset :: Device d => d -> B.ByteString -> IO (TextureId d, IO ())
loadTextureAsset device bytes = case S.decode bytes of
	Right PackedTexture
		{ packedTextureBytes = textureBytes
		, packedTextureInfo = textureInfo
		} -> createStaticTexture device textureInfo defaultSamplerStateInfo textureBytes
	Left err -> throwIO $ DescribeFirstException $ "failed to load texture asset: " ++ err
