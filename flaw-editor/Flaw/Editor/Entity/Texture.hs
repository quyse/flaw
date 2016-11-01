{-|
Module: Flaw.Editor.Entity.Texture
Description: Texture entities.
License: MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Editor.Entity.Texture
	(
	) where

import qualified Data.ByteString as B
import Data.Default
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Flaw.Asset.Texture.Dxt
import Flaw.Editor.Entity
import Flaw.Editor.Entity.Blob
import Flaw.Editor.Processing
import Flaw.Graphics.Texture
import Flaw.Visual.Texture

-- | Interface for entities representing textures.
class (ProcessableEntity a, ProcessableEntityResult a ~ PackedTexture) => ITexture a

instance EntityInterface ITexture where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "ITexture")

-- 'PackedTexture' is itself an entity and a texture.
instance Entity PackedTexture where
	type EntityChange PackedTexture = PackedTexture
	getEntityTypeId _ = $(hashTextToEntityTypeId "PackedTexture")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
	entityToText PackedTexture
		{ packedTextureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = depth
			, textureMips = mips
			, textureFormat = format
			, textureCount = count
			}
		} = TL.toStrict $ TLB.toLazyText $ "texture:"
		<> formatStr <> "_"
		<> TLB.fromString (show width)
		<> (if height > 0 then TLB.fromString (show height) else mempty)
		<> (if depth > 0 then TLB.fromString (show depth) else mempty)
		<> (if mips > 1 then "M" <> TLB.fromString (show mips) else mempty)
		<> (if count > 0 then "[" <> TLB.fromString (show count) <> "]" else mempty)
		where
			formatStr = case format of
				UncompressedTextureFormat
					{ textureFormatComponents = components
					, textureFormatValueType = valueType
					, textureFormatPixelSize = pixelSize
					} -> let
					componentsStr = case components of
						PixelR -> "R"
						PixelRG -> "RG"
						PixelRGB -> "RGB"
						PixelRGBA -> "RGBA"
					valueTypeStr = case valueType of
						PixelUntyped -> "?"
						PixelUint -> "U"
						PixelFloat -> "F"
					pixelSizeStr = case pixelSize of
						Pixel8bit -> "8b"
						Pixel16bit -> "16b"
						Pixel24bit -> "24b"
						Pixel32bit -> "32b"
						Pixel64bit -> "64b"
						Pixel96bit -> "96b"
						Pixel128bit -> "128b"
					in colorSpaceStr <> componentsStr <> valueTypeStr <> pixelSizeStr
				CompressedTextureFormat
					{ textureFormatCompression = compression
					} -> let
					compressionStr = case compression of
						TextureCompressionBC1 -> "BC1"
						TextureCompressionBC1Alpha -> "BC1A"
						TextureCompressionBC2 -> "BC2"
						TextureCompressionBC3 -> "BC3"
						TextureCompressionBC4 -> "BC4"
						TextureCompressionBC4Signed -> "BC4S"
						TextureCompressionBC5 -> "BC5"
						TextureCompressionBC5Signed -> "BC5S"
						in colorSpaceStr <> "_" <> compressionStr
			colorSpaceStr = case textureFormatColorSpace format of
				LinearColorSpace -> "L"
				StandardColorSpace -> "S"

instance Default PackedTexture where
	def = PackedTexture
		{ packedTextureBytes = B.empty
		, packedTextureInfo = TextureInfo
			{ textureWidth = 0
			, textureHeight = 0
			, textureDepth = 0
			, textureMips = 0
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelRGBA
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel32bit
				, textureFormatColorSpace = LinearColorSpace
				}
			, textureCount = 0
			}
		}
instance BasicEntity PackedTexture
instance ProcessableEntity PackedTexture
instance ITexture PackedTexture

-- | Entity importing texture from blob.
newtype TextureFromBlob = TextureFromBlob (InterfacedEntityPtr IBlob) deriving (S.Serialize, Default)
instance Entity TextureFromBlob where
	type EntityChange TextureFromBlob = TextureFromBlob
	getEntityTypeId _ = $(hashTextToEntityTypeId "TextureFromBlob")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
	entityToText (TextureFromBlob ptr) = "texture:" <> entityToText ptr
instance BasicEntity TextureFromBlob
instance ProcessableEntity TextureFromBlob where
	type ProcessableEntityResult TextureFromBlob = PackedTexture
	processEntity (TextureFromBlob blobPtr) = do
		SomeInterfacedEntity blob <- readInterfacedEntityPtr blobPtr
		loadTexture <$> processEntity blob
instance ITexture TextureFromBlob

-- | Entity compressing texture.
newtype CompressTexture = CompressTexture (InterfacedEntityPtr ITexture) deriving (S.Serialize, Default)
instance Entity CompressTexture where
	type EntityChange CompressTexture = CompressTexture
	getEntityTypeId _ = $(hashTextToEntityTypeId "CompressTexture")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
	entityToText (CompressTexture ptr) = "compress_texture:" <> entityToText ptr
instance BasicEntity CompressTexture
instance ProcessableEntity CompressTexture where
	type ProcessableEntityResult CompressTexture = PackedTexture
	processEntity (CompressTexture texturePtr) = do
		SomeInterfacedEntity sourceTextureEntity <- readInterfacedEntityPtr texturePtr
		PackedTexture
			{ packedTextureBytes = sourceTextureBytes
			, packedTextureInfo = sourceTextureInfo
			} <- processEntity sourceTextureEntity
		let (compressedTextureInfo, compressedTextureBytes) = dxtCompressTexture sourceTextureInfo sourceTextureBytes
		return PackedTexture
			{ packedTextureBytes = compressedTextureBytes
			, packedTextureInfo = compressedTextureInfo
			}
instance ITexture CompressTexture
