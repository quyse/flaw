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

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Default
import qualified Data.Serialize as S

import Flaw.Asset.Texture.Dxt
import Flaw.Editor.Entity.Blob
import Flaw.Editor.Processing
import Flaw.Graphics.Texture
import Flaw.Oil.Entity
import Flaw.Visual.Texture

-- | Interface for entities representing textures.
class (ProcessableEntity a, ProcessableEntityResult a ~ PackedTexture) => ITexture a

instance EntityInterface ITexture where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "ITexture")

-- 'PackedTexture' is itself an entity and a texture.
instance Entity PackedTexture where
	getEntityTypeId _ = $(hashTextToEntityTypeId "PackedTexture")
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
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
	getEntityTypeId _ = $(hashTextToEntityTypeId "TextureFromBlob")
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
instance BasicEntity TextureFromBlob
instance ProcessableEntity TextureFromBlob where
	type ProcessableEntityResult TextureFromBlob = PackedTexture
	processEntity (TextureFromBlob blobPtr) = do
		SomeInterfacedEntity blob <- readInterfacedEntityPtr blobPtr
		blobBytes <- processEntity blob
		liftIO $ loadTexture blobBytes
instance ITexture TextureFromBlob

-- | Entity compressing texture.
newtype CompressTexture = CompressTexture (InterfacedEntityPtr ITexture) deriving (S.Serialize, Default)
instance Entity CompressTexture where
	getEntityTypeId _ = $(hashTextToEntityTypeId "CompressTexture")
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''ITexture])
instance BasicEntity CompressTexture
instance ProcessableEntity CompressTexture where
	type ProcessableEntityResult CompressTexture = PackedTexture
	processEntity (CompressTexture texturePtr) = do
		SomeInterfacedEntity sourceTextureEntity <- readInterfacedEntityPtr texturePtr
		PackedTexture
			{ packedTextureBytes = sourceTextureBytes
			, packedTextureInfo = sourceTextureInfo
			} <- processEntity sourceTextureEntity
		(compressedTextureInfo, compressedTextureBytes) <- liftIO $ dxtCompressTexture sourceTextureInfo sourceTextureBytes
		return PackedTexture
			{ packedTextureBytes = compressedTextureBytes
			, packedTextureInfo = compressedTextureInfo
			}
instance ITexture CompressTexture
