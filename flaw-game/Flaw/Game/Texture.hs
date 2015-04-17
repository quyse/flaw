{-|
Module: Flaw.Game.Texture
Description: Texture loading.
License: MIT
-}

{-# LANGUAGE CPP, TemplateHaskell #-}

module Flaw.Game.Texture
	( loadPngTextureExp
	) where

import Language.Haskell.TH

import Flaw.Build

#if defined(ghcjs_HOST_OS)

import qualified Data.Text as T

import Flaw.Graphics.WebGL

#else

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL

import Flaw.Graphics
import Flaw.Graphics.Texture
import Flaw.Asset.Texture

#endif

-- | Create expression for loading texture.
-- Expression will be of type:
-- :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m (ReleaseKey, TextureId GameGraphicsDevice)
loadPngTextureExp :: FilePath -> Q Exp
loadPngTextureExp filePath = do

#if defined(ghcjs_HOST_OS)

	[| \device -> loadWebGLTexture2DFromURL device $(embedExp $ T.pack filePath) |]

#else

	fileData <- loadFile filePath
	(ti, fileBytes) <- runIO $ loadPngTexture $ BL.toStrict fileData
	[|
		\device -> do
			bytes <- liftIO $(embedIOExp fileBytes)
			createStaticTexture device TextureInfo
				{ textureWidth = $(embedExp $ textureWidth ti)
				, textureHeight = $(embedExp $ textureHeight ti)
				, textureDepth = 0
				, textureMips = 1
				, textureFormat = UncompressedTextureFormat
					{ textureFormatComponents = PixelRGBA
					, textureFormatValueType = PixelUint
					, textureFormatPixelSize = Pixel32bit
					, textureFormatColorSpace = LinearColorSpace
					}
				, textureCount = 0
				} bytes
		|]

#endif
