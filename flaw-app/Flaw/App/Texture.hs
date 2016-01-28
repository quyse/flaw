{-|
Module: Flaw.App.Texture
Description: Texture loading.
License: MIT
-}

{-# LANGUAGE CPP, TemplateHaskell #-}

module Flaw.App.Texture
	( loadTextureExp
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
import Flaw.Graphics.Sampler
import Flaw.Visual.Texture

#endif

-- | Create expression for loading texture.
-- Expression will be of type:
-- :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
loadTextureExp :: FilePath -> Q Exp
loadTextureExp filePath = do

#if defined(ghcjs_HOST_OS)

	[| \device -> loadWebGLTexture2DFromURL device $(embedExp $ T.pack filePath) |]

#else

	fileData <- loadFile filePath
	(textureInfo, fileBytes) <- runIO $ loadTexture $ BL.toStrict fileData
	[|
		\device -> do
			bytes <- liftIO $(embedIOExp fileBytes)
			createStaticTexture device $(embedExp textureInfo) defaultSamplerStateInfo bytes
		|]

#endif
