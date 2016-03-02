{-|
Module: Flaw.App.PlainTexture
Description: Plain texture asset operations.
License: MIT
-}

{-# LANGUAGE CPP #-}

module Flaw.App.PlainTexture
	( emitPlainTextureAsset
	, loadPlainTextureAsset
	) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Language.Haskell.TH

import Flaw.App
import Flaw.Asset
import Flaw.Build
import Flaw.Graphics

#if defined(ghcjs_HOST_OS)

import Flaw.Graphics.WebGL

#else

import Flaw.Graphics.Sampler
import Flaw.Visual.Texture

#endif

-- | Emit texture asset suitable for direct loading in web.
emitPlainTextureAsset :: FilePath -> Q B.ByteString
emitPlainTextureAsset fileName = liftM BL.toStrict $ loadFile fileName

loadPlainTextureAsset :: WebAssetPack ap => AppGraphicsDevice -> ap -> AssetId ap -> IO (TextureId AppGraphicsDevice, IO ())
loadPlainTextureAsset device assetPack assetId = do
#ifdef ghcjs_HOST_OS
	loadWebGLTexture2DFromURL device =<< getWebAssetUrl assetPack assetId
#else
	(textureInfo, textureBytes) <- loadTexture =<< loadAsset assetPack assetId
	createStaticTexture device textureInfo defaultSamplerStateInfo textureBytes
#endif
