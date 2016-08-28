{-|
Module: Flaw.Asset.FolderAssetPack
Description: Asset pack loading assets from files (or via AJAX request in case of web).
License: MIT
-}

{-# LANGUAGE CPP, JavaScriptFFI, TemplateHaskell, TypeFamilies #-}

module Flaw.Asset.FolderAssetPack
	( FolderAssetPack(..)
	, AssetPackBuilder(..)
	) where

import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T

import Flaw.Asset
import Flaw.Build

#ifdef ghcjs_HOST_OS
import Data.JSString.Text
import GHCJS.Marshal.Pure

import Flaw.Js
#endif

-- | The simpliest asset pack loading files just from folder (or URL prefix).
newtype FolderAssetPack = FolderAssetPack T.Text

genEmbed ''FolderAssetPack

instance AssetPack FolderAssetPack where
	type AssetId FolderAssetPack = T.Text

	loadAsset (FolderAssetPack prefix) fileName = do
		let fullFileName = prefix <> fileName
#ifdef ghcjs_HOST_OS
		-- get asset by url, convert to bytestring
		arrayBufferToByteString <$> arrayBufferFromUrl (textToJSString fullFileName)
#else
		-- just load file
		B.readFile $ T.unpack fullFileName
#endif

	newtype AssetPackBuilder FolderAssetPack = FolderAssetPackBuilder T.Text
	putAsset (FolderAssetPackBuilder prefix) fileName = B.writeFile (T.unpack $ prefix <> fileName)

instance WebAssetPack FolderAssetPack where
	getWebAssetUrl (FolderAssetPack prefix) fileName = return $ prefix <> fileName
