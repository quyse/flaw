{-|
Module: Flaw.Asset.FolderAssetPack
Description: Asset pack loading assets from files (or via AJAX request in case of web).
License: MIT
-}

{-# LANGUAGE CPP, JavaScriptFFI, TypeFamilies #-}

module Flaw.Asset.FolderAssetPack
	( FolderAssetPack(..)
	, AssetPackBuilder(..)
	) where

import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import qualified GHCJS.Buffer
import GHCJS.Foreign
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified JavaScript.TypedArray.ArrayBuffer as J
#endif

import Flaw.Asset

-- | The simpliest asset pack loading files just from folder (or URL prefix).
newtype FolderAssetPack = FolderAssetPack T.Text

instance AssetPack FolderAssetPack where
	type AssetId FolderAssetPack = T.Text

	loadAsset (FolderAssetPack prefix) fileName = do
		let fullFileName = prefix <> fileName
#ifdef ghcjs_HOST_OS
		-- get asset by url
		buffer <- js_getAsset $ pToJSVal fullFileName
		-- convert to bytestring
		return $ GHCJS.Buffer.toByteString 0 Nothing $ GHCJS.Buffer.createFromArrayBuffer buffer
#else
		-- just load file
		B.readFile $ T.unpack fullFileName
#endif

	newtype AssetPackBuilder FolderAssetPack = FolderAssetPackBuilder T.Text
	putAsset (FolderAssetPackBuilder prefix) fileName asset = B.writeFile (T.unpack $ prefix <> fileName) asset

instance WebAssetPack FolderAssetPack where
	getWebAssetUrl (FolderAssetPack prefix) fileName = return $ prefix <> fileName

#ifdef ghcjs_HOST_OS
foreign import javascript interruptible "h$flaw_asset_get_asset($1, $c);" js_getAsset :: JSVal -> IO J.ArrayBuffer
#endif
