{-|
Module: Flaw.Asset.Wai
Description: Asset pack embedding assets into WAI static app, and serving them via http.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Asset.Wai
  ( AssetPackBuilder(..)
  , newWaiAssetPackBuilder
  , finalizeWaiAssetPackBuilder
  ) where

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash as C
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.TH
import Network.Mime
import qualified WaiAppStatic.Storage.Embedded as W

import Flaw.Asset
import Flaw.Asset.FolderAssetPack

data WaiAssetPack

instance AssetPack WaiAssetPack where
  type AssetId WaiAssetPack = T.Text
  loadAsset = undefined

  data AssetPackBuilder WaiAssetPack
    = WaiAssetPackBuilder
      !(IORef [W.EmbeddableEntry])
      !T.Text
  putAsset (WaiAssetPackBuilder entriesRef prefix) assetId asset = do
    let etag = T.decodeUtf8 $ BA.convertToBase BA.Base64URLUnpadded (C.hash asset :: C.Digest C.SHA256)
    modifyIORef' entriesRef $ (:) W.EmbeddableEntry
      { W.eLocation = prefix <> assetId
      , W.eMimeType = defaultMimeLookup assetId
      , W.eContent = Left (etag, BL.fromStrict asset)
      }

-- | Create new WAI asset pack builder.
newWaiAssetPackBuilder :: T.Text -> IO (AssetPackBuilder WaiAssetPack)
newWaiAssetPackBuilder baseUrl = do
  entriesRef <- newIORef []
  return $ WaiAssetPackBuilder entriesRef baseUrl

-- | Finalize WAI asset pack builder.
finalizeWaiAssetPackBuilder :: AssetPackBuilder WaiAssetPack -> ExpQ
finalizeWaiAssetPackBuilder (WaiAssetPackBuilder entriesRef _prefix) = W.mkSettings $ readIORef entriesRef
