{-|
Module: Flaw.Asset.HashMapAssetPack
Description: Asset pack using simple in-memory map.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.Asset.HashMapAssetPack
  ( HashMapAssetPack(..)
  , AssetPackBuilder(..)
  , newHashMapAssetPackBuilder
  , finalizeHashMapAssetPack
  ) where

import Control.Exception
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Serialize as S
import Data.Typeable
import Language.Haskell.TH

import Flaw.Asset
import Flaw.Build

newtype HashMapAssetPack k = HashMapAssetPack (HM.HashMap k B.ByteString)

instance (Eq k, Hashable k, Typeable k, Show k) => AssetPack (HashMapAssetPack k) where
  type AssetId (HashMapAssetPack k) = k

  loadAsset (HashMapAssetPack assets) assetId = case HM.lookup assetId assets of
    Just asset -> return asset
    Nothing -> throwIO AssetError
      { assetErrorAssetId = assetId
      , assetErrorReason = WrongAssetId
      }

  newtype AssetPackBuilder (HashMapAssetPack k) = HashMapAssetPackBuilder (IORef (HM.HashMap k B.ByteString))

  putAsset (HashMapAssetPackBuilder assetsRef) assetId asset = modifyIORef' assetsRef $ HM.insert assetId asset

instance (Eq k, Hashable k, S.Serialize k) => S.Serialize (HashMapAssetPack k) where
  put (HashMapAssetPack assets) = S.put $ HM.toList assets
  get = HashMapAssetPack . HM.fromList <$> S.get

newHashMapAssetPackBuilder :: IO (AssetPackBuilder (HashMapAssetPack k))
newHashMapAssetPackBuilder = do
  assetsRef <- newIORef HM.empty
  return $ HashMapAssetPackBuilder assetsRef

finalizeHashMapAssetPack :: AssetPackBuilder (HashMapAssetPack k) -> IO (HashMapAssetPack k)
finalizeHashMapAssetPack (HashMapAssetPackBuilder assetsRef) = HashMapAssetPack <$> readIORef assetsRef

instance Embed k => Embed (HashMapAssetPack k) where
  embedExp (HashMapAssetPack assets) = [| HashMapAssetPack $ HM.fromList $(listE $ flip map (HM.toList assets) $ \(assetId, asset) -> tupE [embedExp assetId, embedExp asset]) |]
