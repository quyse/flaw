{-|
Module: Flaw.Asset.HashedAssetPack
Description: Asset pack transformer adding hashes to asset ids.
License: MIT
-}

{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Flaw.Asset.HashedAssetPack
	( HashedAssetPack(..)
	, AssetPackBuilder(..)
	, loadHashedAssetPack
	, newHashedAssetPackBuilder
	, saveHashedAssetPackBuilder
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteArray.Encoding as BA
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Crypto.Hash as C

import Flaw.Asset

data HashedAssetPack ap = HashedAssetPack !ap !(HM.HashMap T.Text T.Text)

loadHashedAssetPack :: AssetPack ap => ap -> AssetId ap -> IO (HashedAssetPack ap)
loadHashedAssetPack assetPack packAssetId = do
	eitherIds <- liftM S.decode $ loadAsset assetPack packAssetId
	case eitherIds of
		Right ids -> return $ HashedAssetPack assetPack $ HM.fromList ids
		Left e -> error e

newHashedAssetPackBuilder :: AssetPack ap => AssetPackBuilder ap -> IO (AssetPackBuilder (HashedAssetPack ap))
newHashedAssetPackBuilder assetPackBuilder = do
	idsVar <- newTVarIO HM.empty
	return $ HashedAssetPackBuilder assetPackBuilder idsVar

saveHashedAssetPackBuilder :: AssetPack ap => AssetPackBuilder (HashedAssetPack ap) -> AssetId ap -> IO ()
saveHashedAssetPackBuilder (HashedAssetPackBuilder packBuilder idsVar) packAssetId = do
	ids <- readTVarIO idsVar
	putAsset packBuilder packAssetId $ S.encode $ HM.toList ids

instance (AssetPack ap, AssetId ap ~ T.Text) => AssetPack (HashedAssetPack ap) where
	type AssetId (HashedAssetPack ap) = T.Text

	loadAsset (HashedAssetPack assetPack ids) assetId = handle handler $ do
		case HM.lookup assetId ids of
			Just ai -> loadAsset assetPack ai
			Nothing -> throwIO AssetError
				{ assetErrorAssetId = assetId
				, assetErrorReason = WrongAssetId
				}
		where
			handler e = throwIO AssetError
				{ assetErrorAssetId = assetId
				, assetErrorReason = UnderlyingAssetError e
				}

	data AssetPackBuilder (HashedAssetPack assetPack) = HashedAssetPackBuilder !(AssetPackBuilder assetPack) !(TVar (HM.HashMap T.Text T.Text))

	putAsset (HashedAssetPackBuilder assetPackBuilder idsVar) assetId asset = do
		-- calculate hashed name
		let hashedName = T.decodeUtf8 $ BA.convertToBase BA.Base16 $ (C.hashFinalize $ C.hashUpdate C.hashInit asset :: C.Digest C.SHA256)
		-- full name
		underlyingAssetId <- evaluate $ hashedName <> "-" <> assetId

		-- put relation into map
		atomically $ do
			ids <- readTVar idsVar
			-- check that there's no such asset id yet
			when (isJust $ HM.lookup assetId ids) $ throwSTM $ AssetBuilderDuplicateAssetIdError assetId
			writeTVar idsVar $ HM.insert assetId underlyingAssetId ids

		-- write underlying asset
		putAsset assetPackBuilder underlyingAssetId asset
