{-|
Module: Flaw.Asset.RemapAssetPack
Description: Asset pack transformer remapping asset ids.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module Flaw.Asset.RemapAssetPack
	( RemapAssetPack(..)
	, AssetPackBuilder(..)
	, loadRemapAssetPack
	, newRemapAssetPackBuilder
	, saveRemapAssetPackBuilder
	, remapAssetWithHash
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import qualified Crypto.Hash as C

import Flaw.Asset

data RemapAssetPack ap ai = RemapAssetPack !ap !(HM.HashMap ai (AssetId ap))

instance (AssetPack ap, Eq ai, Hashable ai, Typeable ai, Show ai) => AssetPack (RemapAssetPack ap ai) where
	type AssetId (RemapAssetPack ap ai) = ai

	loadAsset (RemapAssetPack assetPack ids) assetId = handle (handler assetId) $ do
		case HM.lookup assetId ids of
			Just ai -> loadAsset assetPack ai
			Nothing -> throwIO AssetError
				{ assetErrorAssetId = assetId
				, assetErrorReason = WrongAssetId
				}

	data AssetPackBuilder (RemapAssetPack ap ai) = RemapAssetPackBuilder !(AssetPackBuilder ap) (ai -> B.ByteString -> AssetId ap) !(TVar (HM.HashMap ai (AssetId ap)))

	putAsset (RemapAssetPackBuilder assetPackBuilder remap idsVar) assetId asset = do
		-- get underlying asset id
		underlyingAssetId <- evaluate $ remap assetId asset
		-- put relation into map
		atomically $ do
			ids <- readTVar idsVar
			-- check that there's no such asset id yet
			when (isJust $ HM.lookup assetId ids) $ throwSTM $ AssetBuilderDuplicateAssetIdError assetId
			writeTVar idsVar $ HM.insert assetId underlyingAssetId ids
		-- put asset
		putAsset assetPackBuilder underlyingAssetId asset

instance (WebAssetPack ap, Eq ai, Hashable ai, Typeable ai, Show ai) => WebAssetPack (RemapAssetPack ap ai) where
	getWebAssetUrl (RemapAssetPack assetPack ids) assetId = handle (handler assetId) $ do
		case HM.lookup assetId ids of
			Just ai -> getWebAssetUrl assetPack ai
			Nothing -> throwIO AssetError
				{ assetErrorAssetId = assetId
				, assetErrorReason = WrongAssetId
				}

loadRemapAssetPack :: (AssetPack ap, Eq ai, Hashable ai, S.Serialize ai, S.Serialize (AssetId ap)) => ap -> B.ByteString -> IO (RemapAssetPack ap ai)
loadRemapAssetPack assetPack packBytes = case S.decode packBytes of
	Right ids -> return $ RemapAssetPack assetPack $ HM.fromList ids
	Left e -> error e

newRemapAssetPackBuilder :: AssetPack ap => AssetPackBuilder ap -> (ai -> B.ByteString -> AssetId ap) -> IO (AssetPackBuilder (RemapAssetPack ap ai))
newRemapAssetPackBuilder assetPackBuilder remap = do
	idsVar <- newTVarIO HM.empty
	return $ RemapAssetPackBuilder assetPackBuilder remap idsVar

saveRemapAssetPackBuilder :: (S.Serialize ai, S.Serialize (AssetId ap)) => AssetPackBuilder (RemapAssetPack ap ai) -> IO B.ByteString
saveRemapAssetPackBuilder (RemapAssetPackBuilder _assetPackBuilder _remap idsVar) = fmap (S.encode . HM.toList) $ readTVarIO idsVar

handler :: (Typeable ai, Show ai) => ai -> SomeException -> IO a
handler assetId e = throwIO AssetError
	{ assetErrorAssetId = assetId
	, assetErrorReason = UnderlyingAssetError e
	}

-- | One particular function suitable for generating "hashed" names.
remapAssetWithHash :: T.Text -> B.ByteString -> T.Text
remapAssetWithHash assetId asset = T.decodeUtf8 (BA.convertToBase BA.Base64URLUnpadded $ BA.takeView (C.hashFinalize $ C.hashUpdate C.hashInit asset :: C.Digest C.SHA256) 10) <> "-" <> assetId
