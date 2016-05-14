{-|
Module: Flaw.Asset.RemapAssetPack
Description: Asset pack transformer remapping asset ids.
License: MIT
-}

{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, OverloadedStrings, TemplateHaskell #-}

module Flaw.Asset.RemapAssetPack
	( RemapAssetPack(..)
	, AssetPackBuilder(..)
	, RemapAssetPackContainer(..)
	, newRemapAssetPackBuilder
	, finalizeRemapAssetPackBuilder
	, loadRemapAssetPack
#ifndef ghcjs_HOST_OS
	, remapAssetWithHash
#endif
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import Data.Typeable

#ifndef ghcjs_HOST_OS
import qualified Crypto.Hash as C
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#endif

import Flaw.Asset
import Flaw.Build

-- | Remap asset pack, includes underlying asset pack and mapping of asset ids.
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

instance (Embed ap, Embed ai, Embed (AssetId ap)) => Embed (RemapAssetPack ap ai) where
	embedExp (RemapAssetPack assetPack ids) = [| RemapAssetPack $(embedExp assetPack) $ HM.fromList $(embedExp $ HM.toList ids) |]

-- | Remap asset pack container, includes only mapping, serializable.
newtype RemapAssetPackContainer ai1 ai2 = RemapAssetPackContainer (HM.HashMap ai1 ai2)

instance (S.Serialize ai1, S.Serialize ai2, Eq ai1, Hashable ai1) => S.Serialize (RemapAssetPackContainer ai1 ai2) where
	put (RemapAssetPackContainer ids) = S.put $ HM.toList ids
	get = RemapAssetPackContainer . HM.fromList <$> S.get

instance (Embed ai1, Embed ai2) => Embed (RemapAssetPackContainer ai1 ai2) where
	embedExp (RemapAssetPackContainer ids) = [| RemapAssetPackContainer $ HM.fromList $(embedExp $ HM.toList ids) |]

newRemapAssetPackBuilder :: AssetPack ap => AssetPackBuilder ap -> (ai -> B.ByteString -> AssetId ap) -> IO (AssetPackBuilder (RemapAssetPack ap ai))
newRemapAssetPackBuilder assetPackBuilder remap = do
	idsVar <- newTVarIO HM.empty
	return $ RemapAssetPackBuilder assetPackBuilder remap idsVar

finalizeRemapAssetPackBuilder :: AssetPackBuilder (RemapAssetPack ap ai) -> IO (RemapAssetPackContainer ai (AssetId ap))
finalizeRemapAssetPackBuilder (RemapAssetPackBuilder _assetPackBuilder _remap idsVar) = RemapAssetPackContainer <$> readTVarIO idsVar

loadRemapAssetPack :: (AssetPack ap, AssetId ap ~ ai2) => RemapAssetPackContainer ai1 ai2 -> ap -> RemapAssetPack ap ai1
loadRemapAssetPack (RemapAssetPackContainer ids) assetPack = RemapAssetPack assetPack ids

handler :: (Typeable ai, Show ai) => ai -> SomeException -> IO a
handler assetId e = throwIO AssetError
	{ assetErrorAssetId = assetId
	, assetErrorReason = UnderlyingAssetError e
	}

#ifndef ghcjs_HOST_OS

-- | One particular function suitable for generating "hashed" URLs.
remapAssetWithHash :: T.Text -> B.ByteString -> T.Text
remapAssetWithHash assetId asset = T.decodeUtf8 (BA.convertToBase BA.Base64URLUnpadded $ BA.takeView (C.hash asset :: C.Digest C.SHA256) 10) <> "/" <> T.takeWhileEnd (/= '/') assetId

#endif
