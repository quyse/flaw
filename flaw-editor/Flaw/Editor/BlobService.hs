{-|
Module: Flaw.Editor.BlobService
Description: Blob storage service.
License: MIT
-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Flaw.Editor.BlobService
	( BlobService(..)
	, BlobHash(..)
	, fetchBlobByUrl
	, newBlobService
	, BlobException(..)
	) where

import Control.Exception
import Control.Monad
import qualified Crypto.Hash as C
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import Data.Default
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H

import Flaw.BinaryCache
import Flaw.Book
import Flaw.Editor.Entity

-- | Blob service supports different ways of obtaining blobs.
data BlobService where
	BlobService :: BinaryCache c =>
		{ blobServiceCache :: !c
		, blobServiceHttpManager :: !H.Manager
		} -> BlobService

-- | Blob hash.
data BlobHash
	= BlobHashSHA256 B.ByteString
	deriving (Generic, S.Serialize)

instance Entity BlobHash where
	type EntityChange BlobHash = BlobHash
	getEntityTypeId _ = $(hashTextToEntityTypeId "BlobHash")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	entityToText hash = case hash of
		BlobHashSHA256 bytes -> "sha256:" <> T.decodeUtf8 (BA.convertToBase BA.Base64URLUnpadded bytes)
instance BasicEntity BlobHash
instance Default BlobHash where
	def = BlobHashSHA256 B.empty

fetchBlobByUrl :: BlobService -> T.Text -> BlobHash -> IO B.ByteString
fetchBlobByUrl BlobService
	{ blobServiceCache = cache
	, blobServiceHttpManager = httpManager
	} url hash = do
	let encodedHash = S.encode hash
	-- check cache
	maybeBlob <- getCachedBinary cache encodedHash
	case maybeBlob of
		Just blob -> return blob
		Nothing -> do
			-- fetch blob
			let
				fetchHttp = handle (\SomeException {} -> throwIO BlobFetchException) $ do
					request <- H.parseUrlThrow (T.unpack url)
					H.withResponse request httpManager $ fmap B.concat . H.brConsume . H.responseBody
			blob <- case T.breakOn "://" url of
				("file", fileName) -> handle (\SomeException {} -> throwIO BlobFetchException) $ B.readFile $ T.unpack fileName
				("http", _) -> fetchHttp
				("https" , _) -> fetchHttp
				_ -> throwIO BlobWrongUrlException
			-- verify hash
			let hashOk = case hash of
				BlobHashSHA256 hashBytes -> hashBytes == BA.convert (C.hash blob :: C.Digest C.SHA256)
			unless hashOk $ throwIO BlobHashMismatchException
			-- put into cache
			putCachedBinary cache encodedHash blob
			return blob

newBlobService :: BinaryCache c => c -> IO (BlobService, IO ())
newBlobService cache = withSpecialBook $ \_bk -> do
	httpManager <- H.newManager H.defaultManagerSettings
	return BlobService
		{ blobServiceCache = cache
		, blobServiceHttpManager = httpManager
		}

data BlobException
	= BlobWrongUrlException
	| BlobFetchException
	| BlobHashMismatchException
	deriving Show

instance Exception BlobException
