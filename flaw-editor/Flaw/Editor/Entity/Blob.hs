{-|
Module: Flaw.Editor.Entity.Blob
Description: Blob entities.
License: MIT
-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.Editor.Entity.Blob
	( IBlob
	-- , BlobBytes(..)
	, BlobByUrl(..)
	) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Default
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Serialize.Text()
import GHC.Generics(Generic)

import Flaw.Editor.BlobService
import Flaw.Editor.Entity
import Flaw.Editor.Entity.Basic()
import Flaw.Editor.Processing

class (ProcessableEntity a, ProcessableEntityResult a ~ B.ByteString) => IBlob a

instance EntityInterface IBlob where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "IBlob")

-- | Blob which must be fetched using URL and then hash-verified.
data BlobByUrl = BlobByUrl
	{ blobByUrlUrl :: !T.Text
	, blobByUrlHash :: !BlobHash
	} deriving (Generic, S.Serialize)
instance Entity BlobByUrl where
	getEntityTypeId _ = $(hashTextToEntityTypeId "BlobByUrl")
	interfaceEntity = $(interfaceEntityExp [''ProcessableEntity, ''IBlob])
	entityToText BlobByUrl
		{ blobByUrlUrl = url
		, blobByUrlHash = hash
		} = "blob:" <> url <> " #" <> entityToText hash
instance Default BlobByUrl where
	def = BlobByUrl
		{ blobByUrlUrl = T.empty
		, blobByUrlHash = BlobHashSHA256 B.empty
		}
instance ProcessableEntity BlobByUrl where
	type ProcessableEntityResult BlobByUrl = B.ByteString
	processEntity BlobByUrl
		{ blobByUrlUrl = url
		, blobByUrlHash = hash
		} = do
		ProcessingContext
			{ processingContextBlobService = blobService
			} <- ask
		liftIO $ fetchBlobByUrl blobService url hash
instance IBlob BlobByUrl
