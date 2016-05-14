{-|
Module: Flaw.Asset
Description: Assets.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}

module Flaw.Asset
	( AssetPack(..)
	, AssetError(..)
	, AssetErrorReason(..)
	, AssetBuilderError(..)
	, WebAssetPack(..)
	, assetPackUrlQQ
	) where

import Control.Exception
import qualified Data.ByteString as B
import Data.String
import qualified Data.Text as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Flaw.Build

-- | Asset pack is a thing containing bytestrings accessible by asset id.
class AssetPack a where
	type AssetId a :: *
	loadAsset :: a -> AssetId a -> IO B.ByteString

	data AssetPackBuilder a :: *
	putAsset :: AssetPackBuilder a -> AssetId a -> B.ByteString -> IO ()

-- | Exception datatype for asset loading.
data AssetError ai = AssetError
	{ assetErrorAssetId :: ai
	, assetErrorReason :: AssetErrorReason
	} deriving Typeable

deriving instance Show ai => Show (AssetError ai)

data AssetErrorReason
	= WrongAssetId
	| UnderlyingAssetError SomeException
	deriving Show

instance (Typeable ai, Show ai) => Exception (AssetError ai)

-- | Exception for asset building.
data AssetBuilderError ai
	= AssetBuilderDuplicateAssetIdError ai
	deriving Typeable

deriving instance Show ai => Show (AssetBuilderError ai)

instance (Typeable ai, Show ai) => Exception (AssetBuilderError ai)

-- | Web asset pack can also return asset URL for loading bytestring directly.
class AssetPack a => WebAssetPack a where
	getWebAssetUrl :: a -> AssetId a -> IO T.Text

-- | Expression for quasi-quoter for getting url from web asset pack in compile time.
assetPackUrlQQ :: (Embed a, WebAssetPack a, IsString (AssetId a)) => a -> ExpQ
assetPackUrlQQ assetPack = [| QuasiQuoter
	{ quoteExp = \assetId -> litE . stringL . T.unpack =<< runIO (getWebAssetUrl $(embedExp assetPack) (fromString assetId))
	, quotePat = undefined
	, quoteType = undefined
	, quoteDec = undefined
	}
	|]
