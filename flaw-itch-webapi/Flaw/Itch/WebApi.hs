{-|
Module: Flaw.Itch.WebApi
Description: Itch WebAPI.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, ViewPatterns #-}

module Flaw.Itch.WebApi
	( ItchWebApi()
	, newItchWebApi
	, itchWebApiMe
	, itchWebApiDownloadKeys
	, ItchUserId(..)
	, ItchUser(..)
	, ItchGameId(..)
	, ItchDownloadKeyId(..)
	, ItchDownloadKey(..)
	) where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H

-- | Itch Web Api.
data ItchWebApi = ItchWebApi
	{ itchWebApiHttpManager :: !H.Manager
	, itchWebApiHttpRequest :: !H.Request
	}

newItchWebApi :: H.Manager -> T.Text -> IO ItchWebApi
newItchWebApi httpManager apiKey = return ItchWebApi
	{ itchWebApiHttpManager = httpManager
	, itchWebApiHttpRequest = H.defaultRequest
		{ H.method = "GET"
		, H.secure = True
		, H.host = "itch.io"
		, H.port = 443
		, H.path = "/api/1/" <> T.encodeUtf8 apiKey
		}
	}

itchWebApiRequest :: ItchWebApi -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO A.Value
itchWebApiRequest ItchWebApi
	{ itchWebApiHttpManager = httpManager
	, itchWebApiHttpRequest = httpRequest
	} path params = do
	Just value <- A.decode . H.responseBody <$> H.httpLbs (H.setQueryString params httpRequest
		{ H.path = H.path httpRequest <> path
		}) httpManager
	return value

itchWebApiMe :: ItchWebApi -> T.Text -> Bool -> IO (Maybe ItchUser)
itchWebApiMe ItchWebApi
	{ itchWebApiHttpManager = httpManager
	, itchWebApiHttpRequest = httpRequest
	} token isKey =
	(itchMeResponse_user <$>) . A.decode' . H.responseBody <$> H.httpLbs httpRequest
		{ H.path = "/api/1/" <> (if isKey then "key" else "jwt") <> "/me"
		, H.requestHeaders = ("Authorization", T.encodeUtf8 token) : H.requestHeaders httpRequest
		} httpManager

itchWebApiDownloadKeys :: ItchWebApi -> ItchGameId -> ItchUserId -> IO (Maybe ItchDownloadKey)
itchWebApiDownloadKeys api (ItchGameId gameId) (ItchUserId userId) = do
	value <- itchWebApiRequest api ("game/" <> fromString (show gameId) <> "/download_keys")
		[ ("user_id", Just $ fromString $ show userId)
		]
	return $ case value of
		A.Object (HM.lookup "download_key" -> Just (A.fromJSON -> A.Success downloadKey)) -> Just downloadKey
		_ -> Nothing

newtype ItchUserId = ItchUserId Word64 deriving (Eq, Ord, Hashable, Generic, Show, A.FromJSON, A.ToJSON)
data ItchUser = ItchUser
	{ itchUser_id :: !ItchUserId
	, itchUser_username :: !T.Text
	, itchUser_url :: !T.Text
	, itchUser_cover_url :: !(Maybe T.Text)
	} deriving (Generic, Show)
instance A.FromJSON ItchUser where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 9
		}

data ItchMeResponse = ItchMeResponse
	{ itchMeResponse_user :: !ItchUser
	} deriving (Generic, Show)
instance A.FromJSON ItchMeResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 15
		}

newtype ItchGameId = ItchGameId Word64 deriving (Eq, Ord, Hashable, Generic, Show, A.FromJSON, A.ToJSON)

newtype ItchDownloadKeyId = ItchDownloadKeyId Word64 deriving (Eq, Ord, Hashable, Generic, Show, A.FromJSON, A.ToJSON)
data ItchDownloadKey = ItchDownloadKey
	{ itchDownloadKey_id :: !ItchDownloadKeyId
	, itchDownloadKey_game_id :: !ItchGameId
	, itchDownloadKey_owner :: !ItchUser
	} deriving (Generic, Show)
instance A.FromJSON ItchDownloadKey where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 16
		}
