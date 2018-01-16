{-|
Module: Flaw.Itch.WebApi
Description: Itch WebAPI.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, ViewPatterns #-}

module Flaw.Itch.WebApi
	( ItchWebApiKey(..)
	, itchWebApiMe
	, itchWebApiDownloadKeys
	, ItchUserId(..)
	, ItchUser(..)
	, ItchGameId(..)
	, ItchDownloadKeyId(..)
	, ItchDownloadKey(..)
	) where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import Data.Hashable
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H

-- | Itch API key (normal API key or JWT token).
data ItchWebApiKey = ItchWebApiKey !T.Text !Bool

itchWebApiRequest :: H.Manager -> ItchWebApiKey -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO A.Value
itchWebApiRequest httpManager (ItchWebApiKey apiKey isKey) path params =
	either fail return . A.eitherDecode' . H.responseBody =<< H.httpLbs (H.setQueryString params H.defaultRequest
		{ H.method = "GET"
		, H.secure = True
		, H.host = "itch.io"
		, H.port = 443
		, H.path = "/api/1/" <> (if isKey then "key" else "jwt") <> path
		, H.requestHeaders = [("Authorization", "Bearer " <> T.encodeUtf8 apiKey)]
		}) httpManager

itchWebApiMe :: H.Manager -> ItchWebApiKey -> IO (Maybe (ItchUser, ItchGameId))
itchWebApiMe httpManager apiKey =
	A.parseMaybe parseResponse <$> itchWebApiRequest httpManager apiKey "/me" []
	where parseResponse = A.withObject "response" $ \response -> do
		user <- response A..: "user"
		gameId <- (A..: "game_id") =<< A.withObject "issuer" return =<< (A..: "issuer") =<< A.withObject "api_key" return =<< response A..: "api_key"
		return (user, gameId)

itchWebApiDownloadKeys :: H.Manager -> ItchWebApiKey -> ItchGameId -> ItchUserId -> IO (Maybe ItchDownloadKey)
itchWebApiDownloadKeys httpManager apiKey (ItchGameId gameId) (ItchUserId userId) = do
	A.parseMaybe parseResponse <$> itchWebApiRequest httpManager apiKey ("/game/" <> fromString (show gameId) <> "/download_keys")
		[ ("user_id", Just $ fromString $ show userId)
		]
	where parseResponse = A.withObject "response" (A..: "download_key")

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
	, itchMeResponse_api_key :: !ItchMeApiKey
	} deriving (Generic, Show)
instance A.FromJSON ItchMeResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 15
		}

data ItchMeApiKey = ItchMeApiKey
	{ itchMeApiKey_type :: !T.Text
	, itchMeApiKey_issuer :: !ItchMeApiKeyIssuer
	} deriving (Generic, Show)
instance A.FromJSON ItchMeApiKey where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 13
		}

data ItchMeApiKeyIssuer = ItchMeApiKeyIssuer
	{ itchMeApiKeyIssuer_game_id :: {-# UNPACK #-} !ItchGameId
	} deriving (Generic, Show)
instance A.FromJSON ItchMeApiKeyIssuer where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 19
		}

newtype ItchGameId = ItchGameId Word64 deriving (Eq, Ord, Hashable, Generic, Show, A.FromJSON, A.ToJSON)

newtype ItchDownloadKeyId = ItchDownloadKeyId Word64 deriving (Eq, Ord, Hashable, Generic, Show, A.FromJSON, A.ToJSON)
data ItchDownloadKey = ItchDownloadKey
	{ itchDownloadKey_id :: !ItchDownloadKeyId
	, itchDownloadKey_game_id :: {-# UNPACK #-} !ItchGameId
	, itchDownloadKey_owner :: !ItchUser
	} deriving (Generic, Show)
instance A.FromJSON ItchDownloadKey where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 16
		}
