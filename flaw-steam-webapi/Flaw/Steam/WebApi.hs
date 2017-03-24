{-|
Module: Flaw.Steam.WebApi
Description: Steam WebAPI.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ViewPatterns #-}

module Flaw.Steam.WebApi
	( SteamWebApi()
	, newSteamWebApi
	, steamWebApiAuthenticateUserTicket
	, SteamTicket(..)
	, steamWebApiCheckAppOwnership
	) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H

import Flaw.Steam

-- | Steam Web Api.
data SteamWebApi = SteamWebApi
	{ steamWebApiHttpManager :: !H.Manager
	, steamWebApiHttpRequest :: !H.Request
	, steamWebApiAppId :: !B.ByteString
	, steamWebApiKey :: !B.ByteString
	}

newSteamWebApi :: H.Manager -> Int -> T.Text -> IO SteamWebApi
newSteamWebApi httpManager appId apiKey = return SteamWebApi
	{ steamWebApiHttpManager = httpManager
	, steamWebApiHttpRequest = H.defaultRequest
		{ H.method = "GET"
		, H.secure = True
		, H.host = "api.steampowered.com"
		, H.port = 443
		, H.path = "/"
		}
	, steamWebApiAppId = fromString $ show appId
	, steamWebApiKey = T.encodeUtf8 apiKey
	}

steamWebApiRequest :: SteamWebApi -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO BL.ByteString
steamWebApiRequest SteamWebApi
	{ steamWebApiHttpManager = httpManager
	, steamWebApiHttpRequest = httpRequest
	, steamWebApiAppId = appId
	, steamWebApiKey = apiKey
	} path params = do
	response <- H.httpLbs (H.setQueryString (("appid", Just appId) : ("key", Just apiKey) : params) httpRequest
		{ H.path = H.path httpRequest <> path
		}) httpManager
	return $ H.responseBody response

steamWebApiAuthenticateUserTicket :: SteamWebApi -> B.ByteString -> IO SteamTicket
steamWebApiAuthenticateUserTicket api ticket = do
	response <- steamWebApiRequest api "ISteamUserAuth/AuthenticateUserTicket/V0001/"
		[ ("ticket", Just $ BA.convertToBase BA.Base16 ticket)
		]
	(A.decode -> Just (A.Object (HM.lookup "response" -> Just (A.Object (HM.lookup "params" -> Just
		(A.fromJSON -> A.Success AuthenticatedUserTicket
			{ aut_result = "OK"
			, aut_steamid = SteamId . read . T.unpack -> steamId
			, aut_ownersteamid = SteamId . read . T.unpack -> ownerSteamId
			, aut_vacbanned = vacBanned
			, aut_publisherbanned = publisherBanned
			})))))) <- return response
	return SteamTicket
		{ steamTicketSteamId = steamId
		, steamTicketOwnerSteamId = ownerSteamId
		, steamTicketVacBanned = vacBanned
		, steamTicketPublisherBanned = publisherBanned
		}

data SteamTicket = SteamTicket
	{ steamTicketSteamId :: {-# UNPACK #-} !SteamId
	, steamTicketOwnerSteamId :: {-# UNPACK #-} !SteamId
	, steamTicketVacBanned :: !Bool
	, steamTicketPublisherBanned :: !Bool
	} deriving Show

data AuthenticatedUserTicket = AuthenticatedUserTicket
	{ aut_result :: !T.Text
	, aut_steamid :: !T.Text
	, aut_ownersteamid :: !T.Text
	, aut_vacbanned :: !Bool
	, aut_publisherbanned :: !Bool
	} deriving Generic
instance A.FromJSON AuthenticatedUserTicket where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 4
		}

steamWebApiCheckAppOwnership :: SteamWebApi -> SteamId -> IO Bool
steamWebApiCheckAppOwnership api (SteamId steamId) = do
	response <- steamWebApiRequest api "ISteamUser/CheckAppOwnership/V0001/"
		[ ("steamid", Just $ fromString $ show steamId)
		]
	(A.decode -> Just (A.Object (HM.lookup "appownership" -> Just (A.fromJSON -> A.Success AppOwnership
		{ ao_result = "OK"
		, ao_ownsapp = ownsApp
		})))) <- return response
	return ownsApp

data AppOwnership = AppOwnership
	{ ao_result :: !T.Text
	, ao_ownsapp :: !Bool
	-- , ao_permanent :: !Bool
	-- , ao_timestamp :: !T.Text
	-- , ao_ownersteamid :: !T.Text
	} deriving Generic
instance A.FromJSON AppOwnership where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 3
		}
