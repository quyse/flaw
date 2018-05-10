{-|
Module: Flaw.Steam.WebApi
Description: Steam WebAPI.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ViewPatterns #-}

module Flaw.Steam.WebApi
	( SteamWebApi()
	, newSteamWebApi
	, steamWebApi_ISteamUser_GetPlayerSummary
	, SteamPlayerSummary(..)
	, steamWebApiAuthenticateUserTicket
	, SteamTicket(..)
	, steamWebApiCheckAppOwnership
	) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
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

newSteamWebApi :: H.Manager -> Int -> T.Text -> SteamWebApi
newSteamWebApi httpManager appId apiKey = SteamWebApi
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

steamWebApiRequest :: J.FromJSON a => SteamWebApi -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO a
steamWebApiRequest SteamWebApi
	{ steamWebApiHttpManager = httpManager
	, steamWebApiHttpRequest = httpRequest
	, steamWebApiAppId = appId
	, steamWebApiKey = apiKey
	} path params = do
	Just value <- J.decode . H.responseBody <$> H.httpLbs (H.setQueryString (("appid", Just appId) : ("key", Just apiKey) : params) httpRequest
		{ H.path = H.path httpRequest <> path
		}) httpManager
	return value

steamWebApi_ISteamUser_GetPlayerSummary :: SteamWebApi -> SteamId -> IO (Maybe SteamPlayerSummary)
steamWebApi_ISteamUser_GetPlayerSummary api (SteamId steamId) =
	parseResponse =<< steamWebApiRequest api "ISteamUser/GetPlayerSummaries/v2" [("steamids", Just $ fromString $ show steamId)]
	where parseResponse = (either fail return .) . J.parseEither $ \o -> do
		playerSummaries <- (J..: "players") =<< (o J..: "response")
		return $ if V.null playerSummaries then Nothing else Just (playerSummaries V.! 0)

data SteamPlayerSummary = SteamPlayerSummary
	{ steamPlayerSummary_personaname :: !T.Text
	, steamPlayerSummary_profileurl :: !T.Text
	, steamPlayerSummary_avatar :: !T.Text
	} deriving Generic
instance J.FromJSON SteamPlayerSummary where
	parseJSON = J.genericParseJSON J.defaultOptions
		{ J.fieldLabelModifier = drop 19
		}

steamWebApiAuthenticateUserTicket :: SteamWebApi -> B.ByteString -> IO SteamTicket
steamWebApiAuthenticateUserTicket api ticket = do
	J.Object (HM.lookup "response" -> Just (J.Object (HM.lookup "params" -> Just
		(J.fromJSON -> J.Success AuthenticatedUserTicket
			{ aut_result = "OK"
			, aut_steamid = SteamId . read . T.unpack -> steamId
			, aut_ownersteamid = SteamId . read . T.unpack -> ownerSteamId
			, aut_vacbanned = vacBanned
			, aut_publisherbanned = publisherBanned
			})))) <- steamWebApiRequest api "ISteamUserAuth/AuthenticateUserTicket/V0001/"
		[ ("ticket", Just $ BA.convertToBase BA.Base16 ticket)
		]
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
instance J.FromJSON AuthenticatedUserTicket where
	parseJSON = J.genericParseJSON J.defaultOptions
		{ J.fieldLabelModifier = drop 4
		}

steamWebApiCheckAppOwnership :: SteamWebApi -> SteamId -> IO Bool
steamWebApiCheckAppOwnership api (SteamId steamId) = do
	J.Object (HM.lookup "appownership" -> Just (J.fromJSON -> J.Success AppOwnership
		{ ao_result = "OK"
		, ao_ownsapp = ownsApp
		})) <- steamWebApiRequest api "ISteamUser/CheckAppOwnership/V0001/"
		[ ("steamid", Just $ fromString $ show steamId)
		]
	return ownsApp

data AppOwnership = AppOwnership
	{ ao_result :: !T.Text
	, ao_ownsapp :: !Bool
	-- , ao_permanent :: !Bool
	-- , ao_timestamp :: !T.Text
	-- , ao_ownersteamid :: !T.Text
	} deriving Generic
instance J.FromJSON AppOwnership where
	parseJSON = J.genericParseJSON J.defaultOptions
		{ J.fieldLabelModifier = drop 3
		}
