{-|
Module: Flaw.GameJolt.WebApi
Description: GameJolt WebAPI.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ViewPatterns #-}

module Flaw.GameJolt.WebApi
	( GameJoltWebApi()
	, newGameJoltWebApi
	, gameJoltWebApiUsersAuth
	, gameJoltWebApiUsersFetch
	, GameJoltUser(..)
	) where

import qualified Crypto.Hash as C
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H
import Network.URI(uriToString)

-- | GameJolt Web Api.
data GameJoltWebApi = GameJoltWebApi
	{ gameJoltWebApiHttpManager :: !H.Manager
	, gameJoltWebApiHttpRequest :: !H.Request
	, gameJoltWebApiGameId :: !B.ByteString
	, gameJoltWebApiKey :: !B.ByteString
	}

newGameJoltWebApi :: H.Manager -> Int -> T.Text -> IO GameJoltWebApi
newGameJoltWebApi httpManager gameId apiKey = return GameJoltWebApi
	{ gameJoltWebApiHttpManager = httpManager
	, gameJoltWebApiHttpRequest = H.defaultRequest
		{ H.method = "GET"
		, H.secure = False
		, H.host = "gamejolt.com"
		, H.port = 80
		, H.path = "/api/game/v1/"
		}
	, gameJoltWebApiGameId = fromString $ show gameId
	, gameJoltWebApiKey = T.encodeUtf8 apiKey
	}

gameJoltWebApiRequest :: GameJoltWebApi -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO A.Value
gameJoltWebApiRequest GameJoltWebApi
	{ gameJoltWebApiHttpManager = httpManager
	, gameJoltWebApiHttpRequest = httpRequest
	, gameJoltWebApiGameId = gameId
	, gameJoltWebApiKey = apiKey
	} path (((("game_id", Just gameId) :) . (("format", Just "json") :)) -> params) = do
	let
		request = H.setQueryString params httpRequest
			{ H.path = H.path httpRequest <> path
			}
		uri = T.pack $ uriToString id (H.getUri request) ""
		-- remove port from URI (uriToString puts it there unconditionally)
		fixedUri = maybe uri ("http://gamejolt.com/" <>) $ T.stripPrefix "http://gamejolt.com:80/" uri
		signature = BA.convertToBase BA.Base16 (C.hash (T.encodeUtf8 fixedUri <> apiKey) :: C.Digest C.SHA1)
		signedRequest = H.setQueryString (params ++ [("signature", Just signature)]) request
	response <- H.responseBody <$> H.httpLbs signedRequest httpManager
	Just value <- return $ A.decode response
	return value

gameJoltWebApiUsersAuth :: GameJoltWebApi -> T.Text -> T.Text -> IO Bool
gameJoltWebApiUsersAuth api userName userToken = do
	response <- gameJoltWebApiRequest api "users/auth/"
		[ ("username", Just $ T.encodeUtf8 userName)
		, ("user_token", Just $ T.encodeUtf8 userToken)
		]
	return $ case response of
		A.Object (HM.lookup "response" -> Just (A.Object (HM.lookup "success" -> Just (A.String "true")))) -> True
		_ -> False

gameJoltWebApiUsersFetch :: GameJoltWebApi -> T.Text -> IO GameJoltUser
gameJoltWebApiUsersFetch api userName = do
	response <- gameJoltWebApiRequest api "users/fetch/"
		[ ("username", Just $ T.encodeUtf8 userName)
		]
	A.Object (HM.lookup "response" -> Just (A.Object (HM.lookup "users" -> Just (A.fromJSON -> A.Success [User
		{ u_id = read . T.unpack -> userId
		, u_username = userName1
		, u_avatar_url = userAvatarUrl
		, u_status = ("Active" ==) -> userActive
		}])))) <- return response
	return GameJoltUser
		{ gameJoltUserId = userId
		, gameJoltUserName = userName1
		, gameJoltUserAvatarUrl = userAvatarUrl
		, gameJoltUserActive = userActive
		}

data GameJoltUser = GameJoltUser
	{ gameJoltUserId :: {-# UNPACK #-} !Word64
	, gameJoltUserName :: !T.Text
	, gameJoltUserAvatarUrl :: !T.Text
	, gameJoltUserActive :: !Bool
	} deriving Show

data User = User
	{ u_id :: !T.Text
	, u_username :: !T.Text
	, u_avatar_url :: !T.Text
	, u_status :: !T.Text
	} deriving Generic
instance A.FromJSON User where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 2
		}
