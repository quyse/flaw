{-|
Module: Flaw.Social.Vkontakte
Description: vk.com integration.
License: MIT
-}

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, JavaScriptFFI, OverloadedStrings, TypeFamilies, ViewPatterns #-}

module Flaw.Social.Vkontakte
	( Vkontakte()
	, SocialUserId(..)
	, SocialUserToken(..)
#if defined(ghcjs_HOST_OS)
	, initVkontakteIframe
#else
	, initVkontakte
	, vkontakteHeadScripts
#endif
	) where

import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Flaw.Social

#if defined(ghcjs_HOST_OS)

import Control.Exception
import Control.Monad
import Data.JSString.Text
import GHCJS.Types

import Flaw.Exception

#else

import Crypto.Hash
import Control.Monad.IO.Class
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as B
import Data.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

#endif

instance Social Vkontakte where
	newtype SocialUserId Vkontakte = VkontakteUserId Int deriving S.Serialize
	newtype SocialUserToken Vkontakte = VkontakteUserToken T.Text deriving S.Serialize
	socialUniversalUserId (VkontakteUserId userId) = "vkontakte" <> fromString (show userId)

#if defined(ghcjs_HOST_OS)

data Vkontakte = Vkontakte

-- | Init IFrame Vkontakte application.
initVkontakteIframe :: IO Vkontakte
initVkontakteIframe = do
	initialized <- js_init_iframe
	unless initialized $ throwIO $ DescribeFirstException ("failed to initialize Vkontakte iframe client" :: T.Text)
	return Vkontakte

instance SocialClient Vkontakte where
	authSocialClient Vkontakte = do
		viewerId <- textFromJSString <$> js_viewerId
		case viewerId of
			(reads -> (viewerId, "") : _) -> do
				authKey <- T.encodeUtf8 . textFromJSString <$> js_authKey
				return $ Just (VkontakteUserId viewerId, VkontakteUserToken authKey)
			_ -> return Nothing

foreign import javascript interruptible "h$flaw_social_vk_init_iframe($c);" js_init_iframe :: IO Bool
foreign import javascript unsafe "$r = h$flaw_social_vk_viewer_id" js_viewerId :: IO JSString
foreign import javascript unsafe "$r = h$flaw_social_vk_auth_key" js_authKey :: IO JSString

#else

data Vkontakte = Vkontakte
	{ vkontakteAppIdBytes :: !B.ShortByteString
	, vkontakteAppSecretBytes :: !B.ShortByteString
	}

initVkontakte :: Int -> T.Text -> IO Vkontakte
initVkontakte appId appSecret = return Vkontakte
	{ vkontakteAppIdBytes = fromString $ show appId
	, vkontakteAppSecretBytes = B.toShort $ T.encodeUtf8 appSecret
	}

instance SocialServer Vkontakte where
	authSocialClientByRequest vkontakte getParam = do
		maybeViewerId <- getParam "viewer_id"
		maybeAuthKey <- getParam "auth_key"
		case (maybeViewerId, maybeAuthKey) of
			(Just (reads . T.unpack -> (viewerId, "") : _), Just authKey) -> do
				let userId = VkontakteUserId viewerId
				ok <- liftIO $ verifySocialUserToken vkontakte userId $ VkontakteUserToken authKey
				return $ if ok then Just userId else Nothing
			_ -> return Nothing
	-- | Verify user token.
	-- TODO: check via secure.checkToken.
	verifySocialUserToken Vkontakte
		{ vkontakteAppIdBytes = appIdBytes
		, vkontakteAppSecretBytes = appSecretBytes
		} (VkontakteUserId userId) (VkontakteUserToken userToken) = return $ T.encodeUtf8 userToken == userTokenShouldBe
		where userTokenShouldBe = BL.toStrict $ B.toLazyByteString $ B.byteStringHex $ BA.convert
			$ (hash :: B.ByteString -> Digest MD5) $ BL.toStrict $ B.toLazyByteString
			$  B.shortByteString appIdBytes
			<> B.char7 '_'
			<> fromString (show userId)
			<> B.char7 '_'
			<> B.shortByteString appSecretBytes

vkontakteHeadScripts :: H.Html
vkontakteHeadScripts = H.script H.! A.src "//vk.com/js/api/xd_connection.js?2" $ mempty

#endif
