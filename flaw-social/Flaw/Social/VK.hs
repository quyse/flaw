{-|
Module: Flaw.Social.VK
Description: VK.com integration.
License: MIT
-}

{-# LANGUAGE CPP, JavaScriptFFI, OverloadedStrings, TypeFamilies #-}

module Flaw.Social.VK
	( VK()
	, SocialUserId(..)
	, SocialUserToken(..)
#if defined(ghcjs_HOST_OS)
	, initVKiframe
#else
	, initVK
	, vkHeadScripts
#endif
	) where

import qualified Data.Text as T

import Flaw.Social

#if defined(ghcjs_HOST_OS)

import Control.Exception
import Control.Monad
import Data.JSString.Text
import GHCJS.Types

import Flaw.Exception

#else

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text.Encoding as T
import Crypto.Hash
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

#endif

instance Social VK where
	newtype SocialUserId VK = VKUserId T.Text
	newtype SocialUserToken VK = VKUserToken T.Text

#if defined(ghcjs_HOST_OS)

data VK = VK

-- | Init IFrame VK application.
initVKiframe :: IO VK
initVKiframe = do
	initialized <- js_init_iframe
	unless initialized $ throwIO $ DescribeFirstException ("failed to initialize VK iframe client" :: T.Text)
	return VK

instance SocialClient VK where
	authSocialClient VK = do
		viewerId <- textFromJSString <$> js_viewerId
		if T.null viewerId then return Nothing
		else do
			authKey <- textFromJSString <$> js_authKey
			return $ Just (VKUserId viewerId, VKUserToken authKey)

foreign import javascript interruptible "h$flaw_social_vk_init_iframe($c);" js_init_iframe :: IO Bool
foreign import javascript unsafe "$r = h$flaw_social_vk_viewer_id" js_viewerId :: IO JSString
foreign import javascript unsafe "$r = h$flaw_social_vk_auth_key" js_authKey :: IO JSString

#else

data VK = VK
	{ vkAppIdBytes :: !B.ByteString
	, vkAppSecretBytes :: !B.ByteString
	}

initVK :: T.Text -> T.Text -> IO VK
initVK appId appSecret = return VK
	{ vkAppIdBytes = T.encodeUtf8 appId
	, vkAppSecretBytes = T.encodeUtf8 appSecret
	}

instance SocialServer VK where
	-- | Verify user token.
	-- TODO: check via secure.checkToken.
	verifySocialUserToken VK
		{ vkAppIdBytes = appIdBytes
		, vkAppSecretBytes = appSecretBytes
		} (VKUserId userId) (VKUserToken userToken) = return $ T.encodeUtf8 userToken == userTokenShouldBe
		where userTokenShouldBe = BL.toStrict $ B.toLazyByteString $ B.byteStringHex $ BA.convert
			$ (hash :: B.ByteString -> Digest MD5) $ BL.toStrict $ B.toLazyByteString
			$  B.byteString appIdBytes
			<> B.char7 '_'
			<> B.byteString (T.encodeUtf8 userId)
			<> B.char7 '_'
			<> B.byteString appSecretBytes

vkHeadScripts :: H.Html
vkHeadScripts = H.script H.! A.src "//vk.com/js/api/xd_connection.js?2" $ mempty

#endif
