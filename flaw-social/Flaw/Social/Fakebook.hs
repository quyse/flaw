{-|
Module: Flaw.Social.Fakebook
Description: Fake social network, only for testing.
License: MIT
-}

{-# LANGUAGE CPP, TypeFamilies #-}

module Flaw.Social.Fakebook
	( Fakebook
	, SocialUserId(..)
	, SocialUserToken(..)
	, initFakebook
	) where

import qualified Data.Text as T

import Flaw.Social

#if defined(ghcjs_HOST_OS)

import Data.JSString.Text
import GHCJS.Types

#endif

data Fakebook = Fakebook

instance Social Fakebook where
	newtype SocialUserId Fakebook = FakebookUserId T.Text
	newtype SocialUserToken Fakebook = FakebookUserToken T.Text

#if defined(ghcjs_HOST_OS)

initFakebook :: IO Fakebook
initFakebook = do
	js_init
	return Fakebook


instance SocialClient Fakebook where
	authSocialClient Fakebook = do
		userId <- textFromJSString <$> js_userId
		if T.null userId then return Nothing
		else return $ Just (FakebookUserId userId, FakebookUserToken userId)

foreign import javascript unsafe "h$flaw_social_fakebook_init" js_init :: IO ()
foreign import javascript unsafe "$r = h$flaw_social_fakebook_user_id" js_userId :: IO JSString

#else

initFakebook :: IO Fakebook
initFakebook = return Fakebook

instance SocialServer Fakebook where
	verifySocialUserToken Fakebook (FakebookUserId userId) (FakebookUserToken userToken) = return $ userId == userToken

#endif
