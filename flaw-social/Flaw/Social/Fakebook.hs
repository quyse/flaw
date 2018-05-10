{-|
Module: Flaw.Social.Fakebook
Description: Fake social network, only for testing.
License: MIT
-}

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}

module Flaw.Social.Fakebook
	( Fakebook
	, SocialUserId(..)
	, SocialUserToken(..)
	, initFakebook
	) where

import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T

import Flaw.Social

#if defined(ghcjs_HOST_OS)

import Data.JSString.Text
import GHCJS.Types

#else

#endif

data Fakebook = Fakebook

instance Social Fakebook where
	newtype SocialUserId Fakebook = FakebookUserId T.Text deriving S.Serialize
	newtype SocialUserToken Fakebook = FakebookUserToken T.Text deriving S.Serialize
	socialUniversalUserId (FakebookUserId userId) = "fakebook_" <> userId

#if defined(ghcjs_HOST_OS)

initFakebook :: IO Fakebook
initFakebook = do
	js_init
	return Fakebook

instance SocialClient Fakebook where
	authSocialClient Fakebook = do
		userId <- textFromJSString <$> js_userId
		if B.null userId then return Nothing
		else return $ Just (FakebookUserId userId, FakebookUserToken userId)

foreign import javascript unsafe "h$flaw_social_fakebook_init" js_init :: IO ()
foreign import javascript unsafe "$r = h$flaw_social_fakebook_user_id" js_userId :: IO JSString

#else

initFakebook :: IO Fakebook
initFakebook = return Fakebook

instance SocialServer Fakebook where
	authSocialClientByRequest Fakebook getParam = (FakebookUserId <$>) <$> getParam "fakebook_user_id"
	verifySocialUserToken Fakebook (FakebookUserId userId) (FakebookUserToken userToken) = return $ userId == userToken

#endif
