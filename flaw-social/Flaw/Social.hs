{-|
Module: Flaw.Social
Description: Social networks abstract interface.
License: MIT
-}

{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}

module Flaw.Social
  ( Social(..)
#if defined(ghcjs_HOST_OS)
  , SocialClient(..)
#else
  , SocialServer(..)
#endif
  ) where

import qualified Data.Serialize as S
import qualified Data.Text as T

#if defined(ghcjs_HOST_OS)

#else

import Control.Monad.IO.Class

#endif

-- | Social network abstraction.
class (S.Serialize (SocialUserId s), S.Serialize (SocialUserToken s)) => Social s where
  -- | User id in this social network.
  data SocialUserId s :: *
  -- | Token to prove client authentication to server.
  data SocialUserToken s :: *
  -- | Get universal user id.
  socialUniversalUserId :: SocialUserId s -> T.Text

#if defined(ghcjs_HOST_OS)

-- | Social network, client-side.
class Social s => SocialClient s where
  -- | Authenticate client.
  authSocialClient :: s -> IO (Maybe (SocialUserId s, SocialUserToken s))

#else

-- | Social network, server-side.
class Social s => SocialServer s where
  -- | Authenticate client using data from main GET request to app's page.
  authSocialClientByRequest :: MonadIO m => s -> (T.Text -> m (Maybe T.Text)) -> m (Maybe (SocialUserId s))
  -- | Check validity of a client.
  verifySocialUserToken :: s -> SocialUserId s -> SocialUserToken s -> IO Bool

#endif
