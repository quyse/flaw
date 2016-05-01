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

import qualified Data.ByteString as B
import qualified Data.Serialize as S

#if defined(ghcjs_HOST_OS)

#else

#endif

-- | Social network abstraction.
class (S.Serialize (SocialUserId s), S.Serialize (SocialUserToken s)) => Social s where
	-- | User id in this social network.
	data SocialUserId s :: *
	-- | Token to prove client authentication to server.
	data SocialUserToken s :: *
	-- | Get universal user id.
	socialUniversalUserId :: SocialUserId s -> B.ByteString

#if defined(ghcjs_HOST_OS)

-- | Social network, client-side.
class Social s => SocialClient s where
	-- | Authenticate client.
	authSocialClient :: s -> IO (Maybe (SocialUserId s, SocialUserToken s))

#else

-- | Social network, server-side.
class Social s => SocialServer s where
	-- | Check validity of a client.
	verifySocialUserToken :: s -> SocialUserId s -> SocialUserToken s -> IO Bool

#endif
