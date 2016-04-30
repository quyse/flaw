{-|
Module: Flaw.Social
Description: Social networks abstract interface.
License: MIT
-}

{-# LANGUAGE CPP, TypeFamilies #-}

module Flaw.Social
	( Social(..)
#if defined(ghcjs_HOST_OS)
	, SocialClient(..)
#else
	, SocialServer(..)
#endif
	) where

#if defined(ghcjs_HOST_OS)

#else

#endif

-- | Social network abstraction.
class Social s where
	-- | User id in this social network.
	data SocialUserId s :: *
	-- | Token to prove client authentication to server.
	data SocialUserToken s :: *

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
