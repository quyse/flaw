{-|
Module: Flaw.Js
Description: General javascript things.
License: MIT
-}

module Flaw.Js
	( initJs
	) where

-- Even if nothing has to be done, the module has to contain some code called by some other
-- code (currently flaw-app calls it), otherwise it's excluded from linking together with javascript sources.

initJs :: IO ()
initJs = return ()
