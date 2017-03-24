{-|
Module: Flaw.Steam
Description: General definitions for steam.
License: MIT
-}

module Flaw.Steam
	( SteamId(..)
	) where

import Data.Word

newtype SteamId = SteamId Word64 deriving (Eq, Ord, Show)
