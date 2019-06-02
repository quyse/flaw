{-|
Module: Flaw.Steam
Description: General definitions for steam.
License: MIT
-}

module Flaw.Steam
  ( SteamId(..)
  , SteamAppId(..)
  ) where

import Data.Word

newtype SteamId = SteamId Word64 deriving (Eq, Ord, Show)

newtype SteamAppId = SteamAppId Word32 deriving (Eq, Ord, Show)
