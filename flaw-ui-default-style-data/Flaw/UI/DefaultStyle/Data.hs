{-|
Module: Flaw.UI.DefaultStyle.Data
Description: Embedded data for default style.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.UI.DefaultStyle.Data
  ( loadFontData
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Flaw.Build

loadFontData :: IO B.ByteString
loadFontData = $(embedIOExp =<< BL.toStrict <$> loadFile "src/DejaVuSans.ttf")
