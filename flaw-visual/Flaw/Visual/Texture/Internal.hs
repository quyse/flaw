{-|
Module: Flaw.Visual.Texture.Internal
Description: Texture support.
License: MIT
-}

{-# LANGUAGE DeriveGeneric #-}

module Flaw.Visual.Texture.Internal
  ( PackedTexture(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Serialize as S
import GHC.Generics(Generic)

import Flaw.Graphics.Texture

data PackedTexture = PackedTexture
  { packedTextureBytes :: !B.ByteString
  , packedTextureInfo :: !TextureInfo
  } deriving Generic

instance S.Serialize PackedTexture

