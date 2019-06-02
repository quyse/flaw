{-|
Module: Flaw.Script.Lua.Build
Description: Lua build definitions.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.Script.Lua.Build
  ( luaCompile
  , luaCompileFile
  , lua
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Flaw.Book
import Flaw.Build
import Flaw.Script.Lua.Chunk
import Flaw.Script.Lua.FFI

luaCompile :: B.ByteString -> T.Text -> ExpQ
luaCompile bytes name = do
  chunk <- runIO $ withBook $ \bk -> do
    luaStatePtr <- book bk luaNewState
    luaLoadChunk luaStatePtr name bytes
  luaCompileChunk chunk

luaCompileFile :: FilePath -> ExpQ
luaCompileFile fileName = do
  bytes <- BL.toStrict <$> loadFile fileName
  luaCompile bytes $ T.pack fileName

lua :: QuasiQuoter
lua = QuasiQuoter
  { quoteExp = flip luaCompile "inline" . fromString
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
