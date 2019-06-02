{-|
Module: Main
Description: Oil server.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Middleware.Gzip as W
import qualified Network.Wai.Middleware.RequestLogger as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as O

import Flaw.Book
import Flaw.Flow
import Flaw.Oil.Repo
import Flaw.Oil.ServerRepo

main :: IO ()
main = run =<< O.execParser parser where
  parser = O.info (O.helper <*> opts)
    (  O.fullDesc
    <> O.progDesc "Opens FILE repo and starts listening on given PORT"
    <> O.header "oild - Flaw Oil server"
    )
  opts = Options
    <$> O.strOption
      (  O.short 'f'
      <> O.metavar "FILE"
      <> O.help "Server repo file"
      )
    <*> O.option O.auto
      (  O.long "port"
      <> O.short 'p'
      <> O.value 8080 <> O.showDefault
      <> O.metavar "PORT"
      <> O.help "Port to listen at"
      )
    <*> O.option O.auto
      (  O.long "watch-delay"
      <> O.value 25000000 <> O.showDefault
      <> O.metavar "WATCH-DELAY"
      <> O.help "Delay before negative response to watch request, in μs"
      )

data Options = Options
  { optionsRepoFileName :: String
  , optionsPort :: Int
  , optionsWatchDelay :: Int
  }

run :: Options -> IO ()
run Options
  { optionsRepoFileName = repoFileName
  , optionsPort = port
  , optionsWatchDelay = watchDelay
  } = withBook $ \bk -> do

  let
    -- form manifest with limits
    manifest = defaultManifest

    -- calculate rough limit for sync requests
    syncRequestBodyLimit = manifestMaxPushItemsCount manifest *
      (manifestMaxKeySize manifest + manifestMaxValueSize manifest + 0x1000) + 0x1000

    -- limit for watch requests
    watchRequestBodyLimit = 16

  -- open repo
  repo <- book bk $ openServerRepo $ T.pack repoFileName

  -- flow for repo processing
  flow <- book bk newFlow

  -- server revision var
  revisionVar <- newTVarIO =<< serverRepoMaxRevision repo

  -- logger
  logger <- W.mkRequestLogger $ W.def
    { W.outputFormat = W.Apache W.FromFallback
    , W.autoFlush = False
    }

  -- start web server
  Warp.run port $ W.gzip W.def $ logger $ \request respond -> do

    -- helper methods
    let
      respondFail status msg = respond $ W.responseLBS status
        [(H.hContentType, "text/plain; charset=utf-8")] $ BL.fromStrict msg

    case W.queryString request of

      [("manifest", Nothing)] -> respond $ W.responseLBS H.status200
        [(H.hContentType, "application/x-flawoil-manifest")] $
        S.encodeLazy manifest

      [("sync", Nothing)] -> do
        -- read request
        body <- BL.take (fromIntegral $ syncRequestBodyLimit + 1) <$> W.lazyRequestBody request
        if BL.length body <= fromIntegral syncRequestBodyLimit then
          -- deserialize push
          case S.decodeLazy body of
            Right push -> do
              -- perform sync
              let
                userId = 1 -- TODO: implement user auth
              -- sync in a flow
              pull <- runInFlow flow $ do
                -- perform sync
                pull <- syncServerRepo repo manifest push userId
                -- update current revision
                revision <- serverRepoMaxRevision repo
                atomically $ writeTVar revisionVar revision
                return pull
              -- respond with pull
              respond $ W.responseLBS H.status200
                [(H.hContentType, "application/x-flawoil-sync")] $
                S.encodeLazy pull
            Left _ -> respondFail H.status400 "wrong push format"
        else respondFail H.status400 "too big sync request"

      [("watch", Nothing)] -> do
        body <- BL.take (watchRequestBodyLimit + 1) <$> W.lazyRequestBody request
        if BL.length body <= watchRequestBodyLimit then
          -- deserialize revision
          case S.decodeLazy body of
            Right watchRevision -> do
              -- timer
              timerVar <- registerDelay watchDelay
              -- wait until revision become bigger than watch revison, or time outs
              revision <- atomically $ do
                repoRevision <- readTVar revisionVar
                let
                  checkRevision =
                    if watchRevision < repoRevision then return repoRevision
                    else retry
                  checkTimer = do
                    timer <- readTVar timerVar
                    if timer then return repoRevision
                    else retry
                  in orElse checkRevision checkTimer
              -- respond with repo revision
              respond $ W.responseLBS H.status200
                [(H.hContentType, "application/x-flawoil-watch")] $
                S.encodeLazy revision
            Left _ -> respondFail H.status400 "wrong watch format"

        else respondFail H.status400 "too big watch request"

      _ -> respondFail H.status404 "wrong request"
