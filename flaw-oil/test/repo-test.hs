{-|
Module: Main
Description: Tests for Oil repos.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main
  ( main
  ) where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Exit
import System.IO.Unsafe

import Flaw.Book
import Flaw.Oil.ClientRepo
import Flaw.Oil.Repo
import Flaw.Oil.ServerRepo

data Client = Client
  { clientName :: T.Text
  , clientRepo :: ClientRepo
  , clientManifest :: Manifest
  }

data Session = Session
  { sessionBook :: Book
  , sessionManifest :: Manifest
  , sessionServerRepo :: ServerRepo
  }

type SessionM = ReaderT Session IO

session :: T.Text -> SessionM () -> IO ()
session name = sessionWithManifest name defaultManifest

sessionWithManifest :: T.Text -> Manifest -> SessionM () -> IO ()
sessionWithManifest name manifest m = withBook $ \bk -> do
  putStrLn $ T.unpack $ "session started: " <> name
  serverRepo <- book bk $ openServerRepo ":memory:"
  runReaderT m Session
    { sessionBook = bk
    , sessionManifest = manifest
    , sessionServerRepo = serverRepo
    }
  putStrLn $ T.unpack $ "session ended: " <> name

client :: T.Text -> SessionM Client
client name = do
  Session
    { sessionBook = bk
    , sessionManifest = manifest
    } <- ask
  repo <- lift $ book bk $ openClientRepo ":memory:"
  return Client
    { clientName = name
    , clientRepo = repo
    , clientManifest = manifest
    }

check :: Client -> T.Text -> Revision -> T.Text -> SessionM ()
check Client
  { clientName = name
  , clientRepo = repo
  } key revision value = lift $ do
  (repoRevision, T.decodeUtf8 -> repoValue) <- clientRepoGetRevisionValue repo $ T.encodeUtf8 key
  let ok = revision == repoRevision && value == repoValue
  unless ok failTest
  putStrLn $ T.unpack $ (if ok then "OK    " else "WRONG ")
    <> "check: client=" <> name <> ", key=" <> key <> ", value=" <> value <> ", repoValue=" <> repoValue
    <> ", rev=" <> T.pack (show revision) <> ", repoRev=" <> T.pack (show repoRevision)

checkPrefixed :: Client -> T.Text -> [T.Text] -> SessionM ()
checkPrefixed Client
  { clientName = name
  , clientRepo = repo
  } keyPrefix keys = lift $ do
  (map T.decodeUtf8 -> repoKeys) <- clientRepoGetKeysPrefixed repo $ T.encodeUtf8 keyPrefix
  let ok = keys == repoKeys
  unless ok failTest
  putStrLn $ T.unpack $ (if ok then "OK    " else "WRONG ")
    <> "checkPrefixed: client=" <> name <> ", keyPrefix=" <> keyPrefix
    <> ", keys=" <> T.pack (show keys) <> ", repoKeys=" <> T.pack (show repoKeys)

change :: Client -> T.Text -> T.Text -> SessionM ()
change Client
  { clientName = name
  , clientRepo = repo
  } key value = lift $ do
  clientRepoChange repo (T.encodeUtf8 key) (T.encodeUtf8 value)
  putStrLn $ T.unpack $ "      change: client=" <> name <> ", key=" <> key <> ", value=" <> value

psp :: Client -> SessionM ()
psp Client
  { clientName = cn
  , clientRepo = cr
  , clientManifest = cm
  } = do
  Session
    { sessionManifest = sm
    , sessionServerRepo = sr
    } <- ask
  (push, crps) <- lift $ pushClientRepo cr cm
  pull <- lift $ syncServerRepo sr sm push 1
  lift $ void $ pullClientRepo cr pull crps
  lift $ putStrLn $ T.unpack $ "      psp: client=" <> cn

testFailedRef :: IORef Bool
{-# NOINLINE testFailedRef #-}
testFailedRef = unsafePerformIO $ newIORef False

failTest :: IO ()
failTest = writeIORef testFailedRef True

main :: IO ()
main = do
  session "simple transfers" $ do
    a <- client "A"
    b <- client "B"
    -- null check
    check a "k" 0 ""
    check b "k" 0 ""
    -- A adds (k aaa), transfer to B
    change a "k" "aaa"
    check a "k" 0 "aaa"
    psp a
    check a "k" 1 "aaa"
    psp b
    check b "k" 1 "aaa"

    -- A changes (k bbb), transfer to B
    change a "k" "bbb"
    check a "k" 0 "bbb"
    psp a
    check a "k" 2 "bbb"
    psp b
    check b "k" 2 "bbb"

    -- A removes k, transfer to B
    change a "k" ""
    check a "k" 0 ""
    psp a
    check a "k" 3 ""
    psp b
    check b "k" 3 ""

  session "simple conflicts" $ do
    a <- client "A"
    b <- client "B"

    -- add conflict
    change a "k" "aaa111"
    change b "k" "bbb111"
    check a "k" 0 "aaa111"
    check b "k" 0 "bbb111"
    psp a
    check a "k" 1 "aaa111"
    psp b
    check b "k" 2 "bbb111"
    psp a
    check a "k" 2 "bbb111"
    check b "k" 2 "bbb111"

    -- change conflict
    change a "k" "aaa222"
    change b "k" "bbb222"
    psp b
    check b "k" 3 "bbb222"
    psp a
    check a "k" 4 "aaa222"
    psp b
    check a "k" 4 "aaa222"
    check b "k" 4 "aaa222"

    -- remove conflict
    change a "k" "aaa333"
    check a "k" 0 "aaa333"
    change b "k" ""
    check b "k" 0 ""
    psp a
    check a "k" 5 "aaa333"
    psp b
    check b "k" 6 ""
    psp a
    check a "k" 6 ""
    check b "k" 6 ""

  let
    limitedManifest = defaultManifest
      { manifestMaxPushItemsCount = 1
      , manifestMaxPullItemsCount = 1
      }

  sessionWithManifest "pull conflict" limitedManifest $ do
    a <- client "A"
    b <- client "B"

    change a "k2" "aaa222"
    psp a
    change b "k1" "bbb111"
    change b "k2" "bbb222"
    psp b
    check b "k1" 2 "bbb111"
    check b "k2" 0 "bbb222"
    psp a
    check a "k1" 2 "bbb111"
    check a "k2" 1 "aaa222"
    psp b
    check b "k1" 2 "bbb111"
    check b "k2" 3 "bbb222"
    psp a
    check a "k1" 2 "bbb111"
    check a "k2" 3 "bbb222"

  sessionWithManifest "partial push" limitedManifest $ do
    a <- client "A"
    b <- client "B"

    change a "k1" "aaa111"
    change a "k2" "aaa222"
    change a "k3" "aaa333"
    check a "k1" 0 "aaa111"
    check a "k2" 0 "aaa222"
    check a "k3" 0 "aaa333"
    check b "k1" 0 ""
    check b "k2" 0 ""
    check b "k3" 0 ""
    psp a
    psp b
    check a "k1" 1 "aaa111"
    check a "k2" 0 "aaa222"
    check a "k3" 0 "aaa333"
    check b "k1" 1 "aaa111"
    check b "k2" 0 ""
    check b "k3" 0 ""
    psp a
    psp b
    check a "k1" 1 "aaa111"
    check a "k2" 2 "aaa222"
    check a "k3" 0 "aaa333"
    check b "k1" 1 "aaa111"
    check b "k2" 2 "aaa222"
    check b "k3" 0 ""
    psp a
    psp b
    check a "k1" 1 "aaa111"
    check a "k2" 2 "aaa222"
    check a "k3" 3 "aaa333"
    check b "k1" 1 "aaa111"
    check b "k2" 2 "aaa222"
    check b "k3" 3 "aaa333"

  session "prefixed" $ do
    a <- client "A"
    b <- client "B"

    change a "k1" "a1"
    checkPrefixed a "k" ["k1"]
    psp a
    checkPrefixed a "k" ["k1"]
    change a "k2" "a2"
    checkPrefixed a "k" ["k1", "k2"]
    psp a
    checkPrefixed a "k" ["k1", "k2"]
    change a "k1" ""
    checkPrefixed a "k" ["k1", "k2"]
    psp a
    checkPrefixed a "k" ["k1", "k2"]
    psp b
    change b "k1" "b1"
    psp b
    psp a
    checkPrefixed a "k" ["k1", "k2"]
    change b "k2" ""
    change b "k3" "b3"
    change b "z1" "z1"
    psp b
    psp a
    checkPrefixed a "k" ["k1", "k2", "k3"]
    checkPrefixed a "z" ["z1"]

  testFailed <- readIORef testFailedRef
  when testFailed exitFailure
