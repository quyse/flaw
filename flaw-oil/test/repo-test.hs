{-|
Module: Main
Description: Tests for Oil repos.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
	( main
	) where

import Control.Monad.Reader
import Data.IORef
import Data.Monoid
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
session name m = sessionWithManifest name defaultManifest m

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

check :: Client -> T.Text -> T.Text -> SessionM ()
check Client
	{ clientName = name
	, clientRepo = repo
	} key value = lift $ do
	repoValue <- fmap T.decodeUtf8 $ clientRepoGetValue repo $ T.encodeUtf8 key
	let ok = value == repoValue
	unless ok failTest
	putStrLn $ T.unpack $ (if ok then "OK    " else "WRONG ") <> "checkValue: client=" <> name <> ", key=" <> key <> ", value=" <> value <> ", repoValue=" <> repoValue

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
		check a "k" ""
		check b "k" ""
		-- A adds (k aaa), transfer to B
		change a "k" "aaa"
		check a "k" "aaa"
		psp a
		psp b
		check b "k" "aaa"

		-- A changes (k bbb), transfer to B
		change a "k" "bbb"
		check a "k" "bbb"
		psp a
		psp b
		check b "k" "bbb"

		-- A removes k, transfer to B
		change a "k" ""
		check a "k" ""
		psp a
		psp b
		check b "k" ""

	session "simple conflicts" $ do
		a <- client "A"
		b <- client "B"

		-- add conflict
		change a "k" "aaa111"
		change b "k" "bbb111"
		check a "k" "aaa111"
		check b "k" "bbb111"
		psp a
		psp b
		psp a
		check a "k" "bbb111"
		check b "k" "bbb111"

		-- change conflict
		change a "k" "aaa222"
		change b "k" "bbb222"
		psp b
		psp a
		psp b
		check a "k" "aaa222"
		check b "k" "aaa222"

		-- remove conflict
		change a "k" "aaa333"
		change b "k" ""
		psp a
		psp b
		psp a
		check a "k" ""
		check b "k" ""

	let limitedManifest = defaultManifest
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
		check b "k1" "bbb111"
		check b "k2" "bbb222"
		psp a
		check a "k1" "bbb111"
		check a "k2" "aaa222"
		psp b
		check b "k1" "bbb111"
		check b "k2" "bbb222"
		psp a
		check a "k1" "bbb111"
		check a "k2" "bbb222"

	sessionWithManifest "partial push" limitedManifest $ do
		a <- client "A"
		b <- client "B"

		change a "k1" "aaa111"
		change a "k2" "aaa222"
		change a "k3" "aaa333"
		check a "k1" "aaa111"
		check a "k2" "aaa222"
		check a "k3" "aaa333"
		check b "k1" ""
		check b "k2" ""
		check b "k3" ""
		psp a
		psp b
		check a "k1" "aaa111"
		check a "k2" "aaa222"
		check a "k3" "aaa333"
		check b "k1" "aaa111"
		check b "k2" ""
		check b "k3" ""
		psp a
		psp b
		check a "k1" "aaa111"
		check a "k2" "aaa222"
		check a "k3" "aaa333"
		check b "k1" "aaa111"
		check b "k2" "aaa222"
		check b "k3" ""
		psp a
		psp b
		check a "k1" "aaa111"
		check a "k2" "aaa222"
		check a "k3" "aaa333"
		check b "k1" "aaa111"
		check b "k2" "aaa222"
		check b "k3" "aaa333"

	testFailed <- readIORef testFailedRef
	when testFailed exitFailure
