{-|
Module: Main
Description: Flaw Oil client.
License: MIT
-}

module Main
	( main
	) where

import Control.Concurrent.STM
import Data.Monoid
import qualified Data.Text as T
import qualified Network.HTTP.Client as H
import qualified Options.Applicative as O

import Flaw.Book
import Flaw.Oil.ClientRepo
import Flaw.Oil.RemoteRepo

main :: IO ()
main = run =<< O.execParser parser where
	parser = O.info (O.helper <*> opts)
		(  O.fullDesc
		<> O.progDesc "Client for Flaw Oil server"
		<> O.header "oil - Flaw Oil client"
		)
	opts = Options
		<$> O.strOption
			(  O.short 'r'
			<> O.metavar "URL"
			<> O.help "remote repo URL or path to local server repo file"
			)
		<*> O.strOption
			(  O.short 'f'
			<> O.metavar "FILE"
			<> O.help "path to local client repo file"
			)

data Options = Options
	{ optionsRemoteRepo :: String
	, optionsLocalRepo :: String
	}

run :: Options -> IO ()
run Options
	{ optionsRemoteRepo = remoteRepoUrl
	, optionsLocalRepo = localRepoFileName
	} = withBook $ \bk -> do
	clientRepo <- book bk $ openClientRepo $ T.pack localRepoFileName
	httpManager <- H.newManager H.defaultManagerSettings
	remoteRepo <- book bk $ initHttpRemoteRepo httpManager clientRepo $ T.pack remoteRepoUrl
	notificationsChan <- atomically $ remoteRepoNotificationsChan remoteRepo
	let step = do
		notification <- atomically $ readTChan notificationsChan
		putStrLn $ show notification
		case notification of
			RemoteRepoError _ -> return ()
			_ -> step
	step
