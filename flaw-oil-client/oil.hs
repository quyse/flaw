{-|
Module: Main
Description: Flaw Oil client.
License: MIT
-}

module Main
	( main
	) where

import Control.Concurrent.STM
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Network.HTTP.Client as H
import qualified Options.Applicative as O
import System.Exit

import Flaw.Book
import Flaw.Oil.ClientRepo
import Flaw.Oil.RemoteRepo
import Flaw.Oil.Repo

main :: IO ()
main = run =<< O.execParser parser where
	parser = O.info (O.helper <*> opts)
		(  O.fullDesc
		<> O.progDesc "Client for Flaw Oil server"
		<> O.header "oil - Flaw Oil client"
		)
	opts = Options
		<$> O.strOption
			(  O.short 'f'
			<> O.metavar "FILE"
			<> O.help "Path to local client repo file"
			)
		<*> O.switch
			(  O.long "quiet"
			<> O.short 'q'
			<> O.help "Be quiet, and only signal success or failure by exit code"
			)
		<*> O.hsubparser
			(  O.command "sync"
				(  O.info
					(OptionSyncCommand
						<$> O.strArgument
							(  O.metavar "URL"
							<> O.help "Remote repo URL or path to local server repo file"
							)
						<*> O.switch
							(  O.long "once"
							<> O.help "Perform sync just once and exit"
							)
					) (O.fullDesc <> O.progDesc "Sync with remote repo")
				)
			<> O.command "check"
				(  O.info
					(pure OptionCheckCommand)
					(O.fullDesc <> O.progDesc "Check client repo integrity")
				)
			<> O.command "optimize"
				(  O.info
					(pure OptionOptimizeCommand)
					(O.fullDesc <> O.progDesc "Optimize client repo storage")
				)
			)

data Options = Options
	{ optionsLocalRepo :: String
	, optionsQuiet :: Bool
	, optionsCommand :: OptionCommand
	}

data OptionCommand
	= OptionSyncCommand
		{ optionSyncCommandRemoteRepo :: String
		, optionSyncCommandOnce :: Bool
		}
	| OptionCheckCommand
	| OptionOptimizeCommand

run :: Options -> IO ()
run Options
	{ optionsLocalRepo = localRepoFileName
	, optionsQuiet = quiet
	, optionsCommand = command
	} = withBook $ \bk -> do
	clientRepo <- book bk $ openClientRepo $ T.pack localRepoFileName
	case command of
		OptionSyncCommand
			{ optionSyncCommandRemoteRepo = remoteRepoUrl
			, optionSyncCommandOnce = syncOnce
			} -> do
			httpManager <- H.newManager H.defaultManagerSettings
			remoteRepo <- book bk $ initHttpRemoteRepo httpManager clientRepo $ T.pack remoteRepoUrl
			notificationsChan <- atomically $ remoteRepoNotificationsChan remoteRepo
			let step = do
				notification <- atomically $ readTChan notificationsChan
				when (not quiet) $ putStrLn $ show notification
				case notification of
					RemoteRepoError _ -> exitFailure
					_ -> when (not syncOnce) step
			step
		OptionCheckCommand -> do
			(ok, desc) <- repoDbCheckIntegrity $ repoDb clientRepo
			when (not quiet) $ putStr $ T.unpack desc
			if ok then exitSuccess else exitFailure
		OptionOptimizeCommand -> repoDbVacuum $ repoDb clientRepo
