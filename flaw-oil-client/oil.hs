{-|
Module: Main
Description: Flaw Oil client.
License: MIT
-}

module Main
	( main
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
		<*> O.some (O.hsubparser
			(  O.command "sync"
				(  O.info
					(OptionSyncCommand
						<$> O.strArgument
							(  O.metavar "URL"
							<> O.help "Remote repo URL or path to local server repo file"
							)
						<*> O.switch
							(  O.long "follow"
							<> O.help "Perform sync indefinitely"
							)
						<*> O.switch
							(  O.long "display-updates"
							<> O.help "Display updates pulled from server"
							)
					) (O.fullDesc <> O.progDesc "Sync with remote repo")
				)
			<> O.command "read"
				(  O.info
					(OptionReadCommand
						<$> O.option optionByteFormatReader
							(  O.long "key-format"
							<> O.short 'f'
							<> O.metavar "KEY-FORMAT"
							<> O.value OptionByteFormatUtf8
							<> O.help ("How to serialize KEY into bytes, possible values: " ++ optionByteFormatValues)
							)
						<*> O.option optionByteFormatReader
							(  O.long "value-format"
							<> O.short 'F'
							<> O.metavar "VALUE-FORMAT"
							<> O.value OptionByteFormatUtf8
							<> O.help ("How to deserialize value from bytes, possible values: " ++ optionByteFormatValues)
							)
						<*> O.strArgument
							(  O.metavar "KEY"
							<> O.help "Key to fetch value"
							)
					) (O.fullDesc <> O.progDesc "Read value from repo")
				)
			<> O.command "write"
				(  O.info
					(OptionWriteCommand
						<$> O.option optionByteFormatReader
							(  O.long "key-format"
							<> O.short 'f'
							<> O.metavar "KEY-FORMAT"
							<> O.value OptionByteFormatUtf8
							<> O.help ("How to serialize KEY into bytes, possible values: " ++ optionByteFormatValues)
							)
						<*> O.option optionByteFormatReader
							(  O.long "value-format"
							<> O.short 'F'
							<> O.metavar "VALUE-FORMAT"
							<> O.value OptionByteFormatUtf8
							<> O.help ("How to serialize VALUE into bytes, possible values: " ++ optionByteFormatValues)
							)
						<*> O.strArgument
							(  O.metavar "KEY"
							<> O.help "Key"
							)
						<*> O.strArgument
							(  O.metavar "VALUE"
							<> O.help "Value"
							)
					) (O.fullDesc <> O.progDesc "Read value from repo")
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
			))

data Options = Options
	{ optionsLocalRepo :: String
	, optionsQuiet :: Bool
	, optionsCommands :: [OptionCommand]
	}

data OptionByteFormat
	= OptionByteFormatUtf8

optionByteFormatReader :: O.ReadM OptionByteFormat
optionByteFormatReader = O.eitherReader $ \arg -> case arg of
	"utf8" -> return OptionByteFormatUtf8
	"string" -> return OptionByteFormatUtf8
	_ -> Left $ "wrong byte format: " ++ arg

optionByteFormatValues :: String
optionByteFormatValues = intercalate ", " ["utf8", "string"]

optionByteStringToBytes :: OptionByteFormat -> String -> B.ByteString
optionByteStringToBytes format value = case format of
	OptionByteFormatUtf8 -> T.encodeUtf8 $ T.pack value

optionByteStringFromBytes :: OptionByteFormat -> B.ByteString -> IO String
optionByteStringFromBytes format bytes = case format of
	OptionByteFormatUtf8 -> case T.decodeUtf8' bytes of
		Right text -> return $ T.unpack text
		Left err -> throwIO err

data OptionCommand
	= OptionSyncCommand
		{ optionSyncCommandRemoteRepo :: String
		, optionSyncCommandFollow :: Bool
		, optionSyncCommandDisplayUpdates :: Bool
		}
	| OptionReadCommand
		{ optionReadCommandKeyFormat :: OptionByteFormat
		, optionReadCommandValueFormat :: OptionByteFormat
		, optionReadCommandKey :: String
		}
	| OptionWriteCommand
		{ optionWriteCommandKeyFormat :: OptionByteFormat
		, optionWriteCommandValueFormat :: OptionByteFormat
		, optionWriteCommandKey :: String
		, optionWriteCommandValue :: String
		}
	| OptionCheckCommand
	| OptionOptimizeCommand

run :: Options -> IO ()
run Options
	{ optionsLocalRepo = localRepoFileName
	, optionsQuiet = quiet
	, optionsCommands = commands
	} = withBook $ \bk -> do
	clientRepo <- book bk $ openClientRepo $ T.pack localRepoFileName
	forM_ commands $ \command -> case command of

		OptionSyncCommand
			{ optionSyncCommandRemoteRepo = remoteRepoUrl
			, optionSyncCommandFollow = syncFollow
			, optionSyncCommandDisplayUpdates = displayUpdates
			} -> do
			httpManager <- H.newManager H.defaultManagerSettings
			(remoteRepo, manifest) <- initHttpRemoteRepo httpManager (T.pack remoteRepoUrl)

			let syncStep = do
				-- perform sync
				ClientRepoPullInfo
					{ clientRepoPullLag = lag
					, clientRepoPullChanges = changes
					} <- syncClientRepo clientRepo manifest $ syncHttpRemoteRepo remoteRepo

				-- display changes if requested
				when displayUpdates $ mapM_ print changes

				-- determine if we need to sync more
				when (lag > 0) syncStep

			let watchStep = do
				-- get client revision
				clientRevision <- clientRepoRevision clientRepo
				-- watch for changes
				serverRevision <- watchHttpRemoteRepo remoteRepo clientRevision
				-- if server revision is greater, perform sync
				when (clientRevision < serverRevision) syncStep
				watchStep

			if syncFollow then watchStep
			else syncStep

		OptionReadCommand
			{ optionReadCommandKeyFormat = keyFormat
			, optionReadCommandValueFormat = valueFormat
			, optionReadCommandKey = keyStr
			} -> do
			value <- clientRepoGetValue clientRepo $ optionByteStringToBytes keyFormat keyStr
			putStrLn =<< optionByteStringFromBytes valueFormat value

		OptionWriteCommand
			{ optionWriteCommandKeyFormat = keyFormat
			, optionWriteCommandValueFormat = valueFormat
			, optionWriteCommandKey = keyStr
			, optionWriteCommandValue = valueStr
			} -> do
			clientRepoChange clientRepo (optionByteStringToBytes keyFormat keyStr) (optionByteStringToBytes valueFormat valueStr)

		OptionCheckCommand -> do
			(ok, desc) <- repoDbCheckIntegrity $ repoDb clientRepo
			unless quiet $ putStr $ T.unpack desc
			if ok then exitSuccess else exitFailure

		OptionOptimizeCommand -> repoDbVacuum $ repoDb clientRepo
