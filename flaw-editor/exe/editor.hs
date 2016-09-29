{-|
Module: Main
Description: Flaw Editor.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Main
	( main
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client as H
import qualified Options.Applicative as O

import Flaw.App
import Flaw.Book
import Flaw.Editor.Project
import Flaw.Flow
import Flaw.Graphics
import Flaw.Math
import Flaw.Oil.ClientRepo
import Flaw.Oil.Entity
import Flaw.Oil.RemoteRepo
import Flaw.Oil.Repo
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.DefaultStyle
import Flaw.UI.Drawer
import Flaw.UI.EditBox
import Flaw.UI.Label
import Flaw.UI.Layout
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.VisualElement
import Flaw.UI.Window

main :: IO ()
main = run =<< O.execParser parser where
	parser = O.info (O.helper <*> opts)
		(  O.fullDesc
		<> O.progDesc "Opens visual editor for working on game data."
		<> O.header "flaw-editor"
		)
	opts = Options
		<$> O.strOption
			(  O.short 'p'
			<> O.long "project"
			<> O.metavar "PROJECT"
			<> O.value ""
			<> O.help "Project file name"
			)
		<*> O.strOption
			(  O.short 'u'
			<> O.long "user"
			<> O.metavar "USER"
			<> O.value ""
			<> O.help "User name"
			)
		<*> O.switch
			(  O.short 'g'
			<> O.help "Debug mode"
			)

data Options = Options
	{ optionsProject :: !String
	, optionsUser :: !String
	, optionsDebug :: !Bool
	}

run :: Options -> IO ()
run Options
	{ optionsProject = optionProject
	, optionsUser = optionUser
	, optionsDebug = optionDebug
	} = withApp appConfig
	{ appConfigTitle = "FLAW Editor"
	, appConfigDebug = optionDebug
	} $ \window device context presenter inputManager -> withBook $ \bk -> do

	-- flow for async operations
	flow <- book bk newFlow

	-- UI styles and drawer
	let metrics@Metrics
		{ metricsLabelSize = Vec2 _labelWidth labelHeight
		} = defaultStyleMetrics
	drawer <- book bk $ initDefaultStyleDrawer device

	exitVar <- newTVarIO False

	-- load project
	Project
		{ projectRemoteRepoUrl = initialRemoteRepoUrl
		, projectLocalRepoPath = initialLocalRepoPath
		, projectProcessingCachePath = initialProcessingCachePath
		, projectProcessingCacheMaxAge = initialProcessingCacheMaxAge
		} <- either throwIO return =<< Y.decodeFileEither optionProject

	-- init UI and run app
	join $ atomically $ do
		windowPanel <- newPanel True
		mainWindow <- newWindow window inputManager windowPanel
		setWindowCloseHandler mainWindow $ writeTVar exitVar True

		-- init main window UI
		syncInfoLabel <- newTextLabel
		syncInfoLabelVE <- newVisualElement syncInfoLabel
		syncInfoLabelVEChild <- addFreeChild windowPanel syncInfoLabelVE
		setLayoutHandler windowPanel $ \(Vec2 sx sy) -> do
			layoutElement syncInfoLabelVE $ Vec2 sx labelHeight
			placeFreeChild windowPanel syncInfoLabelVEChild $ Vec2 0 (sy - labelHeight)

		-- show dialog asking for repo
		remoteRepoEditBox <- newEditBox
		setText remoteRepoEditBox initialRemoteRepoUrl
		userEditBox <- newEditBox
		setText userEditBox $ T.pack optionUser
		passwordEditBox <- newEditBox
		setPasswordMode passwordEditBox True
		localRepoEditBox <- newEditBox
		setText localRepoEditBox initialLocalRepoPath
		connectButton <- newLabeledButton "connect"
		cancelButton <- newLabeledButton "cancel"
		frame <- frameFlowLayout metrics $ do
			titleInFlowLayout "remote repo"
			labeledFlowLayout "url:" $ elementInFlowLayout remoteRepoEditBox
			titleInFlowLayout "credentials"
			labeledFlowLayout "user:" $ elementInFlowLayout userEditBox
			labeledFlowLayout "password:" $ elementInFlowLayout passwordEditBox
			titleInFlowLayout "local repo"
			labeledFlowLayout "path:" $ elementInFlowLayout localRepoEditBox
			okCancelButtonsInFlowLayout connectButton cancelButton

		setText frame "connect to repo"
		frameChild <- addFreeChild windowPanel frame
		setSelfFreeChild frame windowPanel frameChild True
		setActionHandler connectButton $ do
			remoteRepoUrl <- getText remoteRepoEditBox
			_user <- getText userEditBox
			_password <- getText passwordEditBox
			localRepoPath <- getText localRepoEditBox
			removeFreeChild windowPanel frameChild
			asyncRunInFlow flow $ mdo
				clientRepo <- book bk $ openClientRepo localRepoPath
				httpManager <- H.newManager H.defaultManagerSettings
				(remoteRepo, manifest) <- initHttpRemoteRepo httpManager remoteRepoUrl

				-- repo sync flow
				syncFlow <- book bk newFlow
				-- client repo operations flow
				clientRepoFlow <- book bk newFlow
				syncInProgressVar <- newTVarIO False -- sync already started
				needSyncVar <- newTVarIO False -- more changes need to push since sync start
				-- create entity manager
				entityManager <- newEntityManager clientRepoFlow clientRepo startSync
				let startSync = let
					doSync = do
						writeTVar needSyncVar False
						asyncRunInFlow clientRepoFlow $ do
							(push, pushState) <- pushClientRepo clientRepo manifest
							-- sync with remote repo in sync flow
							atomically $ asyncRunInFlow syncFlow $ do
								pull <- syncHttpRemoteRepo remoteRepo push
								-- update UI
								-- merge pull info in client repo flow
								atomically $ asyncRunInFlow clientRepoFlow $ do
									ClientRepoPullInfo
										{ clientRepoPullRevision = revision
										, clientRepoPullLag = lag
										, clientRepoPullChanges = changes
										} <- pullClientRepo clientRepo pull pushState
									atomically $ do
										-- notify entity manager
										pullEntityManager entityManager changes
										-- update UI
										setText syncInfoLabel $ T.pack $ "client revision: " <> show revision <> (if lag > 0 then " | pull lag: " <> show lag else "")
										-- DEBUG: display changes
										asyncRunInFlow syncFlow $ mapM_ print changes
										-- check if we need to sync again
										needSync <- readTVar needSyncVar
										-- if there was no items to push and pull lag is zero, and startSync hasn't been called again
										if null (pushItems push) && lag <= 0 && not needSync then
											-- then sync is over
											writeTVar syncInProgressVar False
										else
											-- otherwise repeat sync
											doSync
					in atomically $ do
					-- have only one sync at a time
					syncInProgress <- readTVar syncInProgressVar
					if syncInProgress then writeTVar needSyncVar True
					else do
						writeTVar syncInProgressVar True
						doSync

				-- perform initial sync
				startSync

				-- start watch flow
				book bk $ forkFlow $ forever $ do
					-- don't do watch requests if sync is in progress
					atomically $ do
						syncInProgress <- readTVar syncInProgressVar
						when syncInProgress retry
					-- get client revision
					clientRevision <- clientRepoRevision clientRepo
					-- watch for changes
					serverRevision <- watchHttpRemoteRepo remoteRepo clientRevision
					-- if server revision is greater, perform sync
					when (clientRevision < serverRevision) startSync

		setActionHandler cancelButton $ removeFreeChild windowPanel frameChild

		-- run app
		return $ runApp $ \frameTime -> do
			-- process input and window events
			processWindow mainWindow

			-- update drawer
			atomically $ setDrawerFrameTime drawer frameTime

			-- render window
			windowRender <- atomically $ renderWindow mainWindow drawer

			render context $ present presenter $ do
				renderClearColor 0 $ Vec4 0 0 0 1
				renderDepthTestFunc DepthTestFuncAlways

				windowRender

			exit <- atomically $ readTVar exitVar
			when exit exitApp
