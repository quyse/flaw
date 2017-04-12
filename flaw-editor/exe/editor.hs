{-|
Module: Main
Description: Flaw Editor.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

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
import Flaw.Editor.Entity.Sync
import Flaw.Editor.Project
import Flaw.Flow
import Flaw.Graphics
import Flaw.Math
import Flaw.Oil.ClientRepo
import Flaw.Oil.RemoteRepo
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
		<$> O.strArgument
			(  O.metavar "PROJECT"
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
			asyncRunInFlow flow $ do
				clientRepo <- book bk $ openClientRepo localRepoPath
				httpManager <- H.newManager H.defaultManagerSettings
				(remoteRepo, manifest) <- initHttpRemoteRepo httpManager remoteRepoUrl

				-- create entity manager
				void $ newSyncedEntityManager clientRepo remoteRepo manifest $ \ClientRepoPullInfo
					{ clientRepoPullRevision = revision
					, clientRepoPullLag = lag
					, clientRepoPullChanges = changes
					} -> do
					-- update UI
					setText syncInfoLabel $ T.pack $ "client revision: " <> show revision <> (if lag > 0 then " | pull lag: " <> show lag else "")
					-- DEBUG: display changes
					asyncRunInFlow flow $ mapM_ print changes

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
