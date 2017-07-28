{-|
Module: Main
Description: Flaw Editor.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main
	( main
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client as H
import qualified Options.Applicative as O

import Flaw.App
import Flaw.Book
import Flaw.Editor.EditableEntity
import Flaw.Editor.Entity
import Flaw.Editor.Entity.Sync
import Flaw.Editor.Project
import Flaw.Flow
import Flaw.Graphics
import Flaw.Math
import Flaw.Oil.ClientRepo
import Flaw.Oil.RemoteRepo
import Flaw.UI
import Flaw.UI.DefaultStyle
import Flaw.UI.Drawer
import Flaw.UI.Frame
import Flaw.UI.Label
import Flaw.UI.Layout
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.Popup
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
		<*> O.option O.auto
			(  O.short 'j'
			<> O.long "jobs"
			<> O.metavar "JOBS"
			<> O.value 0
			<> O.help "Jobs count"
			)
		<*> O.switch
			(  O.short 'g'
			<> O.help "Debug mode"
			)

data Options = Options
	{ optionsProject :: !String
	, optionsUser :: !String
	, optionsJobs :: {-# UNPACK #-} !Int
	, optionsDebug :: !Bool
	}

run :: Options -> IO ()
run Options
	{ optionsProject = optionProject
	, optionsUser = optionUser
	, optionsJobs = optionJobs
	, optionsDebug = optionDebug
	} = withApp def
	{ appConfigTitle = "FLAW Editor"
	, appConfigDebug = optionDebug
	} $ \window device context presenter inputManager -> withBook $ \bk -> do

	-- flows for async operations
	backgroundFlow <- book bk newFlow
	processingFlow <- book bk . newMultiFlow =<< if optionJobs > 0 then return optionJobs else getNumCapabilities

	-- UI styles and drawer
	let metrics@Metrics
		{ metricsGap = gap
		, metricsBigGap = bigGap
		, metricsFrameClient = Vec4 frameLeft frameTop frameRight frameBottom
		, metricsLabelSize = Vec2 labelWidth labelHeight
		} = defaultStyleMetrics
	drawer <- book bk $ initDefaultStyleDrawer device

	exitVar <- newTVarIO False

	-- init UI and run app
	join $ atomically $ do
		windowPanel <- newPanel True
		mainWindow <- newWindow window inputManager windowPanel
		setWindowCloseHandler mainWindow $ writeTVar exitVar True

		-- main frame
		syncInfoLabel <- do
			mainPanel <- newPanel False
			syncInfoLabel <- newTextLabel
			syncInfoLabelVE <- newVisualElement syncInfoLabel
			syncInfoLabelVEChild <- addFreeChild mainPanel syncInfoLabelVE
			placeFreeChild mainPanel syncInfoLabelVEChild $ Vec2 bigGap bigGap
			setLayoutHandler mainPanel $ \_ -> do
				layoutElement syncInfoLabelVE $ Vec2 labelWidth labelHeight
			mainFrame <- newFrame mainPanel metrics
			mainFrameChild <- addFreeChild windowPanel mainFrame
			layoutElement mainFrame $ Vec2 (frameLeft + bigGap + labelWidth + bigGap + frameRight) (frameTop + bigGap + labelHeight + bigGap + frameBottom)
			setSelfFreeChild mainFrame windowPanel mainFrameChild True
			setText mainFrame "FLAW"
			return syncInfoLabel

		-- init popup service
		popupService <- newPopupService metrics windowPanel

		-- entity frame
		let openEntityFrame var@SomeEntityVar
			{ someEntityVarEntityManager = entityManager
			, someEntityVarEntityId = entityId
			} = do
			panel <- newPanel False
			frame <- newFrame panel metrics
			layoutPanelChildVar <- newTVar Nothing
			asyncRunInFlow backgroundFlow $ editableLayoutForEntityId EditableLayoutState
				{ elsFlow = backgroundFlow
				, elsBook = bk
				, elsEntityManager = entityManager
				, elsPopupService = popupService
				} entityId $ \layout -> do
				-- set frame's title to datatype name
				do
					SomeEntity entity <- readSomeEntityVar var
					let typeName = case interfaceEntity (Proxy :: Proxy EditableEntity) entity of
						EntityInterfaced -> editableEntityTypeName entity <> " "
						EntityNotInterfaced -> ""
					setText frame $ typeName <> entityToText entityId
				-- remove previous layout panel from frame, if any
				maybeOldLayoutPanelChild <- readTVar layoutPanelChildVar
				case maybeOldLayoutPanelChild of
					Just oldLayoutPanelChild -> removeFreeChild panel oldLayoutPanelChild
					Nothing -> return ()
				layoutPanel <- newPanel False
				layoutPanelChild <- addFreeChild panel layoutPanel
				writeTVar layoutPanelChildVar $ Just layoutPanelChild
				(r, FlowLayoutState
					{ flsLayoutHandler = layoutHandler
					}) <- runStateT layout FlowLayoutState
					{ flsMetrics = metrics
					, flsParentElement = layoutPanel
					, flsLayoutHandler = return
					, flsPreSize = Vec2 0 0
					}
				setLayoutHandler layoutPanel $ \(Vec2 sx sy) -> void $ layoutHandler $ Vec4 0 0 sx sy
				setLayoutHandler panel $ \(Vec2 sx sy) -> do
					layoutElement layoutPanel $ Vec2 (sx - bigGap * 2) (sy - bigGap * 2)
					placeFreeChild panel layoutPanelChild $ Vec2 bigGap bigGap
				return r
			frameChild <- addFreeChild windowPanel frame
			setSelfFreeChild frame windowPanel frameChild True
			placeFreeChild windowPanel frameChild $ Vec2 100 100
			layoutElement frame $ Vec2 300 300

		-- load project, open repos
		asyncRunInFlow backgroundFlow $ do
			-- load project
			Project
				{ projectRemoteRepoUrl = remoteRepoUrl
				, projectLocalRepoPath = localRepoPath
				, projectProcessingCachePath = processingCachePath
				, projectProcessingCacheMaxAge = processingCacheMaxAge
				} <- either throwIO return =<< Y.decodeFileEither optionProject

			-- open client repo
			clientRepo <- book bk $ openClientRepo localRepoPath
			httpManager <- H.newManager H.defaultManagerSettings

			-- open remote repo
			(remoteRepo, manifest) <- initHttpRemoteRepo httpManager remoteRepoUrl

			-- create entity manager
			entityManager <- book bk $ newSyncedEntityManager clientRepo remoteRepo manifest $ \ClientRepoPullInfo
				{ clientRepoPullRevision = revision
				, clientRepoPullLag = lag
				, clientRepoPullChanges = changes
				} ->
				-- update UI
				setText syncInfoLabel $ T.pack $ "r" <> show revision <> (if lag > 0 then "+" <> show lag else "")

			-- register entities
			$registerEntitiesAndInterfacesExp entityManager

			-- open root entity
			atomically . openEntityFrame =<< getSomeEntityVar entityManager (case projectRootEntityPtr of EntityPtr rootEntityId -> rootEntityId)

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
