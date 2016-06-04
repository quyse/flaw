{-|
Module: Flaw.UI.Editor.FileDialog
Description: Dialog allowing user to select a file.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.UI.Editor.FileDialog
	( FileDialogService(..)
	, newFileDialogService
	, FileDialogConfig(..)
	, runFileDialog
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Numeric
import System.Directory
import System.FilePath
import qualified System.IO as IO

import Flaw.Flow
import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.Frame
import Flaw.UI.Label
import Flaw.UI.ListBox
import Flaw.UI.Metrics
import Flaw.UI.Panel
import Flaw.UI.VisualElement

-- | File dialog service.
-- Preferable to be shared between dialog invocations.
data FileDialogService = FileDialogService
	{ fileDialogServiceMetrics :: !Metrics
	, fileDialogServiceParentPanel :: !Panel
	, fileDialogServiceFlow :: !Flow
	, fileDialogServicePathVar :: {-# UNPACK #-} !(TVar T.Text)
	}

newFileDialogService :: Metrics -> Panel -> Flow -> STM FileDialogService
newFileDialogService metrics parentPanel flow = do
	pathVar <- newTVar ""
	return FileDialogService
		{ fileDialogServiceMetrics = metrics
		, fileDialogServiceParentPanel = parentPanel
		, fileDialogServiceFlow = flow
		, fileDialogServicePathVar = pathVar
		}

data Entry = FileEntry
	{
	-- | Full disk path.
	  fileEntryPath :: !T.Text
	-- | Name to display.
	, fileEntryName :: !T.Text
	-- | Size in bytes.
	, fileEntrySize :: !Integer
	}

data FileDialogConfig = FileDialogConfig
	{ fileDialogConfigTitle :: !T.Text
	}

runFileDialog :: FileDialogService -> FileDialogConfig -> (Maybe T.Text -> STM ()) -> STM ()
runFileDialog FileDialogService
	{ fileDialogServiceMetrics = metrics@Metrics
		{ metricsGap = gap
		, metricsBigGap = bigGap
		, metricsFrameClient = frameClient
		, metricsButtonSize = buttonSize@(Vec2 buttonWidth buttonHeight)
		, metricsEditBoxHeight = editBoxHeight
		, metricsListBoxColumnHeaderHeight = columnHeaderHeight
		, metricsListBoxItemHeight = itemHeight
		}
	, fileDialogServiceParentPanel = parentPanel
	, fileDialogServiceFlow = flow
	, fileDialogServicePathVar = pathVar
	} FileDialogConfig
	{ fileDialogConfigTitle = title
	} handler = do
	panel <- newPanel True
	frame <- newFrame panel metrics
	setText frame title

	frameChild <- addFreeChild parentPanel frame
	setSelfFreeChild frame parentPanel frameChild True

	dirPathLabel <- newTextLabel
	dirPathLabelVE <- newVisualElement dirPathLabel
	dirPathLabelVEChild <- addFreeChild panel dirPathLabelVE

	nameColumn <- newListBoxTextColumnDesc "name" fileEntryName fileEntryName
	sizeColumn <- newListBoxTextColumnDesc "size" fileEntrySize (T.pack . showSize . fileEntrySize)
	listBox <- newListBox metrics [nameColumn, sizeColumn]
	reorderListBox listBox fileEntryName
	listBoxChild <- addFreeChild panel listBox

	okButton <- newLabeledButton "ok"
	layoutElement okButton buttonSize
	okButtonChild <- addFreeChild panel okButton
	cancelButton <- newLabeledButton "cancel"
	layoutElement cancelButton buttonSize
	cancelButtonChild <- addFreeChild panel cancelButton

	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel dirPathLabelVEChild $ Vec2 bigGap bigGap
		layoutElement dirPathLabelVE $ Vec2 (sx - bigGap * 2) editBoxHeight
		placeFreeChild panel listBoxChild $ Vec2 bigGap (bigGap + editBoxHeight + gap)
		layoutElement listBox $ Vec2 (sx - bigGap * 2) (sy - bigGap * 2 - gap * 2 - editBoxHeight - buttonHeight)
		placeFreeChild panel okButtonChild $ Vec2 (sx - bigGap - buttonWidth * 2 - gap) (sy - bigGap - buttonHeight)
		placeFreeChild panel cancelButtonChild $ Vec2 (sx - bigGap - buttonWidth) (sy - bigGap - buttonHeight)

	layoutElement frame $ xy__ frameClient + zw__ frameClient
		+ Vec2 ((bigGap * 2 + buttonWidth * 2 + gap) * 2) (bigGap * 2 + columnHeaderHeight + itemHeight * 10 + buttonHeight + gap)

	let openDirectory unnormalizedPath = do
		path <- canonicalizePath $ T.unpack unnormalizedPath
		directoryContents <- filter (/= ".") <$> getDirectoryContents path
		entries <- forM directoryContents $ \entry -> do
			let entryPath = path </> entry
			isDir <- doesDirectoryExist entryPath
			size <- if isDir then return (-1) else handle (\SomeException {} -> return (-1)) $ IO.withFile entryPath IO.ReadMode IO.hFileSize
			return FileEntry
				{ fileEntryPath = T.pack entryPath
				, fileEntryName = T.pack entry
				, fileEntrySize = size
				}
		atomically $ do
			setText dirPathLabel $ T.pack path
			clearListBox listBox
			forM_ entries $ void . addListBoxItem listBox

	setClickHandler okButton $ do
		entries <- getListBoxSelectedValues listBox
		case entries of
			[e] -> do
				let FileEntry
					{ fileEntryPath = path
					} = e
				asyncRunInFlow flow $ do
					isDir <- doesDirectoryExist $ T.unpack path
					if isDir then openDirectory path
					else atomically $ do
						removeFreeChild parentPanel frameChild
						handler $ Just path
			_ -> return ()
	setClickHandler cancelButton $ do
		removeFreeChild parentPanel frameChild
		handler Nothing

	setDefaultElement panel okButton
	setButtonDefault okButton
	setCancelElement panel cancelButton
	setButtonCancel cancelButton

	asyncRunInFlow flow . openDirectory =<< readTVar pathVar

	focusFreeChild parentPanel frameChild

showSize :: Integer -> String
showSize size
	| size < 0                  = ""
	| size < 1024               = shows size " b"
	| size < 1024 * 1024        = showFFloat (Just 1) (fromIntegral size / 1024 :: Float) " Kb"
	| size < 1024 * 1024 * 1024 = showFFloat (Just 1) (fromIntegral size / (1024 * 1024) :: Float) " Mb"
	| otherwise                 = showFFloat (Just 1) (fromIntegral size / (1024 * 1024 * 1024) :: Float) " Gb"
