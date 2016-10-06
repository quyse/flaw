{-|
Module: Flaw.Editor.UI.Elements
Description: UI elements specific to editor
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.Editor.UI.Elements
	( FileElement(..)
	, newFileElement
	) where

import Control.Concurrent.STM
import Control.Monad

import Flaw.Editor.UI.FileDialog
import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.EditBox
import Flaw.UI.Metrics
import Flaw.UI.Panel

data FileElement = FileElement
	{ fileElementPanel :: !Panel
	, fileElementEditBox :: !EditBox
	, fileElementActionHandlerVar :: {-# UNPACK #-} !(TVar (STM ()))
	}

newFileElement :: FileDialogService -> STM FileElement
newFileElement service@FileDialogService
	{ fileDialogServiceMetrics = Metrics
		{ metricsButtonSize = Vec2 buttonWidth _buttonHeight
		}
	} = do
	panel <- newPanel False
	editBox <- newEditBox
	editBoxChild <- addFreeChild panel editBox
	browseButton <- newLabeledButton "..."
	browseButtonChild <- addFreeChild panel browseButton
	loadButton <- newLabeledButton "load"
	loadButtonChild <- addFreeChild panel loadButton
	setDefaultElement panel loadButton
	setButtonDefault loadButton

	actionHandlerVar <- newTVar $ return ()

	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel editBoxChild $ Vec2 0 0
		layoutElement editBox $ Vec2 (sx - buttonWidth) sy
		placeFreeChild panel browseButtonChild $ Vec2 (sx - buttonWidth) 0
		layoutElement browseButton $ Vec2 sy sy
		placeFreeChild panel loadButtonChild $ Vec2 (sx - buttonWidth + sy) 0
		layoutElement loadButton $ Vec2 (buttonWidth - sy) sy

	setActionHandler browseButton $ runFileDialog service FileDialogConfig
		{ fileDialogConfigTitle = "choose file"
		} $ \maybeFileName -> case maybeFileName of
		Just fileName -> do
			setText editBox fileName
			join $ readTVar actionHandlerVar
		Nothing -> return ()
	setActionHandler loadButton $ join $ readTVar actionHandlerVar

	return FileElement
		{ fileElementPanel = panel
		, fileElementEditBox = editBox
		, fileElementActionHandlerVar = actionHandlerVar
		}

instance Element FileElement where
	layoutElement = layoutElement . fileElementPanel
	dabElement = dabElement . fileElementPanel
	elementMouseCursor = elementMouseCursor . fileElementPanel
	renderElement = renderElement . fileElementPanel
	processInputEvent = processInputEvent . fileElementPanel
	focusElement = focusElement . fileElementPanel
	unfocusElement = unfocusElement . fileElementPanel

instance HasText FileElement where
	setText = setText . fileElementEditBox
	setTextScript = setTextScript . fileElementEditBox
	getText = getText . fileElementEditBox

instance HasActionHandler FileElement where
	setActionHandler = writeTVar . fileElementActionHandlerVar

instance HasPreferredSize FileElement where
	preferredSize Metrics
		{ metricsMainWidth = width
		, metricsEditBoxHeight = height
		} _ = Vec2 width height
