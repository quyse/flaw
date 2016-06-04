{-|
Module: Flaw.UI.Editor.Elements
Description: UI elements specific to editor
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Flaw.UI.Editor.Elements
	( FileElement(..)
	, newFileElement
	) where

import Control.Concurrent.STM
import Control.Monad

import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.EditBox
import Flaw.UI.Editor.FileDialog
import Flaw.UI.Metrics
import Flaw.UI.Panel

data FileElement = FileElement
	{ fileElementPanel :: !Panel
	, fileElementEditBox :: !EditBox
	, fileElementClickHandlerVar :: {-# UNPACK #-} !(TVar (STM ()))
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

	clickHandlerVar <- newTVar $ return ()

	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel editBoxChild $ Vec2 0 0
		layoutElement editBox $ Vec2 (sx - buttonWidth) sy
		placeFreeChild panel browseButtonChild $ Vec2 (sx - buttonWidth) 0
		layoutElement browseButton $ Vec2 sy sy
		placeFreeChild panel loadButtonChild $ Vec2 (sx - buttonWidth + sy) 0
		layoutElement loadButton $ Vec2 (buttonWidth - sy) sy

	setClickHandler browseButton $ runFileDialog service FileDialogConfig
		{ fileDialogConfigTitle = "browse file"
		} $ \maybeFileName -> case maybeFileName of
		Just fileName -> do
			setText editBox fileName
			join $ readTVar clickHandlerVar
		Nothing -> return ()
	setClickHandler loadButton $ join $ readTVar clickHandlerVar

	return FileElement
		{ fileElementPanel = panel
		, fileElementEditBox = editBox
		, fileElementClickHandlerVar = clickHandlerVar
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

instance HasClickHandler FileElement where
	setClickHandler = writeTVar . fileElementClickHandlerVar

instance HasPreferredSize FileElement where
	preferredSize Metrics
		{ metricsMainWidth = width
		, metricsEditBoxHeight = height
		} _ = Vec2 width height
