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

import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.EditBox
import Flaw.UI.Metrics
import Flaw.UI.Panel

data FileElement = FileElement
	{ fileElementPanel :: !Panel
	, fileElementEditBox :: !EditBox
	, fileElementButton :: !Button
	}

newFileElement :: Metrics -> STM FileElement
newFileElement Metrics
	{ metricsButtonSize = Vec2 buttonWidth _buttonHeight
	} = do
	panel <- newPanel False
	editBox <- newEditBox
	editBoxChild <- addFreeChild panel editBox
	button <- newLabeledButton "load"
	buttonChild <- addFreeChild panel button
	setDefaultElement panel button
	setButtonDefault button
	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		placeFreeChild panel editBoxChild $ Vec2 0 0
		layoutElement editBox $ Vec2 (sx - buttonWidth) sy
		placeFreeChild panel buttonChild $ Vec2 (sx - buttonWidth) 0
		layoutElement button $ Vec2 buttonWidth sy
	return FileElement
		{ fileElementPanel = panel
		, fileElementEditBox = editBox
		, fileElementButton = button
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
	setClickHandler = setClickHandler . fileElementButton

instance HasPreferredSize FileElement where
	preferredSize Metrics
		{ metricsMainWidth = width
		, metricsEditBoxHeight = height
		} _ = Vec2 width height
