{-|
Module: Flaw.UI.Menu
Description: Menu.
License: MIT
-}

module Flaw.UI.Menu
	( Menu(..)
	, MenuItem(..)
	, newPopupMenu
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T

import Flaw.Math
import Flaw.UI
import Flaw.UI.Button
import Flaw.UI.Metrics
import Flaw.UI.Popup

newtype Menu = Menu [MenuItem]

data MenuItem
	= MenuItemCommand !T.Text !(STM ())
	-- MenuItemSubMenu !T.Text !Menu

newPopupMenu :: PopupService -> Position -> Menu -> STM ()
newPopupMenu popupService@PopupService
	{ popupServiceMetrics = Metrics
		{ metricsButtonSize = buttonSize@(Vec2 buttonWidth buttonHeight)
		}
	} (Vec2 px py) (Menu items) = do
	Popup
		{ popupPanel = panel
		, popupClose = close
		} <- newPopup popupService (Vec4 px py (px + buttonWidth) (py + buttonHeight * length items))
	forM_ (zip [0..] items) $ \(i, MenuItemCommand text handler) -> do
		button <- newLabeledButton text
		buttonChild <- addFreeChild panel button
		layoutElement button buttonSize
		placeFreeChild panel buttonChild $ Vec2 0 (i * buttonHeight)
		setActionHandler button $ do
			close
			handler
