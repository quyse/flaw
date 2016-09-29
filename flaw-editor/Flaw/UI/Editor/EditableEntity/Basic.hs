{-|
Module: Flaw.UI.Editor.EditableEntity.Basic
Description: Basic entities' layout.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flaw.UI.Editor.EditableEntity.Basic
	(
	) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Text as T

import Flaw.Oil.Entity.Basic()
import Flaw.UI
import Flaw.UI.EditBox
import Flaw.UI.Editor.EditableEntity
import Flaw.UI.Layout
import Flaw.UI.Metrics
import Flaw.UI.Panel

instance EditableEntity T.Text where
	editableEntityTypeName _ = "Text"
	editableEntityConstructorName _ = "Text"
	editableEntityLayout setter = ReaderT $ \EditableLayoutState {} -> do
		currentValueVar <- lift $ newTVar T.empty
		panel <- lift $ newPanel False
		editBox <- lift newEditBox
		_editBoxChild <- lift $ addFreeChild panel editBox
		lift $ setLayoutHandler panel $ layoutElement editBox
		FlowLayoutState
			{ flsMetrics = metrics
			} <- get
		elementWithSizeInFlowLayout panel (preferredSize metrics editBox)
		lift $ setCommitHandler panel $ \commitReason -> do
			if commitReason == CommitAccept || commitReason == CommitLostFocus then do
				value <- getText editBox
				currentValue <- readTVar currentValueVar
				when (value /= currentValue) $ do
					writeTVar currentValueVar value
					setter $ const value
			else setText editBox =<< readTVar currentValueVar
			return True
		return $ \newValue _change -> do
			-- check that it's not equal to current value
			currentValue <- readTVar currentValueVar
			when (newValue /= currentValue) $ do
				-- in any case remember new current value
				writeTVar currentValueVar newValue
				-- change text in edit box only if it's not changed
				value <- getText editBox
				when (value == currentValue) $ setText editBox newValue
