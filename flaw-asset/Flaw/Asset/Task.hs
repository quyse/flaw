{-|
Module: Flaw.Asset.Task
Description: Asset processing tasks.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies #-}

module Flaw.Asset.Task
	( Task(..)
	, WrappedTaskInput(..)
	, TaskTag(..)
	, putTaskInput
	, getTaskInput
	) where

import Control.Monad
import qualified Data.Serialize as S

class Task t where
	data TaskInput t :: *
	data TaskOutput t :: *
	runTask :: TaskInput t -> IO (TaskOutput t)
	taskTag :: p t -> String

data WrappedTaskInput where
	WrappedTaskInput :: (Task t, S.Serialize (TaskInput t), S.Serialize (TaskOutput t)) => TaskInput t -> WrappedTaskInput

data TaskTag where
	TaskTag :: (Task t, S.Serialize (TaskInput t), S.Serialize (TaskOutput t)) => p t -> TaskTag

putTaskInput :: (Task t, S.Serialize (TaskInput t)) => TaskInput t -> S.Put
putTaskInput taskInput = do
	S.put $ taskTag taskInput
	S.put taskInput

getTaskInput :: [TaskTag] -> S.Get (Maybe WrappedTaskInput)
getTaskInput taskTags = do
	t <- S.get
	let
		g :: S.Serialize (TaskInput t) => p t -> S.Get (TaskInput t)
		g _ = S.get
		check ((TaskTag pt) : rest) = do
			if t == taskTag pt then liftM (Just . WrappedTaskInput) $ g pt
			else check rest
		check [] = return Nothing
	check taskTags
