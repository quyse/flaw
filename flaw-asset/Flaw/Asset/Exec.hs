{-|
Module: Flaw.Asset.Exec
Description: Out-of-process execution of tasks.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Asset.Exec
	( execTask
	) where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S
import GHC.IO.Handle
import qualified System.Process as P

import Flaw.Asset.Task

execTask :: (Task t, S.Serialize (TaskInput t), S.Serialize (TaskOutput t)) => TaskInput t -> IO (TaskOutput t)
execTask taskInput = do
	(Just hStdin, Just hStdout, Nothing, _hProcess) <- P.createProcess P.CreateProcess
		{ P.cmdspec = P.RawCommand "flaw-asset-worker" []
		, P.cwd = Nothing
		, P.env = Nothing
		, P.std_in = P.CreatePipe
		, P.std_out = P.CreatePipe
		, P.std_err = P.Inherit
		, P.close_fds = False
		, P.create_group = False
		, P.delegate_ctlc = False
		}
	hSetBinaryMode hStdin True
	hSetBinaryMode hStdout True
	BL.hPut hStdin $ S.runPutLazy $ putTaskInput taskInput
	hClose hStdin
	let get = do
		success <- S.get
		if success then liftM Just S.get else return Nothing
	eitherMaybeTaskOutput <- liftM (S.runGetLazy get) $ BL.hGetContents hStdout
	case eitherMaybeTaskOutput of
		Right (Just taskOutput) -> return taskOutput
		_ -> fail "failed to exec task"
