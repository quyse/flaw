{-|
Module: Flaw.Resource
Description: Type synonyms for some resource things, to not depend on ResourceT.
License: MIT
-}

{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flaw.Resource
	( ReleaseKey
	, ResourceIO
	, allocate
	, release
	, registerRelease
	, runResourceIO
	) where

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Resource as R

type ReleaseKey = R.ReleaseKey

type ResourceIO m = (R.MonadResource m, R.MonadBaseControl IO m)

allocate :: R.MonadResource m => IO a -> (a -> IO ()) -> m (ReleaseKey, a)
allocate = R.allocate

release :: MonadIO m => ReleaseKey -> m ()
release = R.release

registerRelease :: R.MonadResource m => IO () -> m ReleaseKey
registerRelease = R.register

runResourceIO :: R.MonadBaseControl IO m => R.ResourceT m a -> m a
runResourceIO = R.runResourceT
