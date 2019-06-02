{-|
Module: Flaw.Editor.Entity.Sync
Description: Entity synchronization.
License: MIT
-}

module Flaw.Editor.Entity.Sync
  ( newSyncedEntityManager
  ) where

import Control.Concurrent.STM
import Control.Monad

import Flaw.Book
import Flaw.Editor.Entity
import Flaw.Flow
import Flaw.Oil.ClientRepo
import Flaw.Oil.RemoteRepo
import Flaw.Oil.Repo

-- | Create entity manager synchronized with given remote repo.
-- After creation entity type deserializers must be registered before creation
-- of any cached entity.
newSyncedEntityManager :: ClientRepo -> HttpRemoteRepo -> Manifest -> (ClientRepoPullInfo -> STM ()) -> IO (EntityManager, IO ())
newSyncedEntityManager clientRepo remoteRepo manifest callback = withSpecialBook $ \bk -> do
  -- status variables
  syncingVar <- newTVarIO False
  needSyncVar <- newTVarIO True -- force initial sync

  clientFlow <- book bk newFlow
  entityManager <- newEntityManager clientFlow clientRepo $ atomically $ writeTVar needSyncVar True

  -- watch flow
  book bk $ forkFlow $ forever $ do
    -- if sync is requested or in progress, no need in watching
    atomically $ do
      syncing <- readTVar syncingVar
      needSync <- readTVar needSyncVar
      when (syncing || needSync) retry
    -- get client revision
    clientRevision <- runInFlow clientFlow $ clientRepoRevision clientRepo
    -- watch for changes
    serverRevision <- watchHttpRemoteRepo remoteRepo clientRevision
    -- if server revision is greater, perform sync
    when (clientRevision < serverRevision) $ atomically $ writeTVar needSyncVar True

  -- sync flow
  book bk $ forkFlow $ forever $ do
    -- sync when needed
    atomically $ do
      needSync <- readTVar needSyncVar
      unless needSync retry
      writeTVar needSyncVar False
      writeTVar syncingVar True

    -- perform client push
    (push, pushState) <- runInFlow clientFlow $ pushClientRepo clientRepo manifest
    -- perform remote sync
    pull <- syncHttpRemoteRepo remoteRepo push
    -- perform client pull
    pullInfo@ClientRepoPullInfo
      { clientRepoPullLag = lag
      } <- runInFlow clientFlow $ do
      pullInfo@ClientRepoPullInfo
        { clientRepoPullChanges = changes
        } <- pullClientRepo clientRepo pull pushState
      -- notify entity manager
      unsafePullEntityManager entityManager changes
      return pullInfo

    -- run callback
    atomically $ callback pullInfo

    -- finish
    atomically $ do
      -- set needSync flag if push or pull was non-empty
      when (not (isClientRepoPushEmpty pushState) || lag > 0) $ writeTVar needSyncVar True
      writeTVar syncingVar False

  return entityManager
