{-|
Module: Flaw.Oil.Repo
Description: General repo functions.
License: MIT
-}

{-# LANGUAGE DeriveGeneric #-}

module Flaw.Oil.Repo
  ( RepoVersion(..)
  , serverRepoVersion
  , clientRepoVersion
  , Manifest(..)
  , protocolVersion
  , defaultManifest
  , openRepoDb
  , Revision
  , UserId
  , Push(..)
  , Pull(..)
  , checkPushLimits
  , SyncError(..)
  , Repo(..)
  , repoDbCheckIntegrity
  , repoDbVacuum
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Foldable
import Data.Int
import qualified Data.Serialize as S
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Generics

import Flaw.Book
import Flaw.Data.Sqlite
import Flaw.Exception

newtype RepoVersion = RepoVersion CInt

-- | "application_id" in SQLite db of server repo.
-- Convention: oil<letter starting from A> in little-endian.
serverRepoVersion :: RepoVersion
serverRepoVersion = RepoVersion 0x416c696f -- "oilA"

-- | "application_id" in SQLite db of client repo.
-- Convention: oil<letter starting from a> in little-endian.
clientRepoVersion :: RepoVersion
clientRepoVersion = RepoVersion 0x616c696f -- "oila"

-- | Manifest with most limitations.
data Manifest = Manifest
  { manifestProtocolVersion :: !Int32
  , manifestMaxKeySize :: !Int
  , manifestMaxValueSize :: !Int
  , manifestMaxPushItemsCount :: !Int
  , manifestMaxPushValuesTotalSize :: !Int
  , manifestMaxPullItemsCount :: !Int
  , manifestMaxPullValuesTotalSize :: !Int
  } deriving Generic

instance S.Serialize Manifest

-- | Current protocol version.
-- Convention: oil<digit starting from 0> in little-endian.
protocolVersion :: Int32
protocolVersion = 0x306c696f -- "oil0"

defaultManifest :: Manifest
defaultManifest = Manifest
  { manifestProtocolVersion = protocolVersion
  , manifestMaxKeySize = 128
  , manifestMaxValueSize = 1024 * 1024
  , manifestMaxPushItemsCount = 1024
  , manifestMaxPushValuesTotalSize = 1024 * 1024 * 2
  , manifestMaxPullItemsCount = 1024
  , manifestMaxPullValuesTotalSize = 1024 * 1024 * 2
  }

openRepoDb :: T.Text -> RepoVersion -> IO (SqliteDb, IO ())
openRepoDb fileName (RepoVersion version) = withSpecialBook $ \bk -> do
  -- open db
  db <- book bk $ sqliteDb fileName

  -- enable exclusive locking mode
  sqliteExec db $ T.pack "PRAGMA locking_mode = EXCLUSIVE"
  -- enable WAL journal mode
  sqliteExec db $ T.pack "PRAGMA journal_mode = WAL"

  -- check version
  withBook $ \tempBk -> do
    stmt <- book tempBk $ sqliteStmt db $ T.pack "PRAGMA application_id"
    sqliteQuery stmt $ \query -> do
      r <- sqliteStep query
      unless r $ throwIO $ DescribeFirstException "failed to get application_id"
      currentAppVersion <- sqliteColumn query 0
      -- if version is not set yet
      if currentAppVersion == 0 then
        -- set it
        sqliteExec db $ T.pack $ "PRAGMA application_id = " ++ show version
      -- else check that version is correct
      else when (currentAppVersion /= version) $ throwIO $ DescribeFirstException "wrong application_id"

  return db

-- | Type for revisions.
-- Start revision is 1. 0 means no revisions.
type Revision = Int64

-- | Type for user ID.
type UserId = Int64

-- | Data sent by client to server.
data Push = Push
  {
  -- | Current revision of the client.
    pushClientRevision :: !Revision
  -- | Upper bound on revisions server may send to client.
  -- Used to prevent sending revisions client already knows about.
  , pushClientUpperRevision :: !Revision
  -- | Pushed (key, value) pairs.
  , pushItems :: [(B.ByteString, B.ByteString)]
  } deriving Generic

instance S.Serialize Push

-- | Data sent by server to client.
data Pull = Pull
  {
  -- | Total number of revisions client needs to pull in order to catch up with server, counting from pushClientRevision
    pullLag :: !Int64
  -- | Server revision before pushing items.
  , pullPrePushRevision :: !Revision
  -- | Server revision after pushing items (should be equal to pre-push revision + number of items pushed).
  , pullPostPushRevision :: !Revision
  -- | Pairs (key, value) to pull.
  , pullItems :: [(Revision, B.ByteString, B.ByteString)]
  -- | New client revision after whole operation.
  , pullNewClientRevision :: !Revision
  } deriving Generic

instance S.Serialize Pull

-- | Check push limits.
checkPushLimits :: Manifest -> Push -> Maybe SyncError
checkPushLimits Manifest
  { manifestMaxKeySize = maxKeySize
  , manifestMaxValueSize = maxValueSize
  , manifestMaxPushItemsCount = maxPushItemsCount
  , manifestMaxPushValuesTotalSize = maxPushValuesTotalSize
  } Push
  { pushItems = items
  } = maybeError where
  maybeError =
    if length items > maxPushItemsCount then Just SyncTooManyItemsError
    else case foldrM f 0 items of
      Right _valuesTotalSize -> Nothing
      Left err -> Just err
  f (key, value) valuesTotalSize
    | B.length key > maxKeySize = Left SyncTooBigKeyError
    | B.length value > maxValueSize = Left SyncTooBigValueError
    | otherwise = let newValuesTotalSize = valuesTotalSize + B.length value in
      if newValuesTotalSize > maxPushValuesTotalSize then Left SyncTooBigPushValuesTotalSize
      else Right newValuesTotalSize

-- | Errors while syncing (reported to client).
data SyncError
  = SyncTooManyItemsError
  | SyncTooBigKeyError
  | SyncTooBigValueError
  | SyncTooBigPushValuesTotalSize
  | SyncFatalError
  deriving Generic

class Repo r where
  -- | Get SQLite DB.
  repoDb :: r -> SqliteDb

-- | Check integrity of DB.
repoDbCheckIntegrity :: SqliteDb -> IO (Bool, T.Text)
repoDbCheckIntegrity db = withBook $ \bk -> do
  stmt <- book bk $ sqliteStmt db $ T.pack "PRAGMA integrity_check"
  lns <- sqliteQuery stmt $ \query -> do
    let
      step = do
        r <- sqliteStep query
        if r then do
          line <- sqliteColumn query 0
          restLines <- step
          return $ line : restLines
        else return []
      in step
  return (lns == [T.pack "ok"], T.unlines lns)

-- | Optimize DB.
repoDbVacuum :: SqliteDb -> IO ()
repoDbVacuum db = sqliteExec db $ T.pack "VACUUM"
