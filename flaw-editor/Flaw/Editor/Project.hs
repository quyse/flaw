{-|
Module: Flaw.Editor.Project
Description: Project settings.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Flaw.Editor.Project
  ( Project(..)
  , projectRootEntityPtr
  ) where

import qualified Data.Aeson as Y
import qualified Data.Text as T
import GHC.Generics(Generic)

import Flaw.Editor.Entity
import Flaw.Editor.Entity.Basic

data Project = Project
  { projectRemoteRepoUrl :: !T.Text
  , projectLocalRepoPath :: !T.Text
  , projectProcessingCachePath :: !T.Text
  , projectProcessingCacheMaxAge :: {-# UNPACK #-} !Int
  } deriving Generic

instance Y.FromJSON Project where
  parseJSON = Y.genericParseJSON Y.defaultOptions
    { Y.fieldLabelModifier = drop 7 -- skip "project"
    }

-- | Predefined "project root" entity.
projectRootEntityPtr :: EntityPtr Folder
projectRootEntityPtr = EntityPtr $(hashTextToEntityId "ProjectRoot")
