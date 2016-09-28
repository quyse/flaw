{-|
Module: Flaw.Editor.Project
Description: Project settings.
License: MIT
-}

{-# LANGUAGE DeriveGeneric #-}

module Flaw.Editor.Project
	( Project(..)
	) where

import qualified Data.Aeson.Types as Y
import qualified Data.Text as T
import GHC.Generics(Generic)

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
