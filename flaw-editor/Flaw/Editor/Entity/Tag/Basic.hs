{-|
Module: Flaw.Editor.Entity.Tag.Basic
Description: Basic entity tags.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Flaw.Editor.Entity.Tag.Basic
	( NameEntityTag(..)
	, nameEntityTagId
	, nameEntityTagEntityId
	, DescriptionEntityTag(..)
	, descriptionEntityTagId
	, descriptionEntityTagEntityId
	) where

import Data.Default
import qualified Data.Serialize as S
import qualified Data.Text as T
import GHC.Generics(Generic)

import Flaw.Editor.Entity
import Flaw.Editor.Entity.Basic()
import Flaw.Editor.Entity.Tag

-- | Name tag.
newtype NameEntityTag = NameEntityTag T.Text deriving (Generic, Default, S.Serialize, Show)

instance Entity NameEntityTag where
	type EntityChange NameEntityTag = NameEntityTag
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
	getEntityTypeId _ = $(hashTextToEntityTypeId "NameEntityTag")
instance BasicEntity NameEntityTag

nameEntityTagId :: EntityTagId
nameEntityTagId = $(hashTextToEntityTagId "NameEntityTag")

nameEntityTagEntityId :: EntityId -> EntityId
nameEntityTagEntityId = entityTagEntityId nameEntityTagId

-- | Description tag.
newtype DescriptionEntityTag = DescriptionEntityTag T.Text deriving (Generic, Default, S.Serialize, Show)

instance Entity DescriptionEntityTag where
	type EntityChange DescriptionEntityTag = DescriptionEntityTag
	getEntityTypeId _ = $(hashTextToEntityTypeId "DescriptionEntityTag")
	processEntityChange = processBasicEntityChange
	applyEntityChange = applyBasicEntityChange
instance BasicEntity DescriptionEntityTag

descriptionEntityTagId :: EntityTagId
descriptionEntityTagId = $(hashTextToEntityTagId "DescriptionEntityTag")

descriptionEntityTagEntityId :: EntityId -> EntityId
descriptionEntityTagEntityId = entityTagEntityId descriptionEntityTagId
