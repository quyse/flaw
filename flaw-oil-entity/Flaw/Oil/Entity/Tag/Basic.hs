{-|
Module: Flaw.Oil.Entity.Tag.Basic
Description: Basic entity tags.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Flaw.Oil.Entity.Tag.Basic
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

import Flaw.Oil.Entity
import Flaw.Oil.Entity.Basic()
import Flaw.Oil.Entity.Tag

-- | Name tag.
newtype NameEntityTag = NameEntityTag T.Text deriving (Generic, Default, S.Serialize)

instance Entity NameEntityTag where
	getEntityTypeId _ = $(hashTextToEntityTypeId "NameEntityTag")
instance BasicEntity NameEntityTag

nameEntityTagId :: EntityTagId
nameEntityTagId = $(hashTextToEntityTagId "NameEntityTag")

nameEntityTagEntityId :: EntityId -> EntityId
nameEntityTagEntityId = entityTagEntityId nameEntityTagId

-- | Description tag.
newtype DescriptionEntityTag = DescriptionEntityTag T.Text deriving (Generic, Default, S.Serialize)

instance Entity DescriptionEntityTag where
	getEntityTypeId _ = $(hashTextToEntityTypeId "DescriptionEntityTag")
instance BasicEntity DescriptionEntityTag

descriptionEntityTagId :: EntityTagId
descriptionEntityTagId = $(hashTextToEntityTagId "DescriptionEntityTag")

descriptionEntityTagEntityId :: EntityId -> EntityId
descriptionEntityTagEntityId = entityTagEntityId descriptionEntityTagId
