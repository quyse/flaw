{-|
Module: Flaw.Editor.Entity.Tag
Description: Entity tags.
License: MIT

Entity tags are special types of entities which could be only attached to other entities.
Each entity may only have zero or one attached tag of any single tag type.
Tags are entities too, but they have unusual entity id: it's concatenation of tag id
and attached entity's id.
-}

{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Editor.Entity.Tag
  ( EntityTagId(..)
  , pattern ENTITY_TAG_ID_SIZE
  , entityTagEntityId
  , hashTextToEntityTagId
  ) where

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import Language.Haskell.TH

import Flaw.Editor.Entity
import Flaw.Editor.Entity.Internal

-- | Entity tag id, used for prefixing entity id of tag entities.
newtype EntityTagId = EntityTagId BS.ShortByteString

pattern ENTITY_TAG_ID_SIZE = 20

-- | Combine entity tag id and entity id to get entity tag's entity id.
entityTagEntityId :: EntityTagId -> EntityId -> EntityId
entityTagEntityId (EntityTagId entityTagIdBytes) (EntityId entityIdBytes) =
  EntityId $ entityTagIdBytes <> entityIdBytes

-- | Handy function to generate compile-time entity tag id out of text.
hashTextToEntityTagId :: T.Text -> Q Exp
hashTextToEntityTagId = hashTextDecl "entityTagIdHash_" [t| EntityTagId |] $ \e -> [| EntityTagId (BS.toShort $e) |]
