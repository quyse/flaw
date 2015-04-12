{-|
Module: Flaw.Asset.Geometry
Description: Some geometry helpers.
License: MIT
-}

module Flaw.Asset.Geometry
	( indexVertices
	) where

import Data.List
import qualified Data.Set as Set

-- | Index vertices.
indexVertices :: (Eq v, Ord v) => [v] -> ([v], [Int])
indexVertices vertices = (resultVertices, resultIndices) where
	resultVertices = map head $ group $ sort vertices
	verticesSet = Set.fromAscList resultVertices
	resultIndices = map (\v -> Set.findIndex v verticesSet) vertices
