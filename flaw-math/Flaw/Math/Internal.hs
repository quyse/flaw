{-|
Module: Flaw.Math.Internal
Description: Template-Haskell generating math types and functions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Math.Internal
	( maxVecDimension
	, vecComponents
	, mathTypeNames
	, mathTypeNamesWithChar
	, swizzleVariantFilter
	, genSwizzleVariants
	) where

import Data.Int
import Data.Word
import Language.Haskell.TH

maxVecDimension :: Int
maxVecDimension = 4

vecComponents :: String
vecComponents = "xyzw"

-- | Meaningful types for math.
-- Some data structures could specialise on this types.
mathTypeNames :: [Name]
mathTypeNames = map fst mathTypeNamesWithChar
mathTypeNamesWithChar :: [(Name, Char)]
mathTypeNamesWithChar = [(''Float, 'f'), (''Double, 'd'), (''Int32, 'i'), (''Word32, 'u')]

swizzleVariantFilter :: String -> String -> Bool
swizzleVariantFilter components variant = all (\c -> elem c components) variant && elem (last components) variant

-- | Return list of swizzle variants for a given length.
genSwizzleVariants :: Int -> [String]
genSwizzleVariants 0 = [""]
genSwizzleVariants len = [c : v | c <- vecComponents, v <- genSwizzleVariants $ len - 1]
