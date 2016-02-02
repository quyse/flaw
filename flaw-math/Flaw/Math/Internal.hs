{-|
Module: Flaw.Math.Internal
Description: Template-Haskell generating math types and functions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Math.Internal
	( maxVecDimension
	, vecComponents
	, matDimensions
	, mathTypeNamesWithPrefix
	, mathQuaternionTypeNamesWithPrefix
	, swizzleVariantFilter
	, genSwizzleVariants
	, addInlines
	) where

import Control.Monad
import Data.Int
import Data.Word
import Language.Haskell.TH

maxVecDimension :: Int
maxVecDimension = 4

vecComponents :: String
vecComponents = "xyzw"

-- | Whitelisted matrix dimensions.
matDimensions :: [(Int, Int)]
matDimensions = [(3, 3), (3, 4), (4, 4)]

-- | Meaningful types for math.
mathTypeNamesWithPrefix :: [(Name, String)]
mathTypeNamesWithPrefix = [(''Float, "Float"), (''Double, "Double"), (''Int32, "Int32_"), (''Word32, "Word32_"), (''Int, "Int"), (''Word8, "Word8_")]

-- | Meaningful types for quaternions.
mathQuaternionTypeNamesWithPrefix :: [(Name, String)]
mathQuaternionTypeNamesWithPrefix = [(''Float, "Float"), (''Double, "Double")]

swizzleVariantFilter :: String -> String -> Bool
swizzleVariantFilter components variant = all (\c -> elem c components) variant && elem (last components) variant

-- | Return list of swizzle variants for a given length.
genSwizzleVariants :: Int -> [String]
genSwizzleVariants 0 = [""]
genSwizzleVariants len = [c : v | c <- vecComponents, v <- genSwizzleVariants $ len - 1]

-- | Add inline pragmas for all functions.
addInlines :: [DecQ] -> Q [DecQ]
addInlines qdecs = liftM concat $ forM qdecs $ \qdec -> do
	dec <- qdec
	return $ case dec of
		FunD funName _clauses -> [return dec, pragInlD funName Inline FunLike AllPhases]
		_ -> [return dec]
