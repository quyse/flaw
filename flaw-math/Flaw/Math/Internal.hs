{-|
Module: Flaw.Math.Internal
Description: Template-Haskell generating math types and functions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, Trustworthy #-}

module Flaw.Math.Internal
	( maxVecDimension
	, vecComponents
	, matDimensions
	, mathTypeNamesWithPrefix
	, mathQuaternionTypeNamesWithPrefix
	, swizzleVariantFilter
	, genSwizzleVariants
	, addInlines
	, mathTypeVectorizedDecls
	) where

import Control.Monad
import Data.Char
import Data.Int
import Data.Word
import GHC.Generics(Generic)
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
mathTypeNamesWithPrefix = [(''Float, "Float"), (''Double, "Double"), (''Int32, "Int32_"), (''Word32, "Word32_"), (''Int, "Int"), (''Int8, "Int8_"), (''Word8, "Word8_")]

-- | Meaningful types for quaternions.
mathQuaternionTypeNamesWithPrefix :: [(Name, String)]
mathQuaternionTypeNamesWithPrefix = [(''Float, "Float"), (''Double, "Double")]

swizzleVariantFilter :: String -> String -> Bool
swizzleVariantFilter components variant = all (`elem` components) variant && elem (last components) variant

-- | Return list of swizzle variants for a given length.
genSwizzleVariants :: Int -> [String]
genSwizzleVariants 0 = [""]
genSwizzleVariants len = [c : v | c <- vecComponents, v <- genSwizzleVariants $ len - 1]

-- | Add inline pragmas for all functions.
addInlines :: [DecQ] -> Q [DecQ]
addInlines qdecs = fmap concat $ forM qdecs $ \qdec -> do
	dec <- qdec
	return $ case dec of
		FunD funName _clauses -> [return dec, pragInlD funName Inline FunLike AllPhases]
		_ -> [return dec]

-- | Declarations for single vectorized math type.
mathTypeVectorizedDecls :: Name -> String -> Q [Dec]
mathTypeVectorizedDecls mathTypeName mathTypePrefix = do

	let elemType = conT mathTypeName

	-- Vectorized instance
	vectorizedInstance <- do

		-- vector things
		vecDecs <- fmap concat $ forM [1..maxVecDimension] $ \dim -> do
			let dimStr = [intToDigit dim]
			let dataName = mkName $ "Vec" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM (take dim vecComponents) $ newName . return
			return
				[ dataInstD (sequence []) dataName [elemType] Nothing [normalC conName (replicate dim $ return (Bang SourceUnpack SourceStrict, ConT mathTypeName))] (sequence [ [t| Generic |] ])
				, funD (mkName $ "vec" ++ dimStr) [clause (map varP components) (normalB $ foldl appE (conE conName) $ map varE components) []]
				, funD (mkName $ "unvec" ++ dimStr) [clause [conP conName $ map varP components] (normalB $ tupE $ map varE components) []]
				]

		-- matrix things
		matDecs <- fmap concat $ forM matDimensions $ \(dimN, dimM) -> do
			let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
			let dataName = mkName $ "Mat" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM [(i, j) | i <- [1..dimN], j <- [1..dimM]] $ \(i, j) -> newName ['m', intToDigit i, intToDigit j]
			return
				[ dataInstD (sequence []) dataName [elemType] Nothing [normalC conName (replicate (dimN * dimM) $ return (Bang SourceUnpack SourceStrict, ConT mathTypeName))] (sequence [ [t| Generic |] ])
				, funD (mkName $ "mat" ++ dimStr) [clause (map varP components) (normalB $ foldl appE (conE conName) $ map varE components) []]
				, funD (mkName $ "unmat" ++ dimStr) [clause [conP conName $ map varP components] (normalB $ tupE $ map varE components) []]
				]

		instanceD (sequence []) (appT (conT $ mkName "Vectorized") elemType) =<< addInlines (vecDecs ++ matDecs)

	-- type synonyms for vectors
	vecSynonyms <- forM [1..maxVecDimension] $ \dim -> do
		let dimStr = [intToDigit dim]
		tySynD (mkName $ mathTypePrefix ++ dimStr) [] [t| $(conT $ mkName $ "Vec" ++ dimStr) $(conT mathTypeName) |]

	-- type synonyms for matrices
	matSynonyms <- forM matDimensions $ \(dimN, dimM) -> do
		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		tySynD (mkName $ mathTypePrefix ++ dimStr) [] [t| $(conT $ mkName $ "Mat" ++ dimStr) $(conT mathTypeName) |]

	return $ vectorizedInstance : vecSynonyms ++ matSynonyms
