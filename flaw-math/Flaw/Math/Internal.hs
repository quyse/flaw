{-|
Module: Flaw.Math.Internal
Description: Template-Haskell generating math types and functions.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}

module Flaw.Math.Internal
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, Dot(..)
	, Cross(..)
	, Mat(..)
	, Mul(..)
	, genVecClasses
	, genVecDatas
	, genMatDatas
	, genMuls
	, genSynonyms
	) where

import Control.Monad
import Language.Haskell.TH
import Data.Char

maxVecDimension :: Int
maxVecDimension = 4

vecComponents :: String
vecComponents = "xyzw"

-- | Meaningful types for math.
-- Some data structures could specialise on this types.
mathTypeNames :: [Name]
mathTypeNames = map fst mathTypeNamesWithChar
mathTypeNamesWithChar :: [(Name, Char)]
mathTypeNamesWithChar = [(''Float, 'f'), (''Double, 'd'), (''Int, 'i')]

-- | General vector class.
class Vec v e | v -> e where
	-- | Get number of components in vector.
	vecLength :: v -> Int -- v is unused
	-- | Convert vector to list.
	vecToList :: v -> [e]

-- | Class for dot operation.
class Dot v e | v -> e where
	dot :: v -> v -> e

-- | Class for cross operation.
class Cross v where
	cross :: v -> v -> v

-- | General matrix class.
class Mat m e | m -> e where
	-- | Get matrix size.
	matSize :: m -> (Int, Int) -- m is unused

-- | Class for general multiplication.
class Mul a b c | a b -> c where
	mul :: a -> b -> c

-- | Generates classes VecX..VecW with only method to access components.
{- Example:
class Vec v e => VecX v e where
	x_ :: v -> e
-}
genVecClasses :: Q [Dec]
genVecClasses = mapM genVecClass vecComponents where
	genVecClass c = do
		let className = mkName $ "Vec" ++ [toUpper c]
		let methodName = mkName $ [c, '_']
		-- type params
		tvV <- newName "v"
		tvE <- newName "e"
		-- v -> e
		let methodType = AppT (AppT ArrowT (VarT tvV)) (VarT tvE)
		-- method signature
		let sig = SigD methodName methodType
		-- class declaration
		return $ ClassD [ClassP (mkName "Vec") [VarT tvV, VarT tvE]] className [PlainTV tvV, PlainTV tvE] [] [sig]

-- | Generate actual vector data with instances.
{- Example:

data Vec4 a = Vec4 a a a a deriving Show
instance VecX (Vec4 a) a where
	x_ (Vec4 x _ _ _) = x
-- VecY, ...

instance Vec (Vec4 a) a where
	vecLength _ = 4
	vecToList (Vec4 x y z w) = [x, y, z, w]

instance Num a => Vec4 a where


-}
genVecDatas :: Q [Dec]
genVecDatas = liftM concat $ mapM genVecData [1..maxVecDimension] where
	-- generate vec data for specified dimension
	genVecData dim = do
		-- name of data (Vec{dim})
		let dataName = mkName $ "Vec" ++ [intToDigit dim]
		-- name of type parameter
		tvA <- newName "a"
		let tyVarBinds = [PlainTV tvA]
		let dataCon = conT dataName
		-- type declaration
		let typeDec = dataD (return []) dataName tyVarBinds [normalC dataName (replicate dim (return (IsStrict, VarT tvA)))] [''Show]

		---- instance declarations

		-- string with symbols of components, like "xyz"
		let components = take dim vecComponents

		-- instances for VecX .. VecW classes
		let genVecComponentInstance component = do
			let className = mkName $ "Vec" ++ [toUpper component]
			let funName = mkName [component, '_']
			varName <- newName [component]
			let funDecl = funD funName [clause [conP dataName [if c == component then (varP varName) else wildP | c <- components]] (normalB (varE varName)) []]
			let specialiseDecl mathType = do
				pragSpecInlD funName (appT (appT arrowT (appT dataCon mathType)) mathType) Inline AllPhases
			let specialiseDecls = map (specialiseDecl . conT) mathTypeNames
			let decls = funDecl : specialiseDecls
			componentParamName <- newName "a"
			instanceD (return []) (appT (appT (conT className) (appT dataCon (varT componentParamName))) (varT componentParamName)) decls

		-- instance for Vec class
		let vecInstance = do
			-- vecLength
			vecLengthDecls <- do
				let funDecl = funD 'vecLength [clause [wildP] (normalB $ litE $ integerL $ fromIntegral dim) []]
				let specialiseDecl mathType = do
					let funType = appT (appT arrowT (appT dataCon mathType)) (conT ''Int)
					pragSpecInlD 'vecLength funType Inline AllPhases
				let specialiseDecls = map (specialiseDecl . conT) mathTypeNames
				return $ funDecl : specialiseDecls
			-- vecToList
			vecToListDecls <- do
				componentParams <- mapM (\c -> newName [c]) components
				let funDecl = funD 'vecToList [clause [conP dataName $ map varP componentParams] (normalB $ listE $ map varE componentParams) []]
				let specialiseDecl mathType = do
					let funType = appT (appT arrowT (appT dataCon mathType)) (appT listT mathType)
					pragSpecInlD 'vecToList funType Inline AllPhases
				let specialiseDecls = map (specialiseDecl . conT) mathTypeNames
				return $ funDecl : specialiseDecls
			let decls = vecLengthDecls ++ vecToListDecls
			componentParamName <- newName "a"
			instanceD (return []) (appT (appT (conT ''Vec) (appT dataCon (varT componentParamName))) (varT componentParamName)) decls

		sequence $ typeDec : vecInstance : (map genVecComponentInstance components)

-- | Generate matrix datatypes.
genMatDatas :: Q [Dec]
genMatDatas = liftM concat $ mapM genMatData [(i, j) | i <- dimensions, j <- dimensions] where
	dimensions = [1..maxVecDimension]
	genMatData (n, m) = do
		-- name of data (Mat{n}x{m})
		let dataName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		-- name of type parameter
		tvA <- newName "a"
		let tyVarBinds = [PlainTV tvA]
		let dataCon = ConT dataName
		-- type declaration
		let typeDec = do
			return $ DataD [] dataName tyVarBinds [NormalC dataName (replicate (n * m) (IsStrict, VarT tvA))] [''Show]

		-- instance for Mat class
		let genMatInstance = do
			let className = mkName "Mat"
			-- matSize
			let matSizeDecls = do
				let funName = mkName "matSize"
				let clause = Clause [WildP] (NormalB $ TupE [LitE $ IntegerL $ toInteger n, LitE $ IntegerL $ toInteger m]) []
				let funDecl = FunD funName [clause]
				let specialiseDecl mathType = do
					let funType = AppT (AppT ArrowT (AppT dataCon mathType)) (AppT (AppT (TupleT 2) (ConT ''Int)) (ConT ''Int))
					return $ PragmaD $ SpecialiseP funName funType (Just Inline) AllPhases
				specialiseDecls <- mapM (specialiseDecl . ConT) mathTypeNames
				return $ funDecl : specialiseDecls
			decls <- matSizeDecls
			componentParamName <- newName "a"
			return $ InstanceD [] (AppT (AppT (ConT className) (AppT dataCon (VarT componentParamName))) (VarT componentParamName)) decls

		sequence $ [typeDec, genMatInstance]

-- | Generate multiplications.
genMuls :: Q [Dec]
genMuls = liftM concat $ sequence [genVecMatMuls, genMatVecMuls, genMatMatMuls] where
	dimensions = [1..maxVecDimension]
	dimensions2 = [(n, m) | n <- dimensions, m <- dimensions]
	dimensions3 = [(n, m, k) | n <- dimensions, m <- dimensions, k <- dimensions]
	genVecMatMuls = sequence [genVecMatMul n m | (n, m) <- dimensions2]
	genMatVecMuls = sequence [genMatVecMul n m | (n, m) <- dimensions2]
	genMatMatMuls = sequence [genMatMatMul n m k | (n, m, k) <- dimensions3]
	className = mkName "Mul"
	classType = ConT className
	mulName = mkName "mul"
	mathTypes = map ConT mathTypeNames
	{-
	instance Mul (Vec{n} e) (Mat{n}x{m} e) (Vec{m} e)
		mul (Vec{n} a1 a2 a3 ... an) (Mat{n}x{m} b11 b12 b13 ... bnm) = Vec{m} (...)
	-}
	genVecMatMul n m = do
		let aName = mkName $ "Vec" ++ [intToDigit n]
		let bName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let cName = mkName $ "Vec" ++ [intToDigit m]
		paramName <- newName "e"
		let eType = VarT paramName
		let aType = AppT (ConT aName) eType
		let bType = AppT (ConT bName) eType
		let cType = AppT (ConT cName) eType
		let aElemName i = mkName ['a', intToDigit i]
		let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
		let cElemResult j = foldl1 (\a b -> UInfixE a (VarE $ mkName "+") b) [UInfixE (VarE $ aElemName i) (VarE $ mkName "*") (VarE $ bElemName i j) | i <- [1..n]]
		let aElems = map aElemName [1..n]
		let bElems = [bElemName i j | i <- [1..n], j <- [1..m]]
		let cElems = foldl AppE (ConE cName) $ map cElemResult [1..m]
		let aPat = ConP aName $ map VarP aElems
		let bPat = ConP bName $ map VarP bElems
		let mulDecls = do
			let clause = Clause [aPat, bPat] (NormalB cElems) []
			let funDecl = FunD mulName [clause]
			let specialiseDecl mathType = do
				let specAType = AppT (ConT aName) mathType
				let specBType = AppT (ConT bName) mathType
				let specCType = AppT (ConT cName) mathType
				let mulType = AppT (AppT ArrowT specAType) $ AppT (AppT ArrowT specBType) specCType
				return $ PragmaD $ SpecialiseP mulName mulType (Just Inline) AllPhases
			specialiseDecls <- mapM specialiseDecl mathTypes
			return $ funDecl : specialiseDecls
		decls <- mulDecls
		return $ InstanceD [ClassP (mkName "Num") [eType]] (AppT (AppT (AppT classType aType) bType) cType) decls
	genMatVecMul n m = do
		let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let bName = mkName $ "Vec" ++ [intToDigit m]
		let cName = mkName $ "Vec" ++ [intToDigit n]
		paramName <- newName "e"
		let eType = VarT paramName
		let aType = AppT (ConT aName) eType
		let bType = AppT (ConT bName) eType
		let cType = AppT (ConT cName) eType
		let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
		let bElemName j = mkName ['b', intToDigit j]
		let cElemResult i = foldl1 (\a b -> UInfixE a (VarE $ mkName "+") b) [UInfixE (VarE $ aElemName i j) (VarE $ mkName "*") (VarE $ bElemName j) | j <- [1..m]]
		let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
		let bElems = map bElemName [1..m]
		let cElems = foldl AppE (ConE cName) $ map cElemResult [1..n]
		let aPat = ConP aName $ map VarP aElems
		let bPat = ConP bName $ map VarP bElems
		let mulDecls = do
			let clause = Clause [aPat, bPat] (NormalB cElems) []
			let funDecl = FunD mulName [clause]
			let specialiseDecl mathType = do
				let specAType = AppT (ConT aName) mathType
				let specBType = AppT (ConT bName) mathType
				let specCType = AppT (ConT cName) mathType
				let mulType = AppT (AppT ArrowT specAType) $ AppT (AppT ArrowT specBType) specCType
				return $ PragmaD $ SpecialiseP mulName mulType (Just Inline) AllPhases
			specialiseDecls <- mapM specialiseDecl mathTypes
			return $ funDecl : specialiseDecls
		decls <- mulDecls
		return $ InstanceD [ClassP (mkName "Num") [eType]] (AppT (AppT (AppT classType aType) bType) cType) decls
	genMatMatMul n m k = do
		let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let bName = mkName $ "Mat" ++ [intToDigit m, 'x', intToDigit k]
		let cName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit k]
		paramName <- newName "e"
		let eType = VarT paramName
		let aType = AppT (ConT aName) eType
		let bType = AppT (ConT bName) eType
		let cType = AppT (ConT cName) eType
		let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
		let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
		let cElemResult i j = foldl1 (\a b -> UInfixE a (VarE $ mkName "+") b) [UInfixE (VarE $ aElemName i t) (VarE $ mkName "*") (VarE $ bElemName t j) | t <- [1..m]]
		let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
		let bElems = [bElemName i j | i <- [1..m], j <- [1..k]]
		let cElems = foldl AppE (ConE cName) [cElemResult i j | i <- [1..n], j <- [1..k]]
		let aPat = ConP aName $ map VarP aElems
		let bPat = ConP bName $ map VarP bElems
		let mulDecls = do
			let clause = Clause [aPat, bPat] (NormalB cElems) []
			let funDecl = FunD mulName [clause]
			let specialiseDecl mathType = do
				let specAType = AppT (ConT aName) mathType
				let specBType = AppT (ConT bName) mathType
				let specCType = AppT (ConT cName) mathType
				let mulType = AppT (AppT ArrowT specAType) $ AppT (AppT ArrowT specBType) specCType
				return $ PragmaD $ SpecialiseP mulName mulType (Just Inline) AllPhases
			specialiseDecls <- mapM specialiseDecl mathTypes
			return $ funDecl : specialiseDecls
		decls <- mulDecls
		return $ InstanceD [ClassP (mkName "Num") [eType]] (AppT (AppT (AppT classType aType) bType) cType) decls

-- | Generate type synonyms for frequently used types
genSynonyms :: Q [Dec]
genSynonyms = do
	let vecSynonym n elemType elemChar = let name = "Vec" ++ [intToDigit n] in
		TySynD (mkName $ name ++ [elemChar]) [] $ AppT (ConT $ mkName name) elemType
	let vecSynonyms = [vecSynonym n (ConT t) c | n <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	let matSynonym n m elemType elemChar = let name = "Mat" ++ [intToDigit n, 'x', intToDigit m] in
		TySynD (mkName $ name ++ [elemChar]) [] $ AppT (ConT $ mkName name) elemType
	let matSynonyms = [matSynonym n m (ConT t) c | n <- [1..maxVecDimension], m <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	return $ vecSynonyms ++ matSynonyms
