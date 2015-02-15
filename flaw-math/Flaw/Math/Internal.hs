{-|
Module: Flaw.Math.Internal
Description: Template-Haskell generating math types and functions.
License: MIT
-}

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, RankNTypes, TemplateHaskell #-}

module Flaw.Math.Internal
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, Dot(..)
	, Cross(..)
	, Mat(..)
	, Mul(..)
	, genVecClasses
	, genSwizzleVecClasses
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
class Vec v e where
	-- | Get number of components in vector.
	vecLength :: v -> Int -- v is unused
	-- | Convert vector to list.
	vecToList :: v -> [e]

-- | Class for dot operation.
class Dot v e where
	dot :: Num e => v -> v -> e

-- | Class for cross operation.
class Cross v where
	cross :: v -> v -> v

-- | General matrix class.
class Mat m e where
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
		tvV <- newName "v"
		tvE <- newName "e"
		classD (return [ClassP (mkName "Vec") [VarT tvV, VarT tvE]]) className [PlainTV tvV, PlainTV tvE] []
			[ sigD methodName [t| $(varT tvV) -> $(varT tvE) |]
			]

-- | Generates classes SwizzleVec1..SwizzleVec4.
{- Example:
class Vec a e => SwizzleVec2 a e where
	xx__ :: VecX v e => v -> a
	xy__ :: (VecX v e, VecY v e) => v -> a
	xz__ :: (VecX v e, VecZ v e) => v -> a
	xw__ :: (VecX v e, VecW v e) => v -> a
	...
-}
genSwizzleVecClasses :: Q [Dec]
genSwizzleVecClasses = mapM genSwizzleVecClass [1..4] where
	genSwizzleVecClass len = do
		let className = mkName $ "SwizzleVec" ++ [intToDigit len]
		tvA <- newName "a"
		tvE <- newName "e"
		tvV <- newName "v"
		let genSig variant = do
			let context = return $ concat [if elem c variant then [ClassP (mkName $ "Vec" ++ [toUpper c]) [VarT tvV, VarT tvE]] else [] | c <- vecComponents]
			sigD (mkName $ variant ++ "__") $ forallT [PlainTV tvV] context [t| $(varT tvV) -> $(varT tvA) |]
		classD (return [ClassP (mkName "Vec") [VarT tvA, VarT tvE]]) className [PlainTV tvA, PlainTV tvE] [] $ map genSig $ genSwizzleVariants len

-- | Return list of swizzle variants for a given length.
genSwizzleVariants :: Int -> [String]
genSwizzleVariants 0 = [""]
genSwizzleVariants len = [c : v | c <- vecComponents, v <- genSwizzleVariants $ len - 1]

-- | Generate actual vector data with instances.
{- Example:

data Vec4 a = Vec4 a a a a deriving Show
instance VecX (Vec4 a) where
	x_ (Vec4 x _ _ _) = x
-- VecY, ...

instance Vec (Vec4 a) where
	vecLength _ = 4
	vecToList (Vec4 x y z w) = [x, y, z, w]

instance Num a => Num (Vec4 a) where
	...
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
		-- data declaration
		let dataDec = dataD (return []) dataName tyVarBinds
			[ normalC dataName (replicate dim (return (IsStrict, VarT tvA)))
			] [''Eq, ''Ord, ''Show]

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
				pragSpecInlD funName [t| $(conT dataName) $mathType -> $mathType |] Inline AllPhases
			let specialiseDecls = map (specialiseDecl . conT) mathTypeNames
			let decls = funDecl : specialiseDecls
			instanceD (return []) [t| $(conT className) ($(conT dataName) $(varT tvA)) $(varT tvA) |] decls

		-- instance for Vec class
		let vecInstance = do
			-- vecLength
			let vecLengthDecl = funD 'vecLength [clause [wildP] (normalB $ litE $ integerL $ fromIntegral dim) []]
			-- vecToList
			vecToListDecls <- do
				componentParams <- mapM (\c -> newName [c]) components
				let funDecl = funD 'vecToList [clause [conP dataName $ map varP componentParams] (normalB $ listE $ map varE componentParams) []]
				let specialiseDecl mathType = do
					pragSpecInlD 'vecToList [t| $(conT dataName) $mathType -> [$mathType] |] Inline AllPhases
				let specialiseDecls = map (specialiseDecl . conT) mathTypeNames
				return $ funDecl : specialiseDecls
			let decls = vecLengthDecl : vecToListDecls
			instanceD (return []) [t| Vec ($(conT dataName) $(varT tvA)) $(varT tvA) |] decls

		-- instance for SwizzleVec{dim} class
		let swizzleVecInstance = do
			let instanceName = mkName $ "SwizzleVec" ++ [intToDigit dim]
			tvV <- newName "v"
			let funDecl variant = do
				let expr = foldl (\v c -> appE v [| $(varE (mkName $ [c, '_'])) $(varE tvV) |]) (conE dataName) variant
				funD (mkName $ variant ++ "__") [clause [varP tvV] (normalB $ expr) []]
			instanceD (return []) [t| $(conT instanceName) ($(conT dataName) $(varT tvA)) $(varT tvA) |] $ map funDecl $ genSwizzleVariants dim

		paramName <- newName "a"
		aParams <- mapM (\c -> newName $ ['a', c]) components
		bParams <- mapM (\c -> newName $ ['b', c]) components
		let supportedMathTypeNames className = do
			filterM (\mathTypeName -> isInstance className [ConT mathTypeName]) mathTypeNames
		let binaryOp opName mtn = let
			funDecl = funD opName [clause
				[ conP dataName $ map varP aParams
				, conP dataName $ map varP bParams
				]
				(normalB $ foldl appE (conE dataName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip aParams bParams)
				[]]
			specialiseDecl mathType = do
				pragSpecInlD opName [t| $(conT dataName) $mathType -> $(conT dataName) $mathType -> $(conT dataName) $mathType |] Inline AllPhases
			in funDecl : map (specialiseDecl . conT) mtn
		let unaryOp opName mtn = let
			funDecl = funD opName [clause
				[ conP dataName $ map varP aParams
				]
				(normalB $ foldl appE (conE dataName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
				[]]
			specialiseDecl mathType = do
				pragSpecInlD opName [t| $(conT dataName) $mathType -> $(conT dataName) $mathType |] Inline AllPhases
			in funDecl : map (specialiseDecl . conT) mtn
		let nullaryOp opName mtn = let
			funDecl = funD opName [clause []
				(normalB $ foldl appE (conE dataName) $ map (\_ -> varE opName) aParams)
				[]]
			specialiseDecl mathType = do
				pragSpecInlD opName [t| $(conT dataName) $mathType |] Inline AllPhases
			in funDecl : map (specialiseDecl . conT) mtn

		-- instance for Num class
		mtnNum <- supportedMathTypeNames ''Num
		let numInstance = do
			let fromIntegerDecl = do
				iParam <- newName "i"
				fiParam <- newName "fi"
				funD 'fromInteger [clause [varP iParam]
					(normalB $ foldl appE (conE dataName) $ replicate dim $ varE fiParam)
					[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

			let funcs = concat
				[ binaryOp '(+) mtnNum
				, binaryOp '(*) mtnNum
				, binaryOp '(-) mtnNum
				, unaryOp 'negate mtnNum
				, unaryOp 'abs mtnNum
				, unaryOp 'signum mtnNum
				, [fromIntegerDecl]
				]
			instanceD (return [ClassP ''Num [VarT paramName]]) [t| Num ($(conT dataName) $(varT paramName)) |] funcs

		-- instance for Fractional class
		mtnFractional <- supportedMathTypeNames ''Fractional
		let fractionalInstance = do
			let fromRationalDecl = do
				rParam <- newName "r"
				frParam <- newName "fr"
				funD 'fromRational [clause [varP rParam]
					(normalB $ foldl appE (conE dataName) $ replicate dim $ varE frParam)
					[valD (varP frParam) (normalB [| fromRational $(varE rParam) |]) []]]
			instanceD (return [ClassP ''Fractional [VarT paramName]]) [t| Fractional ($(conT dataName) $(varT paramName)) |] (concat
				[ binaryOp '(/) mtnFractional
				, unaryOp 'recip mtnFractional
				, [fromRationalDecl]
				])

		-- instance for Floating class
		mtnFloating <- supportedMathTypeNames ''Floating
		let floatingInstance = do
			instanceD (return [ClassP ''Floating [VarT paramName]]) [t| Floating ($(conT dataName) $(varT paramName)) |] (concat $ concat
				[ [nullaryOp 'pi mtnFloating]
				, map (\op -> binaryOp op mtnFloating)
					[ '(**)
					, 'logBase
					]
				, map (\op -> unaryOp op mtnFloating)
					[ 'exp
					, 'sqrt
					, 'log
					, 'sin
					, 'tan
					, 'cos
					, 'asin
					, 'atan
					, 'acos
					, 'sinh
					, 'tanh
					, 'cosh
					, 'asinh
					, 'atanh
					, 'acosh
					]
				])

		sequence $ dataDec : vecInstance : swizzleVecInstance : numInstance : fractionalInstance : floatingInstance : (map genVecComponentInstance components)

-- | Generate matrix datatypes.
genMatDatas :: Q [Dec]
genMatDatas = liftM concat $ mapM genMatData [(i, j) | i <- dimensions, j <- dimensions] where
	dimensions = [1..maxVecDimension]
	genMatData (n, m) = do
		-- name of data (Mat{n}x{m})
		let dataName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		-- name of type parameter
		tvA <- newName "a"
		-- data declaration
		let dataDec = dataD (return []) dataName [PlainTV tvA]
			[ normalC dataName (replicate (n * m) $ return (IsStrict, VarT tvA))
			] [''Eq, ''Ord, ''Show]

		-- instance for Mat class
		let matInstance = instanceD (return []) [t| Mat ($(conT dataName) $(varT tvA)) $(varT tvA) |]
			[ funD 'matSize [clause [wildP] (normalB [| ($(litE $ integerL $ toInteger n), $(litE $ integerL $ toInteger m)) |]) []]
			]

		-- instance for Num class
		let numInstance = do
			paramName <- newName "a"
			aParams <- sequence [newName $ ['a', '_', intToDigit i, '_', intToDigit j] | i <- [1..n], j <- [1..m]]
			bParams <- sequence [newName $ ['b', '_', intToDigit i, '_', intToDigit j] | i <- [1..n], j <- [1..m]]
			let binaryOp opName = let
				funDecl = funD opName [clause
					[ conP dataName $ map varP aParams
					, conP dataName $ map varP bParams
					]
					(normalB $ foldl appE (conE dataName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip aParams bParams)
					[]]
				specialiseDecl mathType = do
					pragSpecInlD opName [t| $(conT dataName) $mathType -> $(conT dataName) $mathType -> $(conT dataName) $mathType |] Inline AllPhases
				in funDecl : map (specialiseDecl . conT) mathTypeNames
			let unaryOp opName = let
				funDecl = funD opName [clause
					[ conP dataName $ map varP aParams
					]
					(normalB $ foldl appE (conE dataName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
					[]]
				specialiseDecl mathType = do
					pragSpecInlD opName [t| $(conT dataName) $mathType -> $(conT dataName) $mathType |] Inline AllPhases
				in funDecl : map (specialiseDecl . conT) mathTypeNames

			let fromIntegerDecl = do
				iParam <- newName "i"
				fiParam <- newName "fi"
				funD 'fromInteger [clause [varP iParam]
					(normalB $ foldl appE (conE dataName) $ replicate (n * m) $ varE fiParam)
					[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

			instanceD (return [ClassP ''Num [VarT paramName]]) [t| Num ($(conT dataName) $(varT paramName)) |] (concat
				[ binaryOp '(+)
				, binaryOp '(*)
				, binaryOp '(-)
				, unaryOp 'negate
				, unaryOp 'abs
				, unaryOp 'signum
				, [fromIntegerDecl]
				])

		sequence $ [dataDec, matInstance, numInstance]

-- | Generate multiplications.
genMuls :: Q [Dec]
genMuls = liftM concat $ sequence [genVecMatMuls, genMatVecMuls, genMatMatMuls] where
	dimensions = [1..maxVecDimension]
	dimensions2 = [(n, m) | n <- dimensions, m <- dimensions]
	dimensions3 = [(n, m, k) | n <- dimensions, m <- dimensions, k <- dimensions]
	genVecMatMuls = sequence [genVecMatMul n m | (n, m) <- dimensions2]
	genMatVecMuls = sequence [genMatVecMul n m | (n, m) <- dimensions2]
	genMatMatMuls = sequence [genMatMatMul n m k | (n, m, k) <- dimensions3]
	mathTypes = map conT mathTypeNames

	gen aName bName cName funDecl = do
		let specialiseDecl mathType = do
			pragSpecInlD 'mul [t| $(conT aName) $mathType -> $(conT bName) $mathType -> $(conT cName) $mathType |] Inline AllPhases
		let specialiseDecls = map specialiseDecl mathTypes
		eName <- newName "e"
		let eType = varT eName
		instanceD (return [ClassP ''Num [VarT eName]]) [t| Mul ($(conT aName) $eType) ($(conT bName) $eType) ($(conT cName) $eType) |] $ funDecl : specialiseDecls

	genVecMatMul n m = do
		let aName = mkName $ "Vec" ++ [intToDigit n]
		let bName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let cName = mkName $ "Vec" ++ [intToDigit m]
		let aElemName i = mkName ['a', intToDigit i]
		let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
		let cElemResult j = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i) * $(varE $ bElemName i j) |] | i <- [1..n]]
		let aElems = map aElemName [1..n]
		let bElems = [bElemName i j | i <- [1..n], j <- [1..m]]
		let cElems = foldl appE (conE cName) $ map cElemResult [1..m]
		let aPat = conP aName $ map varP aElems
		let bPat = conP bName $ map varP bElems
		gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]
	genMatVecMul n m = do
		let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let bName = mkName $ "Vec" ++ [intToDigit m]
		let cName = mkName $ "Vec" ++ [intToDigit n]
		let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
		let bElemName j = mkName ['b', intToDigit j]
		let cElemResult i = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i j) * $(varE $ bElemName j) |] | j <- [1..m]]
		let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
		let bElems = map bElemName [1..m]
		let cElems = foldl appE (conE cName) $ map cElemResult [1..n]
		let aPat = conP aName $ map varP aElems
		let bPat = conP bName $ map varP bElems
		gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]
	genMatMatMul n m k = do
		let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
		let bName = mkName $ "Mat" ++ [intToDigit m, 'x', intToDigit k]
		let cName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit k]
		let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
		let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
		let cElemResult i j = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i t) * $(varE $ bElemName t j) |] | t <- [1..m]]
		let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
		let bElems = [bElemName i j | i <- [1..m], j <- [1..k]]
		let cElems = foldl appE (conE cName) [cElemResult i j | i <- [1..n], j <- [1..k]]
		let aPat = conP aName $ map varP aElems
		let bPat = conP bName $ map varP bElems
		gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]

-- | Generate type synonyms for frequently used types
genSynonyms :: Q [Dec]
genSynonyms = do
	let vecSynonym n elemType elemChar = let name = "Vec" ++ [intToDigit n] in
		tySynD (mkName $ name ++ [elemChar]) [] [t| $(conT $ mkName name) $elemType |]
	vecSynonyms <- sequence [vecSynonym n (conT t) c | n <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	let matSynonym n m elemType elemChar = let name = "Mat" ++ [intToDigit n, 'x', intToDigit m] in
		tySynD (mkName $ name ++ [elemChar]) [] [t| $(conT $ mkName name) $elemType |]
	matSynonyms <- sequence [matSynonym n m (conT t) c | n <- [1..maxVecDimension], m <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	return $ vecSynonyms ++ matSynonyms
