{-|
Module: Flaw.Math
Description: Math.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}

module Flaw.Math
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, VecX(..), VecY(..), VecZ(..), VecW(..)
	, Vectorized(..)
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Mat3x4(..), Mat4x4(..)
	, SwizzleVecX1(..), SwizzleVecX2(..), SwizzleVecX3(..), SwizzleVecX4(..)
	, SwizzleVecY1(..), SwizzleVecY2(..), SwizzleVecY3(..), SwizzleVecY4(..)
	, SwizzleVecZ1(..), SwizzleVecZ2(..), SwizzleVecZ3(..), SwizzleVecZ4(..)
	, SwizzleVecW1(..), SwizzleVecW2(..), SwizzleVecW3(..), SwizzleVecW4(..)
	, CombineVec(..)
	, Combine2Vec(..)
	, Combine3Vec(..)
	, Combine4Vec(..)
	, Float1, Float2, Float3, Float4
	, Double1, Double2, Double3, Double4
	, Int32_1, Int32_2, Int32_3, Int32_4
	, Word32_1, Word32_2, Word32_3, Word32_4
	, Int1, Int2, Int3, Int4
	, Dot(..)
	, Cross(..)
	, Norm(..)
	, Normalize(..)
	, Mat(..)
	, Mul(..)
	, Float3x4, Float4x4
	, Double3x4, Double4x4
	, Int32_3x4, Int32_4x4
	, Word32_3x4, Word32_4x4
	, Int3x4, Int4x4
	, Quaternionized(..)
	, Quat(..)
	, Conjugate(..)
	, FloatQ, DoubleQ
	) where

import Control.Monad
import Data.Char
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Math.Internal

-- | General vector class.
class Vec v where
	type VecElement v :: *
	-- | Get number of components in vector.
	vecLength :: v -> Int -- v is unused
	-- | Convert vector to list.
	vecToList :: v -> [VecElement v]
	-- | Create vector from scalar (put scalar into every component).
	vecFromScalar :: VecElement v -> v

-- | Generate classes VecX..VecW with only method to access components.
{- Example:
class Vec v => VecX v where
	x_ :: v -> VecElement v
-}
forM vecComponents $ \c -> do
	let className = mkName $ "Vec" ++ [toUpper c]
	let methodName = mkName $ [c, '_']
	tvV <- newName "v"
	classD (return [AppT (ConT $ mkName "Vec") $ VarT tvV]) className [PlainTV tvV] []
		[ sigD methodName [t| $(varT tvV) -> VecElement $(varT tvV) |]
		]

-- | Generate class Vectorized.
{-
class Vectorized a where
	data Vec{1234} a :: *
	vec{n=1234} :: a...{n} -> Vec{n} a
	data Mat{1234}x{1234} a :: *
	mat{n=1234}x{m=1234} :: a...{n*m} -> Mat{n}x{m} a
-}
liftM return $ do
	tvA <- newName "a"
	vecDecs <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do
		let dimStr = [intToDigit dim]
		let dataDec = familyKindD dataFam (mkName $ "Vec" ++ dimStr) [PlainTV tvA] StarT
		let conFuncDec = sigD (mkName $ "vec" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) (appT (conT $ mkName $ "Vec" ++ dimStr) (varT tvA)) $ replicate dim $ varT tvA
		return [dataDec, conFuncDec]
	matDecs <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do
		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataDec = familyKindD dataFam (mkName $ "Mat" ++ dimStr) [PlainTV tvA] StarT
		let conFuncDec = sigD (mkName $ "mat" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) (appT (conT $ mkName $ "Mat" ++ dimStr) (varT tvA)) $ replicate (dimN * dimM) $ varT tvA
		return [dataDec, conFuncDec]
	classD (sequence []) (mkName "Vectorized") [PlainTV tvA] [] $ vecDecs ++ matDecs

-- | Class for dot operation.
class Vec v => Dot v where
	dot :: v -> v -> VecElement v

-- | Class for cross operation.
class Cross v where
	cross :: v -> v -> v

-- | Class for norm operation.
class Vec v => Norm v where
	norm :: v -> VecElement v
	norm2 :: v -> VecElement v

-- | Class for normalize operation.
class Normalize v where
	normalize :: v -> v

-- | General matrix class.
class Mat m where
	type MatElement m :: *
	-- | Get matrix size.
	matSize :: m -> (Int, Int) -- m is unused

-- | Class for general multiplication.
class Mul a b where
	type MulResult a b :: *
	mul :: a -> b -> MulResult a b

-- | Class for combining scalars and vectors into single vector.
class CombineVec a where
	type CombineVecResult a :: *
	combineVec :: a -> CombineVecResult a

-- | Generate classes CombineVec{2..4} and corresponding instances for CombineVec.
{-
class Combine2Vec a b where
	type Combine2VecResult a b :: *
	combine2Vec :: a -> b -> Combine2VecResult a b
instance Combine2Vec a b => CombineVec (a, b) where
	type CombineVecResult a b :: *
	combineVec (a, b) = combine2Vec a b
-}
liftM concat $ forM [2..4] $ \dim -> do
	let nameSuffix = [intToDigit dim] ++ "Vec"
	let className = mkName $ "Combine" ++ nameSuffix
	let methodName = mkName $ "combine" ++ nameSuffix
	let resultTypeName = mkName $ "Combine" ++ nameSuffix ++ "Result"
	params <- forM [1..dim] $ \i -> newName [chr $ i + ord 'a' - 1]
	classDec <- classD (return []) className (map PlainTV params) []
		[ familyKindD typeFam resultTypeName (map PlainTV params) StarT
		, sigD methodName $ foldr (\ a b -> appT (appT arrowT a) b) (foldl appT (conT resultTypeName) $ map varT params) $ map varT params
		]
	instanceDec <- instanceD (return [foldl AppT (ConT className) $ map VarT params]) [t| CombineVec $(foldl appT (tupleT dim) $ map varT params) |]
		[ tySynInstD ''CombineVecResult $ tySynEqn [foldl appT (tupleT dim) $ map varT params] $ foldl appT (conT resultTypeName) $ map varT params
		, funD 'combineVec [clause [tupP $ map varP params] (normalB $ foldl appE (varE methodName) $ map varE params) []]
		]
	return [classDec, instanceDec]

-- | Generates classes SwizzleVec{X..W}{1..4}.
{- Letter component should be presented in methods.
Number is a dimension of result.
class (VecX v, VecY v, VecZ v) => SwizzleVecZ2 v where
	type SwizzleVecZ2Result v :: *
	xz__ :: v -> SwizzleVecZ2Result v
	yz__ :: v -> SwizzleVecZ2Result v
	zx__ :: v -> SwizzleVecZ2Result v
	zy__ :: v -> SwizzleVecZ2Result v
	zz__ :: v -> SwizzleVecZ2Result v
-}
forM [(len, maxComp) | len <- [1..4], maxComp <- [1..4]] $ \(len, maxComp) -> do
	let components = take maxComp vecComponents
	let nameSuffix = [toUpper $ last components, intToDigit len]
	let className = mkName $ "SwizzleVec" ++ nameSuffix
	let resultTypeName = mkName $ "SwizzleVecResult" ++ nameSuffix
	tvV <- newName "v"
	let variants = filter (swizzleVariantFilter components) $ genSwizzleVariants len
	let genSig variant = do
		sigD (mkName $ variant ++ "__") [t| $(varT tvV) -> $(conT resultTypeName) $(varT tvV) |]
	classD (sequence [ [t| $(conT $ mkName $ "Vec" ++ [toUpper c]) $(varT tvV) |] | c <- components])
		className [PlainTV tvV] [] $ (familyKindD typeFam resultTypeName [PlainTV tvV] StarT) : map genSig variants

-- | Generate per-math type declarations.
liftM concat $ forM mathTypeNamesWithPrefix $ \(mathTypeName, mathTypePrefix) -> do

	let elemType = conT mathTypeName

	-- Vectorized instance, like Vectorized Float
	vectorizedInstance <- do
		-- things per vector dimension
		vecDecs <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do
			let dimStr = [intToDigit dim]
			let dataName = mkName $ "Vec" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM (take dim vecComponents) $ newName . return
			addInlines
				[ dataInstD (sequence []) dataName [elemType] [normalC conName (replicate dim $ return (Unpacked, ConT mathTypeName))] [''Eq, ''Ord, ''Show]
				, funD (mkName $ "vec" ++ dimStr) [clause (map varP components) (normalB $ foldl appE (conE conName) $ map varE components) []]
				]
		-- things per matrix dimension
		matDecs <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do
			let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
			let dataName = mkName $ "Mat" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM [(i, j) | i <- [1..dimN], j <- [1..dimM]] $ \(i, j) -> newName ['m', intToDigit i, intToDigit j]
			addInlines
				[ dataInstD (sequence []) dataName [elemType] [normalC conName (replicate (dimN * dimM) $ return (Unpacked, ConT mathTypeName))] [''Eq, ''Ord, ''Show]
				, funD (mkName $ "mat" ++ dimStr) [clause (map varP components) (normalB $ foldl appE (conE conName) $ map varE components) []]
				]
		instanceD (sequence []) (appT (conT $ mkName "Vectorized") elemType) $ vecDecs ++ matDecs

	-- instances per vector dimension, like VecX (Vec4 Float)
	vecInstances <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do

		let dimStr = [intToDigit dim]
		let dataName = mkName $ "Vec" ++ dimStr
		let conName = mkName $ mathTypePrefix ++ dimStr

		-- string with symbols of components, like "xyz"
		let components = take dim vecComponents
		-- names for component-parameters
		componentParams <- forM components $ \c -> newName [c]

		-- get some info about instances of math type
		hasFractional <- isInstance ''Fractional [ConT mathTypeName]
		hasFloating <- isInstance ''Floating [ConT mathTypeName]

		-- instance for Vec class
		vecInstance <- do
			let vecFromScalarDecl = do
				a <- newName "a"
				funD 'vecFromScalar [clause [varP a] (normalB $ foldl appE (conE conName) $ map varE $ replicate dim a) []]
			instanceD (return []) [t| Vec ($(conT dataName) $elemType) |] =<< addInlines
				[ tySynInstD ''VecElement $ tySynEqn [ [t| $(conT dataName) $elemType |] ] $ elemType
				, funD 'vecLength [clause [wildP] (normalB $ litE $ integerL $ fromIntegral dim) []]
				, funD 'vecToList [clause [conP conName $ map varP componentParams] (normalB $ listE $ map varE componentParams) []]
				, vecFromScalarDecl
				]

		-- instances for VecX .. VecW classes
		vecComponentInstances <- forM components $ \component -> do
			let className = mkName $ "Vec" ++ [toUpper component]
			let funName = mkName [component, '_']
			varName <- newName [component]
			instanceD (return []) [t| $(conT className) ($(conT dataName) $elemType) |] =<< addInlines
				[ funD funName [clause [conP conName [if c == component then (varP varName) else wildP | c <- components]] (normalB (varE varName)) []]
				]

		-- instance for Dot class
		dotInstance <- do
			as <- forM components $ \c -> newName $ ['a', c]
			bs <- forM components $ \c -> newName $ ['b', c]
			instanceD (sequence []) [t| Dot ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'dot
					[ clause
						[ conP conName $ map varP as
						, conP conName $ map varP bs
						]
						(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\(a, b) -> [| $(varE a) * $(varE b) |]) $ zip as bs) []
					]
				]

		-- instance for Norm class
		normInstances <- if hasFloating then do
			as <- forM components $ \c -> newName [c]
			inst <- instanceD (sequence []) [t| Norm ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'norm [clause [] (normalB $ [| sqrt . norm2 |]) []]
				, funD 'norm2 [clause [conP conName $ map varP as]
					(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\a -> [| $a * $a |]) $ map varE as) []]
				]
			return [inst]
			else return []

		-- instance for Normalize class
		normalizeInstances <- if hasFloating then do
			a <- newName "a"
			inst <- instanceD (sequence []) [t| Normalize ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'normalize [clause [varP a] (normalB [| $(varE a) * (vecFromScalar (1 / norm $(varE a))) |]) []]
				]
			return [inst]
			else return []

		-- instance for SwizzleVec{maxComp}{dim} class
		swizzleVecInstances <- forM [(srcDim, maxComp) | srcDim <- [1..4], maxComp <- [1..srcDim]] $ \(srcDim, maxComp) -> do
			let swizzleComponents = take maxComp vecComponents
			let nameSuffix = [toUpper $ last swizzleComponents, intToDigit dim]
			let instanceName = mkName $ "SwizzleVec" ++ nameSuffix
			tvV <- newName "v"
			let srcDataName = mkName $ "Vec" ++ [intToDigit srcDim]
			let resultDecl = tySynInstD (mkName $ "SwizzleVecResult" ++ nameSuffix) $ tySynEqn [ [t| $(conT srcDataName) $elemType |] ] $ [t| $(conT dataName) $elemType |]
			let variants = filter (swizzleVariantFilter swizzleComponents) $ genSwizzleVariants dim
			let funDecl variant = do
				let expr = foldl (\v c -> appE v [| $(varE (mkName $ [c, '_'])) $(varE tvV) |]) (conE conName) variant
				funD (mkName $ variant ++ "__") [clause [varP tvV] (normalB expr) []]
			instanceD (return []) [t| $(conT instanceName) ($(conT srcDataName) $elemType) |] =<< addInlines (resultDecl : (map funDecl variants))

		-- instances for CombineVec class
		let
			genCombineVecVariants :: Int -> [[Int]]
			genCombineVecVariants 0 = [[]]
			genCombineVecVariants n = [a : as | a <- [1..(if n == dim then n - 1 else n)], as <- genCombineVecVariants (n - a)]
			combineVecInstance variant = do
				pes <- forM variant $ \c -> do
					p <- newName "p"
					let es = if c > 1 then map (\q -> appE (varE $ mkName [q, '_']) $ varE p) $ take c vecComponents else [varE p]
					return (varP p, es)
				let paramTypes = map (\c -> if c > 1 then [t| $(conT $ mkName $ "Vec" ++ [intToDigit c]) $elemType |] else elemType) variant
				let expr = foldl appE (conE conName) $ concat $ map snd pes
				instanceD (return []) (foldl appT (conT $ mkName $ "Combine" ++ [intToDigit $ length variant] ++ "Vec") paramTypes) =<< addInlines
					[ tySynInstD (mkName $ "Combine" ++ [intToDigit $ length variant] ++ "VecResult") $ tySynEqn paramTypes [t| $(conT dataName) $elemType |]
					, funD (mkName $ "combine" ++ [intToDigit $ length variant] ++ "Vec") [clause (map fst pes) (normalB expr) []]
					]
		combineVecInstances <- mapM combineVecInstance (genCombineVecVariants dim)

		aParams <- mapM (\c -> newName $ ['a', c]) components
		bParams <- mapM (\c -> newName $ ['b', c]) components
		let binaryOp opName = funD opName
			[ clause
				[ conP conName $ map varP aParams
				, conP conName $ map varP bParams
				]
				(normalB $ foldl appE (conE conName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip aParams bParams)
				[]
			]
		let unaryOp opName = funD opName
			[ clause
				[ conP conName $ map varP aParams
				]
				(normalB $ foldl appE (conE conName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
				[]
			]
		let nullaryOp opName = funD opName
			[ clause []
				(normalB $ foldl appE (conE conName) $ map (\_ -> varE opName) aParams)
				[]
			]

		-- instance for Num class
		numInstance <- do
			let fromIntegerDecl = do
				iParam <- newName "i"
				fiParam <- newName "fi"
				funD 'fromInteger [clause [varP iParam]
					(normalB $ foldl appE (conE conName) $ replicate dim $ varE fiParam)
					[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

			instanceD (sequence []) [t| Num ($(conT dataName) $elemType) |] =<< addInlines
				[ binaryOp '(+)
				, binaryOp '(*)
				, binaryOp '(-)
				, unaryOp 'negate
				, unaryOp 'abs
				, unaryOp 'signum
				, fromIntegerDecl
				]

		-- instance for Fractional class
		fractionalInstances <- if hasFractional then do
			let fromRationalDecl = do
				rParam <- newName "r"
				frParam <- newName "fr"
				funD 'fromRational [clause [varP rParam]
					(normalB $ foldl appE (conE conName) $ replicate dim $ varE frParam)
					[valD (varP frParam) (normalB [| fromRational $(varE rParam) |]) []]]
			inst <- instanceD (sequence []) [t| Fractional ($(conT dataName) $elemType) |] =<< addInlines
				[ binaryOp '(/)
				, unaryOp 'recip
				, fromRationalDecl
				]
			return [inst]
			else return []

		-- instance for Floating class
		floatingInstances <- if hasFloating then do
			decls <- addInlines $ concat
				[ [nullaryOp 'pi]
				, map binaryOp
					[ '(**)
					, 'logBase
					]
				, map unaryOp
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
				]
			inst <- instanceD (sequence []) [t| Floating ($(conT dataName) $elemType) |] decls
			return [inst]
			else return []

		-- instance for Storable class
		storableInstance <- do
			p <- newName "p"
			let params = zip [0..(dim - 1)] aParams
			instanceD (sequence []) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral dim) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) |] | (i, a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) $(varE a) |] | (i, a) <- params]) []]
				]

		-- type synonym
		synonym <- tySynD conName [] [t| $(conT dataName) $elemType |]

		return $ vecInstance : dotInstance : numInstance : storableInstance : synonym :
			vecComponentInstances ++ swizzleVecInstances ++ combineVecInstances ++ normInstances ++ normalizeInstances ++ fractionalInstances ++ floatingInstances

	-- Cross instance
	crossInstance <- do
		names@[ax, ay, az, bx, by, bz] <- mapM newName ["ax", "ay", "az", "bx", "by", "bz"]
		let [axe, aye, aze, bxe, bye, bze] = map varE names
		let conName = mkName $ mathTypePrefix ++ "3"
		instanceD (sequence []) [t| Cross (Vec3 $elemType) |] =<< addInlines
			[ funD 'cross [clause [conP conName [varP ax, varP ay, varP az], conP conName [varP bx, varP by, varP bz]] (normalB
				[| $(conE conName)
					($aye * $bze - $aze * $bye)
					($aze * $bxe - $axe * $bze)
					($axe * $bye - $aye * $bxe)
					|]) [] ]
			]

	-- matrix instances
	matInstances <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do

		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataName = mkName $ "Mat" ++ dimStr
		let conName = mkName $ mathTypePrefix ++ dimStr

		-- some params
		aParams <- mapM newName [['a', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]
		bParams <- mapM newName [['b', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]

		-- Mat instance
		matInstance <- instanceD (sequence []) [t| Mat ($(conT dataName) $elemType) |] =<< addInlines
			[ tySynInstD ''MatElement $ tySynEqn [ [t| $(conT dataName) $elemType |] ] elemType
			, funD 'matSize [clause [wildP] (normalB [| ($(litE $ integerL $ toInteger dimN), $(litE $ integerL $ toInteger dimM)) |]) []]
			]

		-- Num instance
		numInstance <- do
			let binaryOp opName = funD opName
				[ clause
					[ conP conName $ map varP aParams
					, conP conName $ map varP bParams
					]
					(normalB $ foldl appE (conE conName) $ map (\(a, b) -> infixApp (varE a) (varE opName) (varE b)) $ zip aParams bParams)
					[]
				]
			let unaryOp opName = funD opName
				[ clause
					[ conP conName $ map varP aParams
					]
					(normalB $ foldl appE (conE conName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
					[]
				]
			let fromIntegerDecl = do
				iParam <- newName "i"
				fiParam <- newName "fi"
				funD 'fromInteger [clause [varP iParam]
					(normalB $ foldl appE (conE conName) $ replicate (dimN * dimM) $ varE fiParam)
					[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

			instanceD (sequence []) [t| Num ($(conT dataName) $elemType) |] =<< addInlines
				[ binaryOp '(+)
				, binaryOp '(*)
				, binaryOp '(-)
				, unaryOp 'negate
				, unaryOp 'abs
				, unaryOp 'signum
				, fromIntegerDecl
				]

		-- Storable instance (column-major)
		storableInstance <- do
			p <- newName "p"
			let params = zip [(i, j) | i <- [0..(dimN - 1)], j <- [0..(dimM - 1)]] aParams
			instanceD (sequence []) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral (dimN * dimM)) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) |] | ((i, j), a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) $(varE a) |] | ((i, j), a) <- params]) []]
				]

		-- type synonym
		synonym <- tySynD conName [] [t| $(conT dataName) $elemType |]

		return [matInstance, numInstance, storableInstance, synonym]

	-- Generate multiplications.
	mulInstances <- do
		let gen aName bName cName funDecl = instanceD (sequence []) [t| Mul ($(conT aName) $elemType) ($(conT bName) $elemType) |] =<< addInlines
			[ tySynInstD ''MulResult $ tySynEqn [ [t| $(conT aName) $elemType |], [t| $(conT bName) $elemType |] ] [t| $(conT cName) $elemType |]
			, funDecl
			]

		let genVecMatMul (n, m) = do
			let aName = mkName $ "Vec" ++ [intToDigit n]
			let aConName = mkName $ mathTypePrefix ++ [intToDigit n]
			let bName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
			let bConName = mkName $ mathTypePrefix ++ [intToDigit n, 'x', intToDigit m]
			let cName = mkName $ "Vec" ++ [intToDigit m]
			let cConName = mkName $ mathTypePrefix ++ [intToDigit m]
			let aElemName i = mkName ['a', intToDigit i]
			let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
			let cElemResult j = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i) * $(varE $ bElemName i j) |] | i <- [1..n]]
			let aElems = map aElemName [1..n]
			let bElems = [bElemName i j | i <- [1..n], j <- [1..m]]
			let cElems = foldl appE (conE cConName) $ map cElemResult [1..m]
			let aPat = conP aConName $ map varP aElems
			let bPat = conP bConName $ map varP bElems
			gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]
		let genMatVecMul (n, m) = do
			let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
			let aConName = mkName $ mathTypePrefix ++ [intToDigit n, 'x', intToDigit m]
			let bName = mkName $ "Vec" ++ [intToDigit m]
			let bConName = mkName $ mathTypePrefix ++ [intToDigit m]
			let cName = mkName $ "Vec" ++ [intToDigit n]
			let cConName = mkName $ mathTypePrefix ++ [intToDigit n]
			let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
			let bElemName j = mkName ['b', intToDigit j]
			let cElemResult i = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i j) * $(varE $ bElemName j) |] | j <- [1..m]]
			let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
			let bElems = map bElemName [1..m]
			let cElems = foldl appE (conE cConName) $ map cElemResult [1..n]
			let aPat = conP aConName $ map varP aElems
			let bPat = conP bConName $ map varP bElems
			gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]
		let genMatMatMul (n, m, k) = do
			let aName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
			let aConName = mkName $ mathTypePrefix ++ [intToDigit n, 'x', intToDigit m]
			let bName = mkName $ "Mat" ++ [intToDigit m, 'x', intToDigit k]
			let bConName = mkName $ mathTypePrefix ++ [intToDigit m, 'x', intToDigit k]
			let cName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit k]
			let cConName = mkName $ mathTypePrefix ++ [intToDigit n, 'x', intToDigit k]
			let aElemName i j = mkName ['a', intToDigit i, intToDigit j]
			let bElemName i j = mkName ['b', intToDigit i, intToDigit j]
			let cElemResult i j = foldl1 (\a b -> [| $a + $b |]) [ [| $(varE $ aElemName i t) * $(varE $ bElemName t j) |] | t <- [1..m]]
			let aElems = [aElemName i j | i <- [1..n], j <- [1..m]]
			let bElems = [bElemName i j | i <- [1..m], j <- [1..k]]
			let cElems = foldl appE (conE cConName) [cElemResult i j | i <- [1..n], j <- [1..k]]
			let aPat = conP aConName $ map varP aElems
			let bPat = conP bConName $ map varP bElems
			gen aName bName cName $ funD 'mul [clause [aPat, bPat] (normalB cElems) []]

		vecMatMuls <- mapM genVecMatMul $ do
			n <- [1..maxVecDimension]
			(m, k) <- matDimensions
			if n == m then [(m, k)]
			else []
		matVecMuls <- mapM genMatVecMul $ do
			(n, m) <- matDimensions
			k <- [1..maxVecDimension]
			if m == k then [(n, m)]
			else []
		matMatMuls <- mapM genMatMatMul $ do
			(n, m1) <- matDimensions
			(m2, k) <- matDimensions
			if m1 == m2 then [(n, m1, k)]
			else []
		
		return $ concat [vecMatMuls, matVecMuls, matMatMuls]

	return $ vectorizedInstance : crossInstance : vecInstances ++ matInstances ++ mulInstances

-- | Class of things which has quaternions.
class Vectorized a => Quaternionized a where
	data Quat a :: *
	quat :: Vec4 a -> Quat a

-- | Conjugation.
class Conjugate q where
	conjugate :: q -> q

-- | Generate per-math type declarations for quaternions.
liftM concat $ forM mathQuaternionTypeNamesWithPrefix $ \(mathTypeName, mathTypePrefix) -> do

	let elemType = conT mathTypeName
	let conName = mkName $ mathTypePrefix ++ "Q"
	a <- newName "a"
	v <- newName "v"
	v1 <- newName "v1"
	v2 <- newName "v2"
	args1 <- mapM (newName . (: "1")) vecComponents
	args2 <- mapM (newName . (: "2")) vecComponents
	let [x1, y1, z1, w1] = map varE args1
	let [x2, y2, z2, w2] = map varE args2
	let vecConName = mkName $ mathTypePrefix ++ "4"

	-- Quaternionized instance
	quaternionizedInstance <- instanceD (sequence []) [t| Quaternionized $elemType |] =<< addInlines
		[ newtypeInstD (sequence []) ''Quat [elemType] (normalC conName [liftM (\t -> (NotStrict, t)) [t| Vec4 $elemType |]]) [''Eq, ''Show]
		, funD 'quat [clause [varP v] (normalB [| $(conE conName) $(varE v) |]) []]
		]

	-- Vec instance
	vecInstance <- instanceD (return []) [t| Vec (Quat $elemType) |] =<< addInlines
		[ tySynInstD ''VecElement $ tySynEqn [ [t| Quat $elemType |] ] elemType
		, funD 'vecLength [clause [conP conName [varP v]] (normalB [| vecLength $(varE v) |]) []]
		, funD 'vecToList [clause [conP conName [varP v]] (normalB [| vecToList $(varE v) |]) []]
		, funD 'vecFromScalar [clause [varP a] (normalB [| $(conE conName) (vecFromScalar $(varE a)) |]) []]
		]

	-- Norm instance
	normInstance <- instanceD (sequence []) [t| Norm (Quat $elemType) |] =<< addInlines
		[ funD 'norm [clause [conP conName [varP v]] (normalB [| norm $(varE v) |]) []]
		, funD 'norm2 [clause [conP conName [varP v]] (normalB [| norm2 $(varE v) |]) []]
		]

	-- Normalize instance
	normalizeInstance <- instanceD (sequence []) [t| Normalize (Quat $elemType) |] =<< addInlines
		[ funD 'normalize [clause [conP conName [varP v]] (normalB [| $(conE conName) (normalize $(varE v)) |]) []]
		]

	-- Num instance
	numInstance <- do
		instanceD (sequence []) [t| Num (Quat $elemType) |] =<< addInlines
			[ funD '(+) [clause [conP conName [varP v1], conP conName [varP v2]] (normalB [| $(conE conName) ($(varE v1) + $(varE v2)) |]) []]
			, funD '(-) [clause [conP conName [varP v1], conP conName [varP v2]] (normalB [| $(conE conName) ($(varE v1) - $(varE v2)) |]) []]
			, funD '(*) [clause
				[ conP conName [conP vecConName $ map varP args1]
				, conP conName [conP vecConName $ map varP args2]
				] (normalB [| $(conE conName) ($(conE vecConName)
					($w1 * $x2 + $x1 * $w2 + $y1 * $z2 - $z1 * $y2)
					($w1 * $y2 - $x1 * $z2 + $y1 * $w2 + $z1 * $x2)
					($w1 * $z2 - $x1 * $y2 - $y1 * $x2 + $z1 * $w2)
					($w1 * $w2 - $x1 * $x2 - $y1 * $y2 - $z1 * $z2)
					)|]) []]
			, funD 'negate [clause [conP conName [varP v]] (normalB [| $(conE conName) (negate $(varE v)) |]) []]
			, funD 'abs [clause [conP conName [varP v]] (normalB [| $(conE conName) (abs $(varE v)) |]) []]
			, funD 'signum [clause [] (normalB [| undefined |]) []]
			, funD 'fromInteger [clause [] (normalB [| undefined |]) []]
			]

	-- Conjugate instance
	conjugateInstance <- instanceD (sequence []) [t| Conjugate (Quat $elemType) |] =<< addInlines
		[ funD 'conjugate [clause [conP conName [conP vecConName $ map varP args1]] (normalB [| $(conE conName) ($(conE vecConName) (- $x1) (- $y1) (- $z1) $w1) |]) []]
		]

	-- synonym
	synonym <- tySynD conName [] [t| Quat $elemType |]

	return [quaternionizedInstance, vecInstance, normInstance, normalizeInstance, numInstance, conjugateInstance, synonym]
