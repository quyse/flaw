{-|
Module: Flaw.Math
Description: Math.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}

module Flaw.Math
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, VecX(..), VecY(..), VecZ(..), VecW(..)
	, SwizzleVecX1(..), SwizzleVecX2(..), SwizzleVecX3(..), SwizzleVecX4(..)
	, SwizzleVecY1(..), SwizzleVecY2(..), SwizzleVecY3(..), SwizzleVecY4(..)
	, SwizzleVecZ1(..), SwizzleVecZ2(..), SwizzleVecZ3(..), SwizzleVecZ4(..)
	, SwizzleVecW1(..), SwizzleVecW2(..), SwizzleVecW3(..), SwizzleVecW4(..)
	, CombineVec(..)
	, Combine2Vec(..)
	, Combine3Vec(..)
	, Combine4Vec(..)
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Vec1f, Vec2f, Vec3f, Vec4f
	, Vec1d, Vec2d, Vec3d, Vec4d
	, Vec1i, Vec2i, Vec3i, Vec4i
	, Vec1u, Vec2u, Vec3u, Vec4u
	, Dot(..)
	, Cross(..)
	, Norm(..)
	, Normalize(..)
	, Mat(..)
	, Mul(..)
	, Mat1x1(..), Mat1x2(..), Mat1x3(..), Mat1x4(..)
	, Mat2x1(..), Mat2x2(..), Mat2x3(..), Mat2x4(..)
	, Mat3x1(..), Mat3x2(..), Mat3x3(..), Mat3x4(..)
	, Mat4x1(..), Mat4x2(..), Mat4x3(..), Mat4x4(..)
	, Mat1x1f, Mat1x2f, Mat1x3f, Mat1x4f
	, Mat2x1f, Mat2x2f, Mat2x3f, Mat2x4f
	, Mat3x1f, Mat3x2f, Mat3x3f, Mat3x4f
	, Mat4x1f, Mat4x2f, Mat4x3f, Mat4x4f
	, Mat1x1d, Mat1x2d, Mat1x3d, Mat1x4d
	, Mat2x1d, Mat2x2d, Mat2x3d, Mat2x4d
	, Mat3x1d, Mat3x2d, Mat3x3d, Mat3x4d
	, Mat4x1d, Mat4x2d, Mat4x3d, Mat4x4d
	, Mat1x1i, Mat1x2i, Mat1x3i, Mat1x4i
	, Mat2x1i, Mat2x2i, Mat2x3i, Mat2x4i
	, Mat3x1i, Mat3x2i, Mat3x3i, Mat3x4i
	, Mat4x1i, Mat4x2i, Mat4x3i, Mat4x4i
	, Mat1x1u, Mat1x2u, Mat1x3u, Mat1x4u
	, Mat2x1u, Mat2x2u, Mat2x3u, Mat2x4u
	, Mat3x1u, Mat3x2u, Mat3x3u, Mat3x4u
	, Mat4x1u, Mat4x2u, Mat4x3u, Mat4x4u
	, Quaternion(..)
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

-- | Generates classes VecX..VecW with only method to access components.
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

-- | Class for dot operation.
class Vec v => Dot v where
	dot :: v -> v -> VecElement v

-- | Class for cross operation.
class Cross v where
	cross :: v -> v -> v

-- | Class for norm operation.
class Dot v => Norm v where
	norm :: v -> VecElement v
	norm2 :: v -> VecElement v

-- | Class for normalize operation.
class Norm v => Normalize v where
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

-- | Generate actual vector data with instances.
{- Example:

data Vec4 a = Vec4 a a a a deriving Show

instance Vec (Vec4 a) where
	type VecElement (Vec4 a) = a
	vecLength _ = 4
	vecToList (Vec4 x y z w) = [x, y, z, w]
	vecFromScalar a = Vec4 a a a a

instance VecX (Vec4 a) where
	x_ (Vec4 x _ _ _) = x
-- VecY, ...

instance Num a => Num (Vec4 a) where
	...
-}
liftM concat $ forM [1..maxVecDimension] $ \dim -> do
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
	-- names for component-parameters
	componentParams <- forM components $ \c -> newName [c]

	-- instance for Vec class
	let vecInstance = do
		let vecFromScalarDecl = do
			a <- newName "a"
			funD 'vecFromScalar [clause [varP a] (normalB $ foldl appE (conE dataName) $ map varE $ replicate dim a) []]
		instanceD (return []) [t| Vec ($(conT dataName) $(varT tvA)) |]
			[ tySynInstD ''VecElement $ tySynEqn [ [t| $(conT dataName) $(varT tvA) |] ] $ varT tvA
			, funD 'vecLength [clause [wildP] (normalB $ litE $ integerL $ fromIntegral dim) []]
			, funD 'vecToList [clause [conP dataName $ map varP componentParams] (normalB $ listE $ map varE componentParams) []]
			, vecFromScalarDecl
			]

	-- instances for VecX .. VecW classes
	let genVecComponentInstance component = do
		let className = mkName $ "Vec" ++ [toUpper component]
		let funName = mkName [component, '_']
		varName <- newName [component]
		instanceD (return []) [t| $(conT className) ($(conT dataName) $(varT tvA)) |]
			[ funD funName [clause [conP dataName [if c == component then (varP varName) else wildP | c <- components]] (normalB (varE varName)) []]
			]

	-- instance for Dot class
	let dotInstance = do
		as <- forM components $ \c -> newName $ ['a', c]
		bs <- forM components $ \c -> newName $ ['b', c]
		instanceD (sequence [ [t| Num $(varT tvA) |] ]) [t| Dot ($(conT dataName) $(varT tvA)) |]
			[ funD 'dot
				[ clause
					[ conP dataName $ map varP as
					, conP dataName $ map varP bs
					]
					(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\(a, b) -> [| $(varE a) * $(varE b) |]) $ zip as bs) []
				]
			]

	-- instance for Norm class
	let normInstance = do
		as <- forM components $ \c -> newName [c]
		instanceD (sequence [ [t| Floating $(varT tvA) |] ]) [t| Norm ($(conT dataName) $(varT tvA)) |]
			[ funD 'norm [clause [] (normalB $ [| sqrt . norm2 |]) []]
			, funD 'norm2 [clause [conP dataName $ map varP as]
				(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\a -> [| $a * $a |]) $ map varE as) []]
			]

	-- instance for Normalize class
	let normalizeInstance = do
		a <- newName "a"
		instanceD (sequence [ [t| Floating $(varT tvA) |] ]) [t| Normalize ($(conT dataName) $(varT tvA)) |]
			[ funD 'normalize [clause [varP a] (normalB [| $(varE a) * (vecFromScalar (1 / norm $(varE a))) |]) []]
			]

	-- instance for SwizzleVec{maxComp}{dim} class
	let swizzleVecInstances = map swizzleVecInstance [(srcDim, maxComp) | srcDim <- [1..4], maxComp <- [1..srcDim]] where
		swizzleVecInstance (srcDim, maxComp) = do
			let swizzleComponents = take maxComp vecComponents
			let nameSuffix = [toUpper $ last swizzleComponents, intToDigit dim]
			let instanceName = mkName $ "SwizzleVec" ++ nameSuffix
			tvV <- newName "v"
			let srcDataName = mkName $ "Vec" ++ [intToDigit srcDim]
			let resultDecl = tySynInstD (mkName $ "SwizzleVecResult" ++ nameSuffix) $ tySynEqn [ [t| $(conT srcDataName) $(varT tvA) |] ] $ [t| $(conT dataName) $(varT tvA) |]
			let variants = filter (swizzleVariantFilter swizzleComponents) $ genSwizzleVariants dim
			let funDecl variant = do
				let expr = foldl (\v c -> appE v [| $(varE (mkName $ [c, '_'])) $(varE tvV) |]) (conE dataName) variant
				funD (mkName $ variant ++ "__") [clause [varP tvV] (normalB expr) []]
			instanceD (return []) [t| $(conT instanceName) ($(conT srcDataName) $(varT tvA)) |] $ resultDecl : map funDecl variants

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
			let instanceDecl elemType = do
				let paramTypes = map (\c -> if c > 1 then [t| $(conT $ mkName $ "Vec" ++ [intToDigit c]) $elemType |] else elemType) variant
				let expr = foldl appE (conE dataName) $ concat $ map snd pes
				instanceD (return []) (foldl appT (conT $ mkName $ "Combine" ++ [intToDigit $ length variant] ++ "Vec") paramTypes)
					[ tySynInstD (mkName $ "Combine" ++ [intToDigit $ length variant] ++ "VecResult") $ tySynEqn paramTypes [t| $(conT dataName) $elemType |]
					, funD (mkName $ "combine" ++ [intToDigit $ length variant] ++ "Vec") [clause (map fst pes) (normalB expr) []]
					]
			return $ map (instanceDecl . conT) mathTypeNames
	combineVecInstances <- liftM concat $ mapM combineVecInstance (genCombineVecVariants dim)

	paramName <- newName "a"
	aParams <- mapM (\c -> newName $ ['a', c]) components
	bParams <- mapM (\c -> newName $ ['b', c]) components
	let binaryOp opName = funD opName
		[ clause
			[ conP dataName $ map varP aParams
			, conP dataName $ map varP bParams
			]
			(normalB $ foldl appE (conE dataName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip aParams bParams)
			[]
		]
	let unaryOp opName = funD opName
		[ clause
			[ conP dataName $ map varP aParams
			]
			(normalB $ foldl appE (conE dataName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
			[]
		]
	let nullaryOp opName = funD opName
		[ clause []
			(normalB $ foldl appE (conE dataName) $ map (\_ -> varE opName) aParams)
			[]
		]

	-- instance for Num class
	let numInstance = do
		let fromIntegerDecl = do
			iParam <- newName "i"
			fiParam <- newName "fi"
			funD 'fromInteger [clause [varP iParam]
				(normalB $ foldl appE (conE dataName) $ replicate dim $ varE fiParam)
				[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

		instanceD (sequence [ [t| Num $(varT paramName) |] ]) [t| Num ($(conT dataName) $(varT paramName)) |]
			[ binaryOp '(+)
			, binaryOp '(*)
			, binaryOp '(-)
			, unaryOp 'negate
			, unaryOp 'abs
			, unaryOp 'signum
			, fromIntegerDecl
			]

	-- instance for Fractional class
	let fractionalInstance = do
		let fromRationalDecl = do
			rParam <- newName "r"
			frParam <- newName "fr"
			funD 'fromRational [clause [varP rParam]
				(normalB $ foldl appE (conE dataName) $ replicate dim $ varE frParam)
				[valD (varP frParam) (normalB [| fromRational $(varE rParam) |]) []]]
		instanceD (sequence [ [t| Fractional $(varT paramName) |] ]) [t| Fractional ($(conT dataName) $(varT paramName)) |]
			[ binaryOp '(/)
			, unaryOp 'recip
			, fromRationalDecl
			]

	-- instance for Floating class
	let floatingInstance = do
		instanceD (sequence [ [t| Floating $(varT paramName) |] ]) [t| Floating ($(conT dataName) $(varT paramName)) |] $ concat
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

	-- instance for Storable class
	let storableInstance = do
		p <- newName "p"
		let params = zip [0..(dim - 1)] aParams
		instanceD (sequence [ [t| Storable $(varT paramName) |] ]) [t| Storable ($(conT dataName) $(varT paramName)) |]
			[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral dim) * sizeOf (undefined :: $(varT paramName)) |]) []]
			, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $(varT paramName)) |]) []]
			, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) |] | (i, a) <- params] ++
				[noBindS [| return $(foldl appE (conE dataName) $ map (varE . snd) params) |]]) []]
			, funD 'poke [clause [varP p, conP dataName $ map (varP . snd) params]
				(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) $(varE a) |] | (i, a) <- params]) []]
			]

	sequence $ dataDec : vecInstance : dotInstance : normInstance : normalizeInstance : numInstance : fractionalInstance : floatingInstance : storableInstance : (map genVecComponentInstance components) ++ swizzleVecInstances ++ combineVecInstances

-- Generate matrix datatypes.
liftM concat $ forM [(i, j) | i <- [1..maxVecDimension], j <- [1..maxVecDimension]] $ \(n, m) -> do
	-- name of data (Mat{n}x{m})
	let dataName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]

	-- name of type parameter
	tvA <- newName "a"
	-- some params
	paramName <- newName "a"
	aParams <- sequence [newName $ ['a', '_', intToDigit i, '_', intToDigit j] | i <- [1..n], j <- [1..m]]
	bParams <- sequence [newName $ ['b', '_', intToDigit i, '_', intToDigit j] | i <- [1..n], j <- [1..m]]

	-- data declaration
	let dataDec = dataD (return []) dataName [PlainTV tvA]
		[ normalC dataName (replicate (n * m) $ return (IsStrict, VarT tvA))
		] [''Eq, ''Ord, ''Show]

	-- instance for Mat class
	let matInstance = instanceD (return []) [t| Mat ($(conT dataName) $(varT tvA)) |]
		[ tySynInstD ''MatElement $ tySynEqn [ [t| $(conT dataName) $(varT tvA) |] ] $ varT tvA
		, funD 'matSize [clause [wildP] (normalB [| ($(litE $ integerL $ toInteger n), $(litE $ integerL $ toInteger m)) |]) []]
		]

	-- instance for Num class
	let numInstance = do
		let binaryOp opName = funD opName
			[ clause
				[ conP dataName $ map varP aParams
				, conP dataName $ map varP bParams
				]
				(normalB $ foldl appE (conE dataName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip aParams bParams)
				[]
			]
		let unaryOp opName = funD opName
			[ clause
				[ conP dataName $ map varP aParams
				]
				(normalB $ foldl appE (conE dataName) $ map (\a -> [| $(varE opName) $(varE a) |]) aParams)
				[]
			]

		let fromIntegerDecl = do
			iParam <- newName "i"
			fiParam <- newName "fi"
			funD 'fromInteger [clause [varP iParam]
				(normalB $ foldl appE (conE dataName) $ replicate (n * m) $ varE fiParam)
				[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

		instanceD (sequence [ [t| Num $(varT paramName) |] ]) [t| Num ($(conT dataName) $(varT paramName)) |]
			[ binaryOp '(+)
			, binaryOp '(*)
			, binaryOp '(-)
			, unaryOp 'negate
			, unaryOp 'abs
			, unaryOp 'signum
			, fromIntegerDecl
			]

	-- instance for Storable class (column-major)
	let storableInstance = do
		p <- newName "p"
		let params = zip [(i, j) | i <- [0..(n - 1)], j <- [0..(m - 1)]] aParams
		instanceD (sequence [ [t| Storable $(varT paramName) |] ]) [t| Storable ($(conT dataName) $(varT paramName)) |]
			[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral (n * m)) * sizeOf (undefined :: $(varT paramName)) |]) []]
			, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $(varT paramName)) |]) []]
			, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * n + i)) |] | ((i, j), a) <- params] ++
				[noBindS [| return $(foldl appE (conE dataName) $ map (varE . snd) params) |]]) []]
			, funD 'poke [clause [varP p, conP dataName $ map (varP . snd) params]
				(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * n + i)) $(varE a) |] | ((i, j), a) <- params]) []]
			]

	sequence $ [dataDec, matInstance, numInstance, storableInstance]

-- Generate multiplications.
let
	dimensions = [1..maxVecDimension]
	dimensions2 = [(n, m) | n <- dimensions, m <- dimensions]
	dimensions3 = [(n, m, k) | n <- dimensions, m <- dimensions, k <- dimensions]
	genVecMatMuls = sequence [genVecMatMul n m | (n, m) <- dimensions2]
	genMatVecMuls = sequence [genMatVecMul n m | (n, m) <- dimensions2]
	genMatMatMuls = sequence [genMatMatMul n m k | (n, m, k) <- dimensions3]

	gen aName bName cName funDecl = do
		eName <- newName "e"
		let eType = varT eName
		instanceD (sequence [ [t| Num $(varT eName) |] ]) [t| Mul ($(conT aName) $eType) ($(conT bName) $eType) |]
			[ tySynInstD ''MulResult $ tySynEqn [ [t| $(conT aName) $eType |], [t| $(conT bName) $eType |] ] [t| $(conT cName) $eType |]
			, funDecl
			]

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
	in liftM concat $ sequence [genVecMatMuls, genMatVecMuls, genMatMatMuls]

-- Generate type synonyms for frequently used types
do
	let vecSynonym n elemType elemChar = let name = "Vec" ++ [intToDigit n] in
		tySynD (mkName $ name ++ [elemChar]) [] [t| $(conT $ mkName name) $elemType |]
	vecSynonyms <- sequence [vecSynonym n (conT t) c | n <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	let matSynonym n m elemType elemChar = let name = "Mat" ++ [intToDigit n, 'x', intToDigit m] in
		tySynD (mkName $ name ++ [elemChar]) [] [t| $(conT $ mkName name) $elemType |]
	matSynonyms <- sequence [matSynonym n m (conT t) c | n <- [1..maxVecDimension], m <- [1..maxVecDimension], (t, c) <- mathTypeNamesWithChar]
	return $ vecSynonyms ++ matSynonyms

-- | Quaternion type.
newtype Quaternion a = Quaternion (Vec4 a)

-- | Cross.
instance Num a => Cross (Vec3 a) where
	cross (Vec3 ax ay az) (Vec3 bx by bz) = Vec3
		(ay * bz - by * az)
		(az * bx - ax * bz)
		(ax * by - ay * bx)
