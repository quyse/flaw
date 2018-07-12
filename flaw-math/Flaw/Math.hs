{-|
Module: Flaw.Math
Description: Math.
License: MIT

The math library is mostly generated by means of Template Haskell.
Every data type is monomorphic, and uses unpacked scalar types.

Derived math structures (vectors and matrices) are only available for
scalar types having 'Vectorized' instance. Similarly quaternion types are
only available for 'Quaternionized' scalar types.

By default vectorized types are provided only for scalar types from
'mathTypeNamesWithPrefix' list, and quaternion types only for types from
'mathQuaternionTypeNamesWithPrefix' list. Vector's dimensions are from 1 to 4.
Matrix dimensions are limited to ones from 'matDimensions' list.

In general, math structures are defined as type families parametrized by scalar type.
Convenient type synonyms and pattern synonyms are defined. As a result:

* At type level you can use either 'Vec4' 'Float' (type family parametrized with scalar type)
or 'Float4' (type synonym for the former).

* At value level you can use either 'Float4' (constructor for type family instance)
or 'Vec4' (bi-directional pattern synonym for the former) for both value construction and pattern matching.

Scalar elements of vectors can be accessed with 'x_', 'y_', etc.
Vector swizzling is available with 'xyz__', etc group of functions.

There's no SIMD support yet.
-}

{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, PatternSynonyms, ScopedTypeVariables, TemplateHaskell, Trustworthy, TypeFamilies, ViewPatterns, UnboxedTuples #-}

module Flaw.Math
	(
	-- * Classes
	  Vec(..)
	, Dot(..)
	, Cross(..)
	, Norm(..)
	, Normalize(..)
	, Mat(..)
	, Mul(..)
	, MatInverse(..)
	-- * Data types
	-- ** Vectors and matrices
	, VecX(..), VecY(..), VecZ(..), VecW(..)
	, Vectorized(..)
	, VectorizedFunctor(..)
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Mat3x3(..), Mat3x4(..), Mat4x4(..)
	-- ** Quaternions
	, Quaternionized(..)
	, Quat(..)
	, Conjugate(..)
	-- * Pattern synonyms
	, pattern Vec1, pattern Vec2, pattern Vec3, pattern Vec4
	, pattern Mat3x3, pattern Mat3x4, pattern Mat4x4
	, pattern Quat
	-- * Vector swizzling
	, SwizzleVecX1(..), SwizzleVecX2(..), SwizzleVecX3(..), SwizzleVecX4(..)
	, SwizzleVecY1(..), SwizzleVecY2(..), SwizzleVecY3(..), SwizzleVecY4(..)
	, SwizzleVecZ1(..), SwizzleVecZ2(..), SwizzleVecZ3(..), SwizzleVecZ4(..)
	, SwizzleVecW1(..), SwizzleVecW2(..), SwizzleVecW3(..), SwizzleVecW4(..)
	-- * Type synonyms
	, Float1, Float2, Float3, Float4
	, Double1, Double2, Double3, Double4
	, Int32_1, Int32_2, Int32_3, Int32_4
	, Word32_1, Word32_2, Word32_3, Word32_4
	, Int1, Int2, Int3, Int4
	, Int8_1, Int8_2, Int8_3, Int8_4
	, Word8_1, Word8_2, Word8_3, Word8_4
	, Float3x3, Float3x4, Float4x4
	, Double3x3, Double3x4, Double4x4
	, Int32_3x3, Int32_3x4, Int32_4x4
	, Word32_3x3, Word32_3x4, Word32_4x4
	, Int3x3, Int3x4, Int4x4
	, Int8_3x3, Int8_3x4, Int8_4x4
	, Word8_3x3, Word8_3x4, Word8_4x4
	, FloatQ, DoubleQ
	-- * Base definitions
	, maxVecDimension
	, vecComponents
	) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Default
import Data.List
import Data.Maybe
import qualified Data.Serialize as S
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics(Generic)
import Language.Haskell.TH

import Flaw.Math.Internal

-- | General vector class.
class Vec (v :: *) where
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
	let methodName = mkName [c, '_']
	tvV <- newName "v"
	classD (return [AppT (ConT $ mkName "Vec") $ VarT tvV]) className [PlainTV tvV] []
		[ sigD methodName [t| $(varT tvV) -> VecElement $(varT tvV) |]
		]

-- | Generate class Vectorized.
{-
class Vectorized (a :: *) where
	data Vec{1234} a :: *
	vec{n=1234} :: a...{n} -> Vec{n} a
	unvec{n=1234} :: Vec{n} a -> (# a...{n} #)
	data Mat{1234}x{1234} a :: *
	mat{n=1234}x{m=1234} :: a...{n*m} -> Mat{n}x{m} a
	unmat{n=1234}x{m=1234} :: Mat{n}x{m} a -> (# a...{n*m} #)
-}
fmap return $ do
	tvA <- newName "a"
	vecDecs <- fmap concat $ forM [1..maxVecDimension] $ \dim -> do
		let dimStr = [intToDigit dim]
		let dataName = mkName $ "Vec" ++ dimStr
		let dataDec = dataFamilyD dataName [PlainTV tvA] (Just StarT)
		let packFuncDec = sigD (mkName $ "vec" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) [t| $(conT dataName) $(varT tvA) |] $ replicate dim $ varT tvA
		let unpackFuncDec = sigD (mkName $ "unvec" ++ dimStr) [t| $(conT dataName) $(varT tvA) -> $(if dim == 1 then varT tvA else foldl appT (unboxedTupleT dim) $ replicate dim $ varT tvA) |]
		return [dataDec, packFuncDec, unpackFuncDec]
	matDecs <- fmap concat $ forM matDimensions $ \(dimN, dimM) -> do
		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataName = mkName $ "Mat" ++ dimStr
		let dataDec = dataFamilyD dataName [PlainTV tvA] (Just StarT)
		let packFuncDec = sigD (mkName $ "mat" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) (appT (conT dataName) (varT tvA)) $ replicate (dimN * dimM) $ varT tvA
		let unpackFuncDec = sigD (mkName $ "unmat" ++ dimStr) [t| $(conT dataName) $(varT tvA) -> $(foldl appT (unboxedTupleT (dimN * dimM)) $ replicate (dimN * dimM) $ varT tvA) |]
		return [dataDec, packFuncDec, unpackFuncDec]
	classD (sequence [ [t| Ord $(varT tvA) |], [t| Show $(varT tvA) |] ]) (mkName "Vectorized") [KindedTV tvA StarT] [] $ vecDecs ++ matDecs

-- | Special functor class over Vectorized elements.
class VectorizedFunctor (f :: * -> *) where
	-- | Apply function to elements of functor.
	vecfmap :: (Vectorized a, Vectorized b) => (a -> b) -> f a -> f b

-- Pattern synonyms for vectors and matrices, allowing to construct/deconstruct values using generalized names.
-- vectors
fmap concat . forM [1..maxVecDimension] $ \dim -> do
	let v = mkName $ "Vec" <> show dim
	a <- newName "a"
	comps <- mapM (newName . pure) $ take dim vecComponents
	sequence
		[ patSynSigD v (forallT [PlainTV a] (sequence [(conT ''Vectorized) `appT` (varT a)]) $ foldr (appT . (appT arrowT) . varT) ((conT v) `appT` (varT a)) (replicate dim a))
		, patSynD v (prefixPatSyn comps)
			(explBidir [clause (map varP comps) (normalB $ foldl appE (varE (mkName $ "vec" <> show dim)) $ map varE comps) []])
			(viewP (varE (mkName $ "unvec" <> show dim)) (if dim == 1 then varP (head comps) else unboxedTupP (map varP comps)))
		]
-- matrices
fmap concat . forM matDimensions $ \(dimN, dimM) -> do
	let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
	let v = mkName $ "Mat" <> dimStr
	a <- newName "a"
	comps <- mapM newName [['m', intToDigit n, intToDigit m] | n <- [1..dimN], m <- [1..dimM]]
	sequence
		[ patSynSigD v (forallT [PlainTV a] (sequence [(conT ''Vectorized) `appT` (varT a)]) $ foldr (appT . (appT arrowT) . varT) ((conT v) `appT` (varT a)) (replicate (dimN * dimM) a))
		, patSynD v (prefixPatSyn comps)
			(explBidir [clause (map varP comps) (normalB $ foldl appE (varE (mkName $ "mat" <> dimStr)) $ map varE comps) []])
			(viewP (varE (mkName $ "unmat" <> dimStr)) (unboxedTupP (map varP comps)))
		]

-- | Class for dot operation.
class Vec v => Dot v where
	dot :: v -> v -> VecElement v

-- | Class for cross operation.
class Cross (v :: *) where
	cross :: v -> v -> v

-- | Class for norm operation.
class Vec v => Norm v where
	norm :: v -> VecElement v
	norm2 :: v -> VecElement v

-- | Class for normalize operation.
class Normalize (v :: *) where
	normalize :: v -> v

-- | General matrix class.
class Mat (m :: *) where
	type MatElement m :: *
	-- | Get matrix size.
	matSize :: m -> (Int, Int) -- m is unused
	-- | Create matrix from scalar (put scalar into every component).
	matFromScalar :: MatElement m -> m

-- | Class for general multiplication.
class Mul (a :: *) (b :: *) where
	type MulResult a b :: *
	mul :: a -> b -> MulResult a b

-- | Class for matrix inversion.
class MatInverse (a :: *) where
	matInverse :: a -> a

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
	let genSig variant =
		sigD (mkName $ variant ++ "__") [t| $(varT tvV) -> $(conT resultTypeName) $(varT tvV) |]
	classD (sequence [ [t| $(conT $ mkName $ "Vec" ++ [toUpper c]) $(varT tvV) |] | c <- components])
		className [PlainTV tvV] [] $ openTypeFamilyD resultTypeName [PlainTV tvV] (KindSig StarT) Nothing : map genSig variants

-- Things per math type.
fmap concat $ mapM (uncurry mathTypeVectorizedDecls) mathTypeNamesWithPrefix

-- Abstract instances for vector and matrix types.
do
	tvE <- newName "e"
	let elemType = varT tvE

	-- vector declarations
	vecDecs <- fmap concat $ forM [1..maxVecDimension] $ \dim -> do

		let dimStr = [intToDigit dim]
		let dataName = mkName $ "Vec" ++ dimStr
		let conName = mkName $ "Vec" ++ dimStr -- using pattern synonym

		-- string with symbols of components, like "xyz"
		let components = take dim vecComponents
		-- names for component-parameters
		componentParams <- forM components $ \c -> newName [c]
		as <- forM components $ \c -> newName ['a', c]
		bs <- forM components $ \c -> newName ['b', c]
		p <- newName "p"

		-- instance for Vec class
		vecInstance <- instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| Vec ($(conT dataName) $elemType) |] =<< addInlines
			[ tySynInstD ''VecElement $ tySynEqn [ [t| $(conT dataName) $elemType |] ] elemType
			, funD 'vecLength [clause [wildP] (normalB $ litE $ integerL $ fromIntegral dim) []]
			, funD 'vecToList [clause [conP conName $ map varP componentParams] (normalB $ listE $ map varE componentParams) []]
			, funD 'vecFromScalar [clause [varP p] (normalB $ foldl appE (conE conName) $ replicate dim (varE p)) []]
			]

		-- instances for VecX .. VecW classes
		vecComponentInstances <- forM components $ \component -> do
			let className = mkName $ "Vec" ++ [toUpper component]
			let funName = mkName [component, '_']
			varName <- newName [component]
			instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| $(conT className) ($(conT dataName) $elemType) |] =<< addInlines
				[ funD funName [clause [conP conName [if c == component then varP varName else wildP | c <- components]] (normalB (varE varName)) []]
				]

		-- instance for VectorizedFunctor class
		vectorizedFunctorInstance <- instanceD (sequence []) [t| VectorizedFunctor $(conT dataName) |] =<< addInlines
			[ funD 'vecfmap [clause [varP p, conP conName $ map varP as] (normalB $ foldl appE (conE conName) $ map (appE (varE p) . varE) as) []]
			]

		-- instance for Dot class
		dotInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Dot ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'dot
				[ clause
					[ conP conName $ map varP as
					, conP conName $ map varP bs
					]
					(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\(a, b) -> [| $(varE a) * $(varE b) |]) $ zip as bs) []
				]
			]

		-- instance for Norm class
		normInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Floating $elemType |] ]) [t| Norm ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'norm [clause [] (normalB $ [| sqrt . norm2 |]) []]
			, funD 'norm2 [clause [conP conName $ map varP as]
				(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map ((\a -> [| $a * $a |]) . varE) as) []]
			]

		-- instance for Normalize class
		normalizeInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Floating $elemType |] ]) [t| Normalize ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'normalize [clause [varP p] (normalB [| $(varE p) * vecFromScalar (1 / norm $(varE p)) |]) []]
			]

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
				let expr = foldl (\v c -> appE v [| $(varE (mkName [c, '_'])) $(varE tvV) |]) (conE conName) variant
				funD (mkName $ variant ++ "__") [clause [varP tvV] (normalB expr) []]
			instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| $(conT instanceName) ($(conT srcDataName) $elemType) |] =<< addInlines (resultDecl : map funDecl variants)

		let binaryOp opName = funD opName
			[ clause
				[ conP conName $ map varP as
				, conP conName $ map varP bs
				]
				(normalB $ foldl appE (conE conName) $ map (\(a, b) -> [| $(varE opName) $(varE a) $(varE b) |]) $ zip as bs)
				[]
			]
		let unaryOp opName = funD opName
			[ clause
				[ conP conName $ map varP as
				]
				(normalB $ foldl appE (conE conName) $ map (\a -> [| $(varE opName) $(varE a) |]) as)
				[]
			]
		let nullaryOp opName = funD opName
			[ clause []
				(normalB $ foldl appE (conE conName) $ map (\_ -> varE opName) as)
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

			instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Num ($(conT dataName) $elemType) |] =<< addInlines
				[ binaryOp '(+)
				, binaryOp '(*)
				, binaryOp '(-)
				, unaryOp 'negate
				, unaryOp 'abs
				, unaryOp 'signum
				, fromIntegerDecl
				]

		-- instance for Fractional class
		fractionalInstance <- do
			let fromRationalDecl = do
				rParam <- newName "r"
				frParam <- newName "fr"
				funD 'fromRational [clause [varP rParam]
					(normalB $ foldl appE (conE conName) $ replicate dim $ varE frParam)
					[valD (varP frParam) (normalB [| fromRational $(varE rParam) |]) []]]
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Fractional $elemType |] ]) [t| Fractional ($(conT dataName) $elemType) |] =<< addInlines
				[ binaryOp '(/)
				, unaryOp 'recip
				, fromRationalDecl
				]

		-- instance for Floating class
		floatingInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Floating $elemType |] ]) [t| Floating ($(conT dataName) $elemType) |] =<< addInlines (concat
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
			])

		-- instance for Storable class
		storableInstance <- do
			let params = zip [0..(dim - 1)] as
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Storable $elemType |] ]) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral dim) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) |] | (i, a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) $(varE a) |] | (i, a) <- params]) []]
				]

		-- Eq instance
		eqInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Eq $elemType |] ]) [t| Eq ($(conT dataName) $elemType) |] =<< addInlines
			[ funD '(==) [clause [conP conName $ map varP as, conP conName $ map varP bs] (normalB $ foldl1 (\a b -> [| $a && $b |]) $ map (\(a, b) -> [| $(varE a) == $(varE b) |]) $ zip as bs) []]
			]

		-- Ord instance
		ordInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Ord $elemType |] ]) [t| Ord ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'compare [clause [conP conName $ map varP as, conP conName $ map varP bs] (normalB $ foldr ($) [| EQ |] $ map (\(a, b) c ->
				[| case compare $(varE a) $(varE b) of
					EQ -> $c
					r -> r
					|]) $ zip as bs) []]
			]

		-- Show instance
		{- Example:
		showsPrec p (Vec4 x y z w) q = if p >= 10 then '(' : s (')' : q) else s q where
			s h = "Vec4" ++ f x (f y (f z (f w h)))
			f t h = ' ' : (showsPrec 10 t h)
		-}
		showInstance <- do
			q <- newName "q"
			s <- newName "s"
			f <- newName "f"
			h <- newName "h"
			t <- newName "t"
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Show $elemType |] ]) [t| Show ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'showsPrec [clause [varP p, conP conName $ map varP as, varP q] (normalB [| if $(varE p) >= 10 then '(' : $(varE s) (')' : $(varE q)) else $(varE s) $(varE q) |])
					[ funD s [clause [varP h] (normalB [| $(litE $ stringL $ "Vec" ++ dimStr) ++ $(foldr (appE . appE (varE f) . varE) (varE h) as) |]) []]
					, funD f [clause [varP t, varP h] (normalB [| ' ' : showsPrec 10 $(varE t) $(varE h) |]) []]
					]]
				]

		-- Serialize instance
		serializeInstance <-
			instanceD (sequence [ [t| Vectorized $elemType |], [t| S.Serialize $elemType |] ]) [t| S.Serialize ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'S.put [clause [conP conName $ map varP as] (normalB $ doE $ map (\a -> noBindS [| S.put $(varE a) |]) as) []]
				, funD 'S.get [clause [] (normalB $ doE $ map (\a -> bindS (varP a) [| S.get |]) as ++ [noBindS $ [| return $(foldl appE (conE conName) $ map varE as) |] ]) []]
				]

		-- Default instance
		defaultInstance <-
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Default $elemType |] ]) [t| Default ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'def [clause [] (normalB $ foldl appE (conE conName) $ replicate dim [| def |]) []]
				]

		return $ vecInstance : vectorizedFunctorInstance : dotInstance : numInstance : normInstance : normalizeInstance : fractionalInstance : floatingInstance : storableInstance : eqInstance : ordInstance : showInstance : serializeInstance : defaultInstance :
			vecComponentInstances ++ swizzleVecInstances

	-- Cross instance
	crossInstance <- do
		names@[ax, ay, az, bx, by, bz] <- mapM newName ["ax", "ay", "az", "bx", "by", "bz"]
		let [axe, aye, aze, bxe, bye, bze] = map varE names
		instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Cross (Vec3 $elemType) |] =<< addInlines
			[ funD 'cross [clause [conP 'Vec3 [varP ax, varP ay, varP az], conP 'Vec3 [varP bx, varP by, varP bz]] (normalB
				[| Vec3
					($aye * $bze - $aze * $bye)
					($aze * $bxe - $axe * $bze)
					($axe * $bye - $aye * $bxe)
					|]) [] ]
			]

	-- matrix declarations
	matDecs <- fmap concat $ forM matDimensions $ \(dimN, dimM) -> do

		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataName = mkName $ "Mat" ++ dimStr
		let conName = mkName $ "Mat" ++ dimStr -- using pattern synonym

		-- some params
		as <- mapM newName [['a', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]
		bs <- mapM newName [['b', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]
		p <- newName "p"

		-- Mat instance
		matInstance <- instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| Mat ($(conT dataName) $elemType) |] =<< addInlines
			[ tySynInstD ''MatElement $ tySynEqn [ [t| $(conT dataName) $elemType |] ] elemType
			, funD 'matSize [clause [wildP] (normalB [| ($(litE $ integerL $ toInteger dimN), $(litE $ integerL $ toInteger dimM)) |]) []]
			, funD 'matFromScalar [clause [varP p] (normalB $ foldl appE (conE conName) $ replicate (dimN * dimM) (varE p)) []]
			]

		-- Num instance
		numInstance <- do
			let binaryOp opName = funD opName
				[ clause
					[ conP conName $ map varP as
					, conP conName $ map varP bs
					]
					(normalB $ foldl appE (conE conName) $ map (\(a, b) -> infixApp (varE a) (varE opName) (varE b)) $ zip as bs)
					[]
				]
			let unaryOp opName = funD opName
				[ clause
					[ conP conName $ map varP as
					]
					(normalB $ foldl appE (conE conName) $ map (\a -> [| $(varE opName) $(varE a) |]) as)
					[]
				]
			let fromIntegerDecl = do
				iParam <- newName "i"
				fiParam <- newName "fi"
				funD 'fromInteger [clause [varP iParam]
					(normalB $ foldl appE (conE conName) $ replicate (dimN * dimM) $ varE fiParam)
					[valD (varP fiParam) (normalB [| fromInteger $(varE iParam) |]) []]]

			instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Num ($(conT dataName) $elemType) |] =<< addInlines
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
			let params = zip [(i, j) | i <- [0..(dimN - 1)], j <- [0..(dimM - 1)]] as
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Storable $elemType |] ]) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral (dimN * dimM)) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) |] | ((i, j), a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) $(varE a) |] | ((i, j), a) <- params]) []]
				]

		-- Eq instance
		eqInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Eq $elemType |] ]) [t| Eq ($(conT dataName) $elemType) |] =<< addInlines
			[ funD '(==) [clause [conP conName $ map varP as, conP conName $ map varP bs] (normalB $ foldl1 (\a b -> [| $a && $b |]) $ map (\(a, b) -> [| $(varE a) == $(varE b) |]) $ zip as bs) []]
			]

		-- Ord instance
		ordInstance <- instanceD (sequence [ [t| Vectorized $elemType |], [t| Ord $elemType |] ]) [t| Ord ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'compare [clause [conP conName $ map varP as, conP conName $ map varP bs] (normalB $ foldr ($) [| EQ |] $ map (\(a, b) c ->
				[| case compare $(varE a) $(varE b) of
					EQ -> $c
					r -> r
					|]) $ zip as bs) []]
			]

		-- Show instance
		showInstance <- do
			q <- newName "q"
			s <- newName "s"
			f <- newName "f"
			h <- newName "h"
			t <- newName "t"
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Show $elemType |] ]) [t| Show ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'showsPrec [clause [varP p, conP conName $ map varP as, varP q] (normalB [| if $(varE p) >= 10 then '(' : $(varE s) (')' : $(varE q)) else $(varE s) $(varE q) |])
					[ funD s [clause [varP h] (normalB [| $(litE $ stringL $ "Mat" ++ dimStr) ++ $(foldr (appE . appE (varE f) . varE) (varE h) as) |]) []]
					, funD f [clause [varP t, varP h] (normalB [| ' ' : showsPrec 10 $(varE t) $(varE h) |]) []]
					]]
				]

		-- Serialize instance
		serializeInstance <-
			instanceD (sequence [ [t| Vectorized $elemType |], [t| S.Serialize $elemType |] ]) [t| S.Serialize ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'S.put [clause [conP conName $ map varP as] (normalB $ doE $ map (\a -> noBindS [| S.put $(varE a) |]) as) []]
				, funD 'S.get [clause [] (normalB $ doE $ map (\a -> bindS (varP a) [| S.get |]) as ++ [noBindS $ [| return $(foldl appE (conE conName) $ map varE as) |] ]) []]
				]

		return [matInstance, numInstance, storableInstance, eqInstance, ordInstance, showInstance, serializeInstance]

	-- Generate multiplications.
	mulInstances <- do
		let gen aName bName cName funDecl = instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Mul ($(conT aName) $elemType) ($(conT bName) $elemType) |] =<< addInlines
			[ tySynInstD ''MulResult $ tySynEqn [ [t| $(conT aName) $elemType |], [t| $(conT bName) $elemType |] ] [t| $(conT cName) $elemType |]
			, funDecl
			]

		let genVecMatMul (n, m) = do
			let aName = mkName $ "Vec" ++ [intToDigit n]
			let aConName = aName -- using pattern synonym
			let bName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit m]
			let bConName = bName -- using pattern synonym
			let cName = mkName $ "Vec" ++ [intToDigit m]
			let cConName = cName -- using pattern synonym
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
			let aConName = aName -- using pattern synonym
			let bName = mkName $ "Vec" ++ [intToDigit m]
			let bConName = bName -- using pattern synonym
			let cName = mkName $ "Vec" ++ [intToDigit n]
			let cConName = cName -- using pattern synonym
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
			let aConName = aName -- using pattern synonym
			let bName = mkName $ "Mat" ++ [intToDigit m, 'x', intToDigit k]
			let bConName = bName -- using pattern synonym
			let cName = mkName $ "Mat" ++ [intToDigit n, 'x', intToDigit k]
			let cConName = cName -- using pattern synonym
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
			[(m, k) | n == m]
		matVecMuls <- mapM genMatVecMul $ do
			(n, m) <- matDimensions
			k <- [1..maxVecDimension]
			[(n, m) | m == k]
		matMatMuls <- mapM genMatMatMul $ do
			(n, m1) <- matDimensions
			(m2, k) <- matDimensions
			[(n, m1, k) | m1 == m2]
		
		return $ concat [vecMatMuls, matVecMuls, matMatMuls]

	-- matrix inversions
	matInverseInstances <- forM (map fst $ filter (uncurry (==)) matDimensions) $ \dim -> do
		mNames <- mapM newName ["m" ++ show i ++ "_" ++ show j | i <- [1..dim], j <- [1..dim]]
		let
			dataName = mkName $ "Mat" ++ show dim ++ "x" ++ show dim
			-- full mask
			dimMask = (1 `shiftL` dim) - 1
			-- name for value of minor
			detName imask jmask = case (maskBits imask, maskBits jmask) of
				([i], [j]) -> mNames !! (i * dim + j)
				_ -> mkName $ "det" ++ show (imask :: Int) ++ "_" ++ show (jmask :: Int)
			-- get list of bits from mask
			maskBits mask = if mask == 0 then [] else let smask = (mask - 1) .&. mask in maskIndex (smask `xor` mask) : maskBits smask
			-- get index of one-bit mask
			maskIndex mask = fromJust $ elemIndex mask [1 `shiftL` a | a <- [0..(dim - 1)]]
			-- determinant expression
			detExp imask jmask = let
				(i : ribits) = maskBits imask
				jbits@(j : rjbits) = maskBits jmask
				subDets = detName (imask `xor` (1 `shiftL` i)) <$> map ((jmask `xor`) . (1 `shiftL`)) jbits
				alternateSign = zipWith ($) (cycle [id, \e -> [| negate $e |] ])
				subElems = alternateSign $ map (\jj -> varE $ mNames !! (i * dim + jj)) jbits
				subDetsElems = map (\(a, b) -> [| $(varE a) * $b |]) $ zip subDets subElems
				in if null ribits && null rjbits then varE $ mNames !! (i * dim + j) else foldl1 (\a b -> [| $a + $b |]) subDetsElems
			-- adjugate matrix expression
			adjExp = foldl appE (conE dataName)
				[ if even (i + j) then e else [| negate $e |]
				| i <- [0..(dim - 1)]
				, j <- [0..(dim - 1)]
				, let e = varE $ detName (dimMask `xor` (1 `shiftL` j)) (dimMask `xor` (1 `shiftL` i))
				]
			-- inverse matrix expression
			invMatExp = [| $adjExp * matFromScalar (1 / $(varE $ detName dimMask dimMask)) |]
			-- decls for all determinants
			detDecls =
				[ valD (varP (detName imask jmask)) (normalB $ detExp imask jmask) []
				| imask <- [1..dimMask]
				, jmask <- [1..dimMask]
				, let l = length (maskBits imask)
				, l > 1 && ((1 `shiftL` max 0 (dim - l - 1)) - 1) .&. imask == 0 && l == length (maskBits jmask)
				]

		instanceD (sequence [ [t| Vectorized $elemType |], [t| Fractional $elemType |] ]) [t| MatInverse ($(conT dataName) $elemType) |] =<< addInlines
			[ funD 'matInverse [clause [conP dataName $ map varP mNames] (normalB invMatExp) detDecls]
			]

	return $ crossInstance : vecDecs ++ matDecs ++ mulInstances ++ matInverseInstances

-- | Class of things which has quaternions.
class (Vectorized a, Floating a) => Quaternionized a where
	data Quat a :: *
	quat :: Vec4 a -> Quat a
	unquat :: Quat a -> Vec4 a

pattern Quat :: Quaternionized a => Vec4 a -> Quat a
pattern Quat v <- (unquat -> v) where Quat v = quat v

-- | Conjugation.
class Conjugate (q :: *) where
	conjugate :: q -> q

-- Generate per-math type declarations for quaternions.
fmap concat $ forM mathQuaternionTypeNamesWithPrefix $ \(mathTypeName, mathTypePrefix) -> do

	let elemType = conT mathTypeName
	let conName = mkName $ mathTypePrefix ++ "Q"
	v <- newName "v"

	-- Quaternionized instance
	quaternionizedInstance <- instanceD (sequence []) [t| Quaternionized $elemType |] =<< addInlines
		[ newtypeInstD (sequence []) ''Quat [elemType] Nothing (normalC conName [fmap (\t -> (Bang NoSourceUnpackedness NoSourceStrictness, t)) [t| Vec4 $elemType |]]) [derivClause Nothing [ [t| Generic |] ] ]
		, funD 'quat [clause [varP v] (normalB [| $(conE conName) $(varE v) |]) []]
		, funD 'unquat [clause [conP conName [varP v]] (normalB $ varE v) []]
		]

	-- synonym
	synonym <- tySynD conName [] [t| Quat $elemType |]

	return [quaternionizedInstance, synonym]

-- Abstract declarations for quaternions.
do
	tvE <- newName "e"
	let elemType = varT tvE
	a <- newName "a"
	v <- newName "v"
	av <- newName "av"
	bv <- newName "bv"
	as <- forM vecComponents $ \c -> newName ['a', c]
	bs <- forM vecComponents $ \c -> newName ['b', c]
	let [ax, ay, az, aw] = map varE as
	let [bx, by, bz, bw] = map varE bs

	-- Vec instance
	vecInstance <- instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Vec (Quat $elemType) |] =<< addInlines
		[ tySynInstD ''VecElement $ tySynEqn [ [t| Quat $elemType |] ] elemType
		, funD 'vecLength [clause [conP 'Quat [varP v]] (normalB [| vecLength $(varE v) |]) []]
		, funD 'vecToList [clause [conP 'Quat [varP v]] (normalB [| vecToList $(varE v) |]) []]
		, funD 'vecFromScalar [clause [varP a] (normalB [| Quat (vecFromScalar $(varE a)) |]) []]
		]

	-- Norm instance
	normInstance <- instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Norm (Quat $elemType) |] =<< addInlines
		[ funD 'norm [clause [conP 'Quat [varP v]] (normalB [| norm $(varE v) |]) []]
		, funD 'norm2 [clause [conP 'Quat [varP v]] (normalB [| norm2 $(varE v) |]) []]
		]

	-- Normalize instance
	normalizeInstance <- instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Normalize (Quat $elemType) |] =<< addInlines
		[ funD 'normalize [clause [conP 'Quat [varP v]] (normalB [| Quat (normalize $(varE v)) |]) []]
		]

	-- Num instance
	numInstance <-
		instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Num (Quat $elemType) |] =<< addInlines
			[ funD '(+) [clause [conP 'Quat [varP av], conP 'Quat [varP bv]] (normalB [| Quat ($(varE av) + $(varE bv)) |]) []]
			, funD '(-) [clause [conP 'Quat [varP av], conP 'Quat [varP bv]] (normalB [| Quat ($(varE av) - $(varE bv)) |]) []]
			, funD '(*) [clause
				[ conP 'Quat [conP 'Vec4 $ map varP as]
				, conP 'Quat [conP 'Vec4 $ map varP bs]
				] (normalB [| Quat (Vec4
					($aw * $bx + $ax * $bw + $ay * $bz - $az * $by)
					($aw * $by - $ax * $bz + $ay * $bw + $az * $bx)
					($aw * $bz + $ax * $by - $ay * $bx + $az * $bw)
					($aw * $bw - $ax * $bx - $ay * $by - $az * $bz)
					)|]) []]
			, funD 'negate [clause [conP 'Quat [varP v]] (normalB [| Quat (negate $(varE v)) |]) []]
			, funD 'abs [clause [conP 'Quat [varP v]] (normalB [| Quat (abs $(varE v)) |]) []]
			, funD 'signum [clause [] (normalB [| undefined |]) []]
			, funD 'fromInteger [clause [] (normalB [| undefined |]) []]
			]

	-- Conjugate instance
	conjugateInstance <- instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Conjugate (Quat $elemType) |] =<< addInlines
		[ funD 'conjugate [clause [conP 'Quat [conP 'Vec4 $ map varP as]] (normalB [| Quat (Vec4 (- $ax) (- $ay) (- $az) $aw) |]) []]
		]

	-- Storable instance
	storableInstance <-
		instanceD (sequence [ [t| Quaternionized $elemType |], [t| Storable $elemType |] ]) [t| Storable (Quat $elemType) |] =<< addInlines
			[ funD 'sizeOf [clause [wildP] (normalB [| sizeOf (undefined :: Vec4 $elemType) |]) []]
			, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: Vec4 $elemType) |]) []]
			, funD 'peek [clause [varP a] (normalB [| Quat <$> peek (castPtr $(varE a)) |]) []]
			, funD 'poke [clause [varP a, conP 'Quat [varP av]] (normalB [| poke (castPtr $(varE a)) $(varE av) |]) []]
			]

	-- Eq instance
	eqInstance <- instanceD (sequence [ [t| Quaternionized $elemType |], [t| Eq $elemType |] ]) [t| Eq (Quat $elemType) |] =<< addInlines
		[ funD '(==) [clause [conP 'Quat [varP av], conP 'Quat [varP bv]] (normalB [| $(varE av) == $(varE bv) |]) []]
		]

	-- Ord instance
	ordInstance <- instanceD (sequence [ [t| Quaternionized $elemType |], [t| Ord $elemType |] ]) [t| Ord (Quat $elemType) |] =<< addInlines
		[ funD 'compare [clause [conP 'Quat [varP av], conP 'Quat [varP bv]] (normalB [| compare $(varE av) $(varE bv) |]) []]
		]

	-- Show instance
	{- Example:
	showsPrec p (Quat v) q = if p >= 10 then '(' : s (')' : q) else s q where
		s h = "Quat " ++ showsPrec 10 v h
	-}
	showInstance <- do
		p <- newName "p"
		q <- newName "q"
		s <- newName "s"
		h <- newName "h"
		instanceD (sequence [ [t| Quaternionized $elemType |], [t| Show $elemType |] ]) [t| Show (Quat $elemType) |] =<< addInlines
			[ funD 'showsPrec [clause [varP p, conP 'Quat [varP av], varP q] (normalB [| if $(varE p) >= 10 then '(' : $(varE s) (')' : $(varE q)) else $(varE s) $(varE q) |])
				[ funD s [clause [varP h] (normalB [| "Quat " ++ showsPrec 10 $(varE av) $(varE h) |]) []]
				]]
			]

	-- Serialize instance
	serializeInstance <-
		instanceD (sequence [ [t| Quaternionized $elemType |], [t| S.Serialize $elemType |] ]) [t| S.Serialize (Quat $elemType) |] =<< addInlines
			[ funD 'S.put [clause [conP 'Quat [varP av]] (normalB [| S.put $(varE av) |]) []]
			, funD 'S.get [clause [] (normalB [| Quat <$> S.get |]) []]
			]

	return [vecInstance, normInstance, normalizeInstance, numInstance, conjugateInstance, storableInstance, eqInstance, ordInstance, showInstance, serializeInstance]
