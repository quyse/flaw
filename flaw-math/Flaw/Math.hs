{-|
Module: Flaw.Math
Description: Math.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms, ScopedTypeVariables, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Flaw.Math
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, VecX(..), VecY(..), VecZ(..), VecW(..)
	, Vectorized(..)
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Mat3x4(..), Mat4x4(..)
	, pattern Vec1, pattern Vec2, pattern Vec3, pattern Vec4
	, pattern Mat3x4, pattern Mat4x4
	, pattern Quat
	, SwizzleVecX1(..), SwizzleVecX2(..), SwizzleVecX3(..), SwizzleVecX4(..)
	, SwizzleVecY1(..), SwizzleVecY2(..), SwizzleVecY3(..), SwizzleVecY4(..)
	, SwizzleVecZ1(..), SwizzleVecZ2(..), SwizzleVecZ3(..), SwizzleVecZ4(..)
	, SwizzleVecW1(..), SwizzleVecW2(..), SwizzleVecW3(..), SwizzleVecW4(..)
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
	unvec{n=1234} :: Vec{n} a -> (a...{n})
	data Mat{1234}x{1234} a :: *
	mat{n=1234}x{m=1234} :: a...{n*m} -> Mat{n}x{m} a
	unmat{n=1234}x{m=1234} :: Mat{n}x{m} a -> (a...{n*m})
-}
liftM return $ do
	tvA <- newName "a"
	vecDecs <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do
		let dimStr = [intToDigit dim]
		let dataName = mkName $ "Vec" ++ dimStr
		let dataDec = familyKindD dataFam dataName [PlainTV tvA] StarT
		let packFuncDec = sigD (mkName $ "vec" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) [t| $(conT dataName) $(varT tvA) |] $ replicate dim $ varT tvA
		let unpackFuncDec = sigD (mkName $ "unvec" ++ dimStr) [t| $(conT dataName) $(varT tvA) -> $(foldl appT (tupleT dim) $ replicate dim $ varT tvA) |]
		return [dataDec, packFuncDec, unpackFuncDec]
	matDecs <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do
		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataName = mkName $ "Mat" ++ dimStr
		let dataDec = familyKindD dataFam dataName [PlainTV tvA] StarT
		let packFuncDec = sigD (mkName $ "mat" ++ dimStr) $ foldr (\a b -> [t| $a -> $b |]) (appT (conT dataName) (varT tvA)) $ replicate (dimN * dimM) $ varT tvA
		let unpackFuncDec = sigD (mkName $ "unmat" ++ dimStr) [t| $(conT dataName) $(varT tvA) -> $(foldl appT (tupleT (dimN * dimM)) $ replicate (dimN * dimM) $ varT tvA) |]
		return [dataDec, packFuncDec, unpackFuncDec]
	classD (sequence []) (mkName "Vectorized") [PlainTV tvA] [] $ vecDecs ++ matDecs

-- Pattern synonyms for vectors and matrices.
-- TH doesn't support pattern synonyms yet (https://ghc.haskell.org/trac/ghc/ticket/8761), so doing it manually :(
pattern Vec1 x <- (unvec1 -> x) where Vec1 x = vec1 x
pattern Vec2 x y <- (unvec2 -> (x, y)) where Vec2 x y = vec2 x y
pattern Vec3 x y z <- (unvec3 -> (x, y, z)) where Vec3 x y z = vec3 x y z
pattern Vec4 x y z w <- (unvec4 -> (x, y, z, w)) where Vec4 x y z w = vec4 x y z w
pattern Mat3x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34
	<- (unmat3x4 -> (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34)) where
	Mat3x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 = mat3x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34
pattern Mat4x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44
	<- (unmat4x4 -> (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44)) where
	Mat4x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44 = mat4x4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44

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

-- Things per math type.
liftM concat $ forM mathTypeNamesWithPrefix $ \(mathTypeName, mathTypePrefix) -> do

	let elemType = conT mathTypeName

	-- Vectorized instance
	vectorizedInstance <- do

		-- vector things
		vecDecs <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do
			let dimStr = [intToDigit dim]
			let dataName = mkName $ "Vec" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM (take dim vecComponents) $ newName . return
			return
				[ dataInstD (sequence []) dataName [elemType] [normalC conName (replicate dim $ return (Unpacked, ConT mathTypeName))] [''Eq, ''Ord, ''Show]
				, funD (mkName $ "vec" ++ dimStr) [clause (map varP components) (normalB $ foldl appE (conE conName) $ map varE components) []]
				, funD (mkName $ "unvec" ++ dimStr) [clause [conP conName $ map varP components] (normalB $ tupE $ map varE components) []]
				]

		-- matrix things
		matDecs <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do
			let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
			let dataName = mkName $ "Mat" ++ dimStr
			let conName = mkName $ mathTypePrefix ++ dimStr
			components <- forM [(i, j) | i <- [1..dimN], j <- [1..dimM]] $ \(i, j) -> newName ['m', intToDigit i, intToDigit j]
			return
				[ dataInstD (sequence []) dataName [elemType] [normalC conName (replicate (dimN * dimM) $ return (Unpacked, ConT mathTypeName))] [''Eq, ''Ord, ''Show]
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

-- Abstract instances for vector and matrix types.
do
	tvE <- newName "e"
	let elemType = varT tvE

	-- instances per vector dimension, like VecX (Vec4 Float)
	vecInstances <- liftM concat $ forM [1..maxVecDimension] $ \dim -> do

		let dimStr = [intToDigit dim]
		let dataName = mkName $ "Vec" ++ dimStr
		let conName = mkName $ "Vec" ++ dimStr -- using pattern synonym

		-- string with symbols of components, like "xyz"
		let components = take dim vecComponents
		-- names for component-parameters
		componentParams <- forM components $ \c -> newName [c]

		-- instance for Vec class
		vecInstance <- do
			let vecFromScalarDecl = do
				a <- newName "a"
				funD 'vecFromScalar [clause [varP a] (normalB $ foldl appE (conE conName) $ map varE $ replicate dim a) []]
			instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| Vec ($(conT dataName) $elemType) |] =<< addInlines
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
			instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| $(conT className) ($(conT dataName) $elemType) |] =<< addInlines
				[ funD funName [clause [conP conName [if c == component then (varP varName) else wildP | c <- components]] (normalB (varE varName)) []]
				]

		-- instance for Dot class
		dotInstance <- do
			as <- forM components $ \c -> newName $ ['a', c]
			bs <- forM components $ \c -> newName $ ['b', c]
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Num $elemType |] ]) [t| Dot ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'dot
					[ clause
						[ conP conName $ map varP as
						, conP conName $ map varP bs
						]
						(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\(a, b) -> [| $(varE a) * $(varE b) |]) $ zip as bs) []
					]
				]

		-- instance for Norm class
		normInstance <- do
			as <- forM components $ \c -> newName [c]
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Floating $elemType |] ]) [t| Norm ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'norm [clause [] (normalB $ [| sqrt . norm2 |]) []]
				, funD 'norm2 [clause [conP conName $ map varP as]
					(normalB $ foldl1 (\a b -> [| $a + $b |]) $ map (\a -> [| $a * $a |]) $ map varE as) []]
				]

		-- instance for Normalize class
		normalizeInstance <- do
			a <- newName "a"
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Floating $elemType |] ]) [t| Normalize ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'normalize [clause [varP a] (normalB [| $(varE a) * (vecFromScalar (1 / norm $(varE a))) |]) []]
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
				let expr = foldl (\v c -> appE v [| $(varE (mkName $ [c, '_'])) $(varE tvV) |]) (conE conName) variant
				funD (mkName $ variant ++ "__") [clause [varP tvV] (normalB expr) []]
			instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| $(conT instanceName) ($(conT srcDataName) $elemType) |] =<< addInlines (resultDecl : (map funDecl variants))

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
			p <- newName "p"
			let params = zip [0..(dim - 1)] aParams
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Storable $elemType |] ]) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral dim) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) |] | (i, a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral i) $(varE a) |] | (i, a) <- params]) []]
				]

		return $ vecInstance : dotInstance : numInstance : normInstance : normalizeInstance : fractionalInstance : floatingInstance : storableInstance :
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

	-- matrix instances
	matInstances <- liftM concat $ forM matDimensions $ \(dimN, dimM) -> do

		let dimStr = [intToDigit dimN, 'x', intToDigit dimM]
		let dataName = mkName $ "Mat" ++ dimStr
		let conName = mkName $ "Mat" ++ dimStr -- using pattern synonym

		-- some params
		aParams <- mapM newName [['a', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]
		bParams <- mapM newName [['b', intToDigit i, intToDigit j] | i <- [1..dimN], j <- [1..dimM]]

		-- Mat instance
		matInstance <- instanceD (sequence [ [t| Vectorized $elemType |] ]) [t| Mat ($(conT dataName) $elemType) |] =<< addInlines
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
			p <- newName "p"
			let params = zip [(i, j) | i <- [0..(dimN - 1)], j <- [0..(dimM - 1)]] aParams
			instanceD (sequence [ [t| Vectorized $elemType |], [t| Storable $elemType |] ]) [t| Storable ($(conT dataName) $elemType) |] =<< addInlines
				[ funD 'sizeOf [clause [wildP] (normalB [| $(litE $ integerL $ fromIntegral (dimN * dimM)) * sizeOf (undefined :: $elemType) |]) []]
				, funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: $elemType) |]) []]
				, funD 'peek [clause [varP p] (normalB $ doE $ [bindS (varP a) [| peekElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) |] | ((i, j), a) <- params] ++
					[noBindS [| return $(foldl appE (conE conName) $ map (varE . snd) params) |]]) []]
				, funD 'poke [clause [varP p, conP conName $ map (varP . snd) params]
					(normalB $ doE [noBindS [| pokeElemOff (castPtr $(varE p)) $(litE $ integerL $ fromIntegral (j * dimN + i)) $(varE a) |] | ((i, j), a) <- params]) []]
				]

		return [matInstance, numInstance, storableInstance]

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

	return $ crossInstance : vecInstances ++ matInstances ++ mulInstances

-- | Class of things which has quaternions.
class (Vectorized a, Floating a) => Quaternionized a where
	data Quat a :: *
	quat :: Vec4 a -> Quat a
	unquat :: Quat a -> Vec4 a

pattern Quat v <- (unquat -> v) where Quat v = quat v

-- | Conjugation.
class Conjugate q where
	conjugate :: q -> q

-- Generate per-math type declarations for quaternions.
liftM concat $ forM mathQuaternionTypeNamesWithPrefix $ \(mathTypeName, mathTypePrefix) -> do

	let elemType = conT mathTypeName
	let conName = mkName $ mathTypePrefix ++ "Q"
	v <- newName "v"

	-- Quaternionized instance
	quaternionizedInstance <- instanceD (sequence []) [t| Quaternionized $elemType |] =<< addInlines
		[ newtypeInstD (sequence []) ''Quat [elemType] (normalC conName [liftM (\t -> (NotStrict, t)) [t| Vec4 $elemType |]]) [''Eq, ''Show]
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
	v1 <- newName "v1"
	v2 <- newName "v2"
	args1 <- mapM (newName . (: "1")) vecComponents
	args2 <- mapM (newName . (: "2")) vecComponents
	let [x1, y1, z1, w1] = map varE args1
	let [x2, y2, z2, w2] = map varE args2

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
	numInstance <- do
		instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Num (Quat $elemType) |] =<< addInlines
			[ funD '(+) [clause [conP 'Quat [varP v1], conP 'Quat [varP v2]] (normalB [| Quat ($(varE v1) + $(varE v2)) |]) []]
			, funD '(-) [clause [conP 'Quat [varP v1], conP 'Quat [varP v2]] (normalB [| Quat ($(varE v1) - $(varE v2)) |]) []]
			, funD '(*) [clause
				[ conP 'Quat [conP 'Vec4 $ map varP args1]
				, conP 'Quat [conP 'Vec4 $ map varP args2]
				] (normalB [| Quat (Vec4
					($w1 * $x2 + $x1 * $w2 + $y1 * $z2 - $z1 * $y2)
					($w1 * $y2 - $x1 * $z2 + $y1 * $w2 + $z1 * $x2)
					($w1 * $z2 - $x1 * $y2 - $y1 * $x2 + $z1 * $w2)
					($w1 * $w2 - $x1 * $x2 - $y1 * $y2 - $z1 * $z2)
					)|]) []]
			, funD 'negate [clause [conP 'Quat [varP v]] (normalB [| Quat (negate $(varE v)) |]) []]
			, funD 'abs [clause [conP 'Quat [varP v]] (normalB [| Quat (abs $(varE v)) |]) []]
			, funD 'signum [clause [] (normalB [| undefined |]) []]
			, funD 'fromInteger [clause [] (normalB [| undefined |]) []]
			]

	-- Conjugate instance
	conjugateInstance <- instanceD (sequence [ [t| Quaternionized $elemType |] ]) [t| Conjugate (Quat $elemType) |] =<< addInlines
		[ funD 'conjugate [clause [conP 'Quat [conP 'Vec4 $ map varP args1]] (normalB [| Quat (Vec4 (- $x1) (- $y1) (- $z1) $w1) |]) []]
		]

	return [vecInstance, normInstance, normalizeInstance, numInstance, conjugateInstance]
