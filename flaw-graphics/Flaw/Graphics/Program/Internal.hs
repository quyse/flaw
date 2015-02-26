{-|
Module: Flaw.Graphics.Program.Internal
Description: Internals for shader program support.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}

module Flaw.Graphics.Program.Internal
	( ProgramScalarType(..)
	, ProgramDimension(..)
	, ProgramType(..)
	, ProgrammableScalarType(..)
	, ProgrammableVectorType
	, ProgrammableType(..)
	, AttributableType(..)
	, AttributeFormat(..)
	, AttributeType(..)
	, AttributeNormalization(..)
	, UniformableType(..)
	, UniformFormat(..)
	, UniformType(..)
	, ProgrammableTuple(..)
	, ProgramSamplerDimension(..)
	, ProgramStage
	, VertexStage
	, PixelStage
	, ProgramGenerator(..)
	) where

import Control.Monad
import Data.Char
import Data.Word
import Language.Haskell.TH

import Flaw.Math

-- | Supported scalar types in programs.
data ProgramScalarType
	= ProgramScalarFloat
	| ProgramScalarDouble
	| ProgramScalarInt
	| ProgramScalarUint
	| ProgramScalarBool

-- | Supported dimensions in programs.
data ProgramDimension
	= ProgramDimension1
	| ProgramDimension2
	| ProgramDimension3
	| ProgramDimension4

-- | Supported types in programs.
data ProgramType
	= ProgramScalar ProgramScalarType
	| ProgramVector ProgramDimension ProgramScalarType
	| ProgramMatrix ProgramDimension ProgramDimension ProgramScalarType

-- | Class of scalar types which can be used in program.
class ProgrammableType a => ProgrammableScalarType a where
	-- | Get program scalar type.
	-- Argument is not used.
	programScalarType :: a -> ProgramScalarType

instance ProgrammableScalarType Float where
	programScalarType _ = ProgramScalarFloat
instance ProgrammableScalarType Double where
	programScalarType _ = ProgramScalarDouble
instance ProgrammableScalarType Int where
	programScalarType _ = ProgramScalarInt
instance ProgrammableScalarType Word where
	programScalarType _ = ProgramScalarUint
instance ProgrammableScalarType Bool where
	programScalarType _ = ProgramScalarBool

-- | Class of vector types which can be used in program.
class ProgrammableType a => ProgrammableVectorType a

instance ProgrammableScalarType a => ProgrammableVectorType (Vec1 a)
instance ProgrammableScalarType a => ProgrammableVectorType (Vec2 a)
instance ProgrammableScalarType a => ProgrammableVectorType (Vec3 a)
instance ProgrammableScalarType a => ProgrammableVectorType (Vec4 a)

-- | Class of types which can be used in program.
class ProgrammableType a where
	programType :: a -> ProgramType

instance ProgrammableType Float where
	programType _ = ProgramScalar ProgramScalarFloat
instance ProgrammableType Double where
	programType _ = ProgramScalar ProgramScalarDouble
instance ProgrammableType Int where
	programType _ = ProgramScalar ProgramScalarInt
instance ProgrammableType Word where
	programType _ = ProgramScalar ProgramScalarUint
instance ProgrammableType Bool where
	programType _ = ProgramScalar ProgramScalarBool

-- instance ProgrammableScalarType a => ProgrammableType (Vec{1..4} a)
liftM concat $ forM ['1'..'4'] $ \c -> do
	let t = conT $ mkName $ "Vec" ++ [c]
	let d = conE $ mkName $ "ProgramDimension" ++ [c]
	[d|
		instance ProgrammableScalarType a => ProgrammableType ($t a) where
			programType _ = ProgramVector $d $ programScalarType (undefined :: a)
		|]

-- instance ProgrammableScalarType a => ProgrammableType (Mat{1..4}x{1..4} a)
liftM concat $ forM [(x, y) | x <- ['1'..'4'], y <- ['1'..'4']] $ \(cx, cy) -> let
	t = conT $ mkName $ "Mat" ++ [cx, 'x', cy]
	dx = conE $ mkName $ "ProgramDimension" ++ [cx]
	dy = conE $ mkName $ "ProgramDimension" ++ [cy]
	in [d|
		instance ProgrammableScalarType a => ProgrammableType ($t a) where
			programType _ = ProgramMatrix $dx $dy $ programScalarType (undefined :: a)
		|]

-- | Class of types which can be used in vertex attribute.
class ProgrammableType a => AttributableType a where
	-- | Typed attibute format.
	data AttributeFormat a :: *
	attributeType :: AttributeFormat a -> AttributeType

-- | Attribute format ids.
data AttributeType
	= ATFloat32
	| ATFloat16
	| ATInt32 AttributeNormalization
	| ATInt16 AttributeNormalization
	| ATInt8 AttributeNormalization
	| ATUint32 AttributeNormalization
	| ATUint16 AttributeNormalization
	| ATUint8 AttributeNormalization
	| ATVec1 AttributeType
	| ATVec2 AttributeType
	| ATVec3 AttributeType
	| ATVec4 AttributeType
	| ATMat1x1 AttributeType
	| ATMat1x2 AttributeType
	| ATMat1x3 AttributeType
	| ATMat1x4 AttributeType
	| ATMat2x1 AttributeType
	| ATMat2x2 AttributeType
	| ATMat2x3 AttributeType
	| ATMat2x4 AttributeType
	| ATMat3x1 AttributeType
	| ATMat3x2 AttributeType
	| ATMat3x3 AttributeType
	| ATMat3x4 AttributeType
	| ATMat4x1 AttributeType
	| ATMat4x2 AttributeType
	| ATMat4x3 AttributeType
	| ATMat4x4 AttributeType

-- | Normalization mode for integer attribute.
data AttributeNormalization
	= AttributeNonNormalized
	| AttributeNormalized

instance AttributableType Float where
	data AttributeFormat Float
		= AttributeFloat32
		| AttributeFloat16
	attributeType f = case f of
		AttributeFloat32 -> ATFloat32
		AttributeFloat16 -> ATFloat16

instance AttributableType Int where
	data AttributeFormat Int
		= AttributeInt32 AttributeNormalization
		| AttributeInt16 AttributeNormalization
		| AttributeInt8 AttributeNormalization
	attributeType f = case f of
		AttributeInt32 n -> ATInt32 n
		AttributeInt16 n -> ATInt16 n
		AttributeInt8 n -> ATInt8 n

instance AttributableType Word where
	data AttributeFormat Word
		= AttributeUint32 AttributeNormalization
		| AttributeUint16 AttributeNormalization
		| AttributeUint8 AttributeNormalization
	attributeType f = case f of
		AttributeUint32 n -> ATUint32 n
		AttributeUint16 n -> ATUint16 n
		AttributeUint8 n -> ATUint8 n

-- instance (ProgrammableScalarType a, AttributableType a) => AttributableType (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	let conName = mkName $ "AttributeVec" ++ [c]
	b <- newName "b"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''AttributableType [VarT a]]) (appT (conT ''AttributableType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''AttributeFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''AttributeFormat) $ VarT a)]
			] []
		, funD 'attributeType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "ATVec" ++ [c]) (attributeType $(varE b)) |]) []]
		]

-- instance (ProgrammableScalarType a, AttributableType a) => AttributableType (Mat{1..4}x{1..4} a)
forM [(ci, cj) | ci <- ['1'..'4'], cj <- ['1'..'4']] $ \(ci, cj) -> do
	let v = mkName $ "Mat" ++ [ci, 'x', cj]
	a <- newName "a"
	let conName = mkName $ "AttributeMat" ++ [ci, 'x', cj]
	b <- newName "b"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''AttributableType [VarT a]]) (appT (conT ''AttributableType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''AttributeFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''AttributeFormat) $ VarT a)]
			] []
		, funD 'attributeType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "ATMat" ++ [ci, 'x', cj]) (attributeType $(varE b)) |]) []]
		]

-- | Class of types which can be used in uniform.
class ProgrammableType a => UniformableType a where
	data UniformFormat a :: *
	uniformType :: UniformFormat a -> UniformType

data UniformType
	= UTFloat
	| UTHalf
	| UTInt
	| UTUint
	| UTVec1 UniformType
	| UTVec2 UniformType
	| UTVec3 UniformType
	| UTVec4 UniformType
	| UTMat1x1 UniformType
	| UTMat1x2 UniformType
	| UTMat1x3 UniformType
	| UTMat1x4 UniformType
	| UTMat2x1 UniformType
	| UTMat2x2 UniformType
	| UTMat2x3 UniformType
	| UTMat2x4 UniformType
	| UTMat3x1 UniformType
	| UTMat3x2 UniformType
	| UTMat3x3 UniformType
	| UTMat3x4 UniformType
	| UTMat4x1 UniformType
	| UTMat4x2 UniformType
	| UTMat4x3 UniformType
	| UTMat4x4 UniformType

instance UniformableType Float where
	data UniformFormat Float = UniformFloat | UniformHalf
	uniformType f = case f of
		UniformFloat -> UTFloat
		UniformHalf -> UTHalf

instance UniformableType Int where
	data UniformFormat Int = UniformInt
	uniformType _ = UTInt

instance UniformableType Word where
	data UniformFormat Word = UniformUint
	uniformType _ = UTUint

-- instance (ProgrammableScalarType a, UniformableType a) => UniformableType (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	let conName = mkName $ "UniformVec" ++ [c]
	b <- newName "b"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''UniformableType [VarT a]]) (appT (conT ''UniformableType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''UniformFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''UniformFormat) $ VarT a)]
			] []
		, funD 'uniformType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "UTVec" ++ [c]) (uniformType $(varE b)) |]) []]
		]

-- instance (ProgrammableScalarType a, UniformableType a) => UniformableType (Mat{1..4}x{1..4} a)
forM [(ci, cj) | ci <- ['1'..'4'], cj <- ['1'..'4']] $ \(ci, cj) -> do
	let v = mkName $ "Mat" ++ [ci, 'x', cj]
	a <- newName "a"
	let conName = mkName $ "UniformMat" ++ [ci, 'x', cj]
	b <- newName "b"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''UniformableType [VarT a]]) (appT (conT ''UniformableType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''UniformFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''UniformFormat) $ VarT a)]
			] []
		, funD 'uniformType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "UTMat" ++ [ci, 'x', cj]) (uniformType $(varE b)) |]) []]
		]

class ProgrammableTuple a where
	type MapTuple a (b :: * -> *) :: *

instance ProgrammableTuple (a, b) where
	type MapTuple (a, b) f = (f a, f a)
instance ProgrammableTuple (a, b, c) where
	type MapTuple (a, b, c) f = (f a, f b, f c)
instance ProgrammableTuple (a, b, c, d) where
	type MapTuple (a, b, c, d) f = (f a, f b, f c, f d)

data ProgramSamplerDimension
	= ProgramSampler1D
	| ProgramSampler2D
	| ProgramSampler3D
	| ProgramSamplerCube

-- | Program stage class.
class ProgramStage s

data VertexStage
instance ProgramStage VertexStage

data PixelStage
instance ProgramStage PixelStage

-- | Class of program generator.
class ProgramGenerator g where
	data ProgramNode g
		:: * -- stage
		-> * -- type
		-> *
	data ProgramSamplerNode g
		:: * -- stage
		-> * -- sample type
		-> * -- coords type
		-> *
	programRegisterAttribute :: AttributableType a => g
		-> Int -- ^ Slot.
		-> Int -- ^ Offset.
		-> Int -- ^ Divisor.
		-> AttributeType
		-> IO (ProgramNode g VertexStage a)
	programRegisterUniform :: (UniformableType a, ProgramStage s) => g
		-> Int -- ^ Buffer slot.
		-> Int -- ^ Offset in buffer.
		-> Int -- ^ Array size (0 for scalar).
		-> UniformType
		-> IO (ProgramNode g s a)
	programRegisterSampler :: (ProgrammableType a, ProgrammableType b, ProgramStage s) => g
		-> Int
		-> ProgramSamplerDimension
		-> IO (ProgramSamplerNode g s a b)
	-- | Register temporary variable.
	programRegisterTemp :: (ProgrammableType a, ProgramStage s) => g
		-> ProgramNode g s a
		-> IO (ProgramNode g s a)
	programInterpolate :: ProgrammableType a => g
		-> ProgramNode g VertexStage a
		-> IO (ProgramNode g PixelStage a)
	programNodeConst :: (ProgrammableType a, ProgramStage s) => a
		-> ProgramNode g s a
	programNodeCombineVec2 :: (ProgrammableType a, ProgrammableType b, Combine2Vec a b, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s (Combine2VecResult a b)
	programNodeCombineVec3 :: (ProgrammableType a, ProgrammableType b, ProgrammableType c, Combine3Vec a b c, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s c
		-> ProgramNode g s (Combine3VecResult a b c)
	programNodeCombineVec4 :: (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableType d, Combine4Vec a b c d, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s c
		-> ProgramNode g s d
		-> ProgramNode g s (Combine4VecResult a b c d)
	programNodeAdd :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeSubtract :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeMultiply :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeDivide :: (ProgrammableType a, Fractional a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeRecip :: (ProgrammableType a, Fractional a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeNegate :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAbs :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeSignum :: (ProgrammableType a, Num a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodePi :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
	programNodeExp :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeSqrt :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeLog :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodePow :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeLogBase :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
		-> ProgramNode g s a
	programNodeSin :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeTan :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeCos :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAsin :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAtan :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAcos :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeSinh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeTanh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeCosh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAsinh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAtanh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeAcosh :: (ProgrammableType a, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s a
	programNodeMul :: (Mul a b, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s (MulResult a b)
	programNodeDot :: (ProgrammableVectorType v, e ~ VecElement v, ProgrammableScalarType e, Dot v, ProgramStage s)
		=> ProgramNode g s v
		-> ProgramNode g s v
		-> ProgramNode g s (VecElement v)
	programNodeInstanceID :: ProgramNode g VertexStage Word
	programNodeComponent :: (ProgrammableVectorType v, ProgrammableScalarType a, ProgramStage s)
		=> Char
		-> ProgramNode g s v
		-> ProgramNode g s a
	programNodeSwizzle :: (ProgrammableVectorType v1, ProgrammableVectorType v2, ProgramStage s)
		=> String
		-> ProgramNode g s v1
		-> ProgramNode g s v2
	programNodeSample :: (ProgrammableType a, ProgrammableType b, ProgramStage s)
		=> ProgramSamplerNode g s a b
		-> ProgramNode g s b
		-> ProgramNode g s a
	programNodeCast :: (ProgrammableType a, ProgrammableType b, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b

instance Vec v => Vec (ProgramNode g s v) where
	type VecElement (ProgramNode g s v) = ProgramNode g s (VecElement v)
	vecLength _ = vecLength (undefined :: v)
	vecToList _ = undefined

instance (ProgramGenerator g, ProgramStage s, ProgrammableType a, Num a) => Num (ProgramNode g s a) where
	(+) = programNodeAdd
	(*) = programNodeMultiply
	(-) = programNodeSubtract
	negate = programNodeNegate
	abs = programNodeAbs
	signum = programNodeSignum
	fromInteger = programNodeConst . fromInteger

instance (ProgramGenerator g, ProgramStage s, ProgrammableType a, Fractional a) => Fractional (ProgramNode g s a) where
	(/) = programNodeDivide
	recip = programNodeRecip
	fromRational = programNodeConst . fromRational

instance (ProgramGenerator g, ProgramStage s, ProgrammableType a, Floating a) => Floating (ProgramNode g s a) where
	pi = programNodePi
	exp = programNodeExp
	sqrt = programNodeSqrt
	log = programNodeLog
	(**) = programNodePow
	logBase = programNodeLogBase
	sin = programNodeSin
	tan = programNodeTan
	cos = programNodeCos
	asin = programNodeAsin
	atan = programNodeAtan
	acos = programNodeAcos
	sinh = programNodeSinh
	tanh = programNodeTanh
	cosh = programNodeCosh
	asinh = programNodeAsinh
	atanh = programNodeAtanh
	acosh = programNodeAcosh

instance (ProgramGenerator g, ProgramStage s, ProgrammableType a, ProgrammableType b, ProgrammableType (MulResult a b), Mul a b) => Mul (ProgramNode g s a) (ProgramNode g s b) where
	type MulResult (ProgramNode g s a) (ProgramNode g s b) = ProgramNode g s (MulResult a b)
	mul = programNodeMul

instance (ProgramGenerator g, ProgramStage s, ProgrammableVectorType v, ProgrammableScalarType (VecElement v), Dot v) => Dot (ProgramNode g s v) where
	dot = programNodeDot

{- instance
	( ProgramGenerator g
	, ProgramStage s
	, ProgrammableVectorType v
	, ProgrammableScalarType (VecElement v)
	, Vec{X..W} v
	) => Vec{X..W} (ProgramNode g s v)
-}
forM "xyzw" $ \c -> do
	g <- newName "g"
	s <- newName "s"
	v <- newName "v"
	let vc = mkName $ "Vec" ++ [toUpper c]
	instanceD (return
		[ ClassP ''ProgramGenerator [VarT g]
		, ClassP ''ProgramStage [VarT s]
		, ClassP ''ProgrammableVectorType [VarT v]
		, ClassP ''ProgrammableScalarType [AppT (ConT ''VecElement) $ VarT v]
		, ClassP vc [VarT v]
		]) [t| $(conT vc) (ProgramNode $(varT g) $(varT s) $(varT v)) |]
		[ funD (mkName $ [c, '_']) [clause [] (normalB [| programNodeComponent $(litE $ charL c) |]) []]
		]

{- instance
	( ProgramGenerator g
	, ProgramStage s
	, ProgrammableVectorType v
	, ProgrammableVectorType (SwizzleVec{X..W}{1..4}Result v)
	, SwizzleVec{X..W}{1..4} v
	) => SwizzleVec{X..W}{1..4} (ProgramNode g s v)
-}
forM [(maxComp, dim) | maxComp <- [1..4], dim <- [1..4]] $ \(maxComp, dim) -> do
	g <- newName "g"
	s <- newName "s"
	v <- newName "v"
	let components = take maxComp "xyzw"
	let nameSuffix = [toUpper $ last components, intToDigit dim]
	let sv = mkName $ "SwizzleVec" ++ nameSuffix
	let resultTypeName = mkName $ "SwizzleVecResult" ++ nameSuffix
	let variants = filter variantFilter $ genVariants dim where
		genVariants 0 = [""]
		genVariants len = [c : cs | c <- components, cs <- genVariants $ len - 1]
		variantFilter variant = all (\c -> elem c components) variant && elem (last components) variant
	let funDecl variant = do
		funD (mkName $ variant ++ "__") [clause [] (normalB [| programNodeSwizzle $(litE $ stringL variant) |]) []]
	let resultTypeDecl = tySynInstD resultTypeName $ tySynEqn
		[ [t| ProgramNode $(varT g) $(varT s) $(varT v) |] ]
		[t| ProgramNode $(varT g) $(varT s) ($(conT resultTypeName) $(varT v)) |]
	instanceD (return $
		[ ClassP ''ProgramGenerator [VarT g]
		, ClassP ''ProgramStage [VarT s]
		, ClassP ''ProgrammableVectorType [VarT v]
		, ClassP ''ProgrammableScalarType [AppT (ConT ''VecElement) $ VarT v]
		, ClassP ''ProgrammableVectorType [AppT (ConT $ mkName $ "SwizzleVecResult" ++ nameSuffix) $ VarT v]
		, ClassP sv [VarT v]
		])
		[t| $(conT sv) (ProgramNode $(varT g) $(varT s) $(varT v)) |] $ resultTypeDecl : map funDecl variants

---- Combine{2..4}Vec instances.
instance (ProgrammableType a, ProgrammableType b, ProgramGenerator g, ProgramStage s, Combine2Vec a b) => Combine2Vec (ProgramNode g s a) (ProgramNode g s b) where
	type Combine2VecResult (ProgramNode g s a) (ProgramNode g s b) = ProgramNode g s (Combine2VecResult a b)
	combine2Vec = programNodeCombineVec2
instance (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgramGenerator g, ProgramStage s, Combine3Vec a b c) => Combine3Vec (ProgramNode g s a) (ProgramNode g s b) (ProgramNode g s c) where
	type Combine3VecResult (ProgramNode g s a) (ProgramNode g s b) (ProgramNode g s c) = ProgramNode g s (Combine3VecResult a b c)
	combine3Vec = programNodeCombineVec3
instance (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableType d, ProgramGenerator g, ProgramStage s, Combine4Vec a b c d) => Combine4Vec (ProgramNode g s a) (ProgramNode g s b) (ProgramNode g s c) (ProgramNode g s d) where
	type Combine4VecResult (ProgramNode g s a) (ProgramNode g s b) (ProgramNode g s c) (ProgramNode g s d) = ProgramNode g s (Combine4VecResult a b c d)
	combine4Vec = programNodeCombineVec4
