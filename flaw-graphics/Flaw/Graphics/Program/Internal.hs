{-|
Module: Flaw.Graphics.Program.Internal
Description: Internals for shader program support.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Flaw.Graphics.Program.Internal
	( ProgramScalarType(..)
	, ProgramDimension(..)
	, ProgramType(..)
	, ProgrammableScalarType(..)
	, ProgrammableVectorType
	, ProgrammableType(..)
	, ProgramAttributableType
	, ProgramAttributeType(..)
	, ProgramAttributeNormalization(..)
	, ProgrammableTuple(..)
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
class ProgrammableType a => ProgramAttributableType a

-- | Types of attributes.
data ProgramAttributeType
	= ProgramAttributeFloat32
	| ProgramAttributeFloat16
	| ProgramAttributeInt32 ProgramAttributeNormalization
	| ProgramAttributeInt16 ProgramAttributeNormalization
	| ProgramAttributeInt8 ProgramAttributeNormalization
	| ProgramAttributeUint32 ProgramAttributeNormalization
	| ProgramAttributeUint16 ProgramAttributeNormalization
	| ProgramAttributeUint8 ProgramAttributeNormalization
	| ProgramAttributeVec1 ProgramAttributeType
	| ProgramAttributeVec2 ProgramAttributeType
	| ProgramAttributeVec3 ProgramAttributeType
	| ProgramAttributeVec4 ProgramAttributeType

-- | Normalization mode for integer attribute.
data ProgramAttributeNormalization
	= ProgramAttributeNonNormalized
	| ProgramAttributeNormalized

-- instance (ProgrammableScalarType a, ProgramAttributableType a) => ProgramAttributableType (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''ProgramAttributableType [VarT a]]) (appT (conT ''ProgramAttributableType) $ appT (conT v) $ varT a) []

class ProgrammableTuple a where
	type MapTuple a (b :: * -> *) :: *

instance ProgrammableTuple (a, b) where
	type MapTuple (a, b) f = (f a, f a)
instance ProgrammableTuple (a, b, c) where
	type MapTuple (a, b, c) f = (f a, f b, f c)
instance ProgrammableTuple (a, b, c, d) where
	type MapTuple (a, b, c, d) f = (f a, f b, f c, f d)

-- | Program stage class.
class ProgramStage s

data VertexStage
instance ProgramStage VertexStage

data PixelStage
instance ProgramStage PixelStage

-- | Class of program generator.
class ProgramGenerator g where
	data ProgramNode g :: * -> * -> *
	programRegisterAttribute :: ProgramAttributableType a => g
		-> Int -- ^ Slot.
		-> Int -- ^ Offset.
		-> Int -- ^ Divisor.
		-> ProgramAttributeType
		-> IO (ProgramNode g VertexStage a)
	programRegisterUniform :: (ProgrammableType a, ProgramStage s) => g
		-> Int -- ^ Buffer slot.
		-> Int -- ^ Offset in buffer.
		-> Int -- ^ Array size (0 for scalar).
		-> IO (ProgramNode g s a)
	programRegisterSampler :: (ProgrammableType a, ProgramStage s) => g
		-> Int -- ^ Sampler slot.
		-> Int -- ^ Dimension.
		-> IO (ProgramNode g s a)
	-- | Register temporary variable.
	programRegisterValue :: (ProgrammableType a, ProgramStage s) => g
		-> ProgramNode g s a
		-> IO (ProgramNode g s a)
	programInterpolate :: ProgrammableType a => g
		-> ProgramNode g VertexStage a
		-> IO (ProgramNode g PixelStage a)
	programNodeConst :: (ProgrammableType a, ProgramStage s) => a
		-> ProgramNode g s a
	programNodeCombineVec2 :: (ProgrammableType a, ProgrammableType b, ProgrammableVectorType q, CombineVec (a, b) q, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s q
	programNodeCombineVec3 :: (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableVectorType q, CombineVec (a, b, c) q, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s c
		-> ProgramNode g s q
	programNodeCombineVec4 :: (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableType d, ProgrammableVectorType q, CombineVec (a, b, c, d) q, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s c
		-> ProgramNode g s d
		-> ProgramNode g s q
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
	programNodeMul :: (Mul a b c, ProgramStage s)
		=> ProgramNode g s a
		-> ProgramNode g s b
		-> ProgramNode g s c
	programNodeDot :: (ProgrammableScalarType a, ProgrammableVectorType v, Dot v a, ProgramStage s)
		=> ProgramNode g s v
		-> ProgramNode g s v
		-> ProgramNode g s a
	programNodeInstanceID :: ProgramNode g VertexStage Word
	programNodeComponent :: (ProgrammableVectorType v, ProgrammableScalarType a, ProgramStage s)
		=> Char
		-> ProgramNode g s v
		-> ProgramNode g s a
	programNodeSwizzle :: (ProgrammableVectorType v1, ProgrammableVectorType v2, ProgramStage s)
		=> String
		-> ProgramNode g s v1
		-> ProgramNode g s v2

instance (Vec v e, ProgrammableVectorType v, ProgrammableScalarType e, ProgramStage s) => Vec (ProgramNode g s v) (ProgramNode g s e) where
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

instance (ProgramGenerator g, ProgramStage s, ProgrammableType a, ProgrammableType b, ProgrammableType c, Mul a b c) => Mul (ProgramNode g s a) (ProgramNode g s b) (ProgramNode g s c) where
	mul = programNodeMul

instance (ProgramGenerator g, ProgramStage s, ProgrammableScalarType a, ProgrammableVectorType v, Dot v a) => Dot (ProgramNode g s v) (ProgramNode g s a) where
	dot = programNodeDot

{- instance
	( ProgramGenerator g
	, ProgramStage s
	, ProgrammableScalarType a
	, ProgrammableVectorType v
	, Vec{X..W} v a
	) => Vec{X..W} (ProgramNode g s v) (ProgramNode g s a)
-}
forM "xyzw" $ \c -> do
	g <- newName "g"
	s <- newName "s"
	a <- newName "a"
	v <- newName "v"
	let vc = mkName $ "Vec" ++ [toUpper c]
	instanceD (return
		[ ClassP ''ProgramGenerator [VarT g]
		, ClassP ''ProgramStage [VarT s]
		, ClassP ''ProgrammableScalarType [VarT a]
		, ClassP ''ProgrammableVectorType [VarT v]
		, ClassP vc [VarT v, VarT a]
		]) [t| $(conT vc) (ProgramNode $(varT g) $(varT s) $(varT v)) (ProgramNode $(varT g) $(varT s) $(varT a)) |]
		[ funD (mkName $ [c, '_']) [clause [] (normalB [| programNodeComponent $(litE $ charL c) |]) []]
		]

{- instance
	( ProgramGenerator g
	, ProgramStage s
	, ProgrammableScalarType a
	, ProgrammableVectorType v1
	, ProgrammableVectorType v2
	, SwizzleVec{X..W}{1..4} v1 v2 a
	) => SwizzleVec{X..W}{1..4} (ProgramNode g s v1) (ProgramNode g s v2) (ProgramNode g s a)
-}
forM [(maxComp, dim) | maxComp <- [1..4], dim <- [1..4]] $ \(maxComp, dim) -> do
	g <- newName "g"
	s <- newName "s"
	a <- newName "a"
	v1 <- newName "v1"
	v2 <- newName "v2"
	let components = take maxComp "xyzw"
	let sv = mkName $ "SwizzleVec" ++ [toUpper $ last components, intToDigit dim]
	let variants = filter variantFilter $ genVariants dim where
		genVariants 0 = [""]
		genVariants len = [c : cs | c <- components, cs <- genVariants $ len - 1]
		variantFilter variant = all (\c -> elem c components) variant && elem (last components) variant
	let funDecl variant = do
		funD (mkName $ variant ++ "__") [clause [] (normalB [| programNodeSwizzle $(litE $ stringL variant) |]) []]
	instanceD (return $
		[ ClassP ''ProgramGenerator [VarT g]
		, ClassP ''ProgramStage [VarT s]
		, ClassP ''ProgrammableScalarType [VarT a]
		, ClassP ''ProgrammableVectorType [VarT v1]
		, ClassP ''ProgrammableVectorType [VarT v2]
		, ClassP sv [VarT v1, VarT v2, VarT a]
		])
		[t| $(conT sv) (ProgramNode $(varT g) $(varT s) $(varT v1)) (ProgramNode $(varT g) $(varT s) $(varT v2)) (ProgramNode $(varT g) $(varT s) $(varT a)) |] $
		map funDecl variants

---- CombineVec instances.
instance (ProgrammableType a, ProgrammableType b, ProgrammableVectorType q, ProgramGenerator g, ProgramStage s, CombineVec (a, b) q) => CombineVec (ProgramNode g s a, ProgramNode g s b) (ProgramNode g s q) where
	combineVec (a, b) = programNodeCombineVec2 a b
instance (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableVectorType q, ProgramGenerator g, ProgramStage s, CombineVec (a, b, c) q) => CombineVec (ProgramNode g s a, ProgramNode g s b, ProgramNode g s c) (ProgramNode g s q) where
	combineVec (a, b, c) = programNodeCombineVec3 a b c
instance (ProgrammableType a, ProgrammableType b, ProgrammableType c, ProgrammableType d, ProgrammableVectorType q, ProgramGenerator g, ProgramStage s, CombineVec (a, b, c, d) q) => CombineVec (ProgramNode g s a, ProgramNode g s b, ProgramNode g s c, ProgramNode g s d) (ProgramNode g s q) where
	combineVec (a, b, c, d) = programNodeCombineVec4 a b c d
