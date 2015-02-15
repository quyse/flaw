{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

{-# LANGUAGE GADTs, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Flaw.Graphics.Program
	( ProgramScalarType(..)
	, ProgramDimension(..)
	, ProgramType(..)
	, ProgrammableScalarType(..)
	, ProgrammableVectorType
	, ProgrammableType(..)
	, ProgramAttributableType(..)
	, ProgramAttributeType(..)
	, ProgramGenerator(..)
	, ProgramM
	, cnst
	, vec11
	, vec111
	, vec12
	, vec21
	, vec1111
	, vec112
	, vec121
	, vec13
	, vec211
	, vec22
	, vec31
	, attribute
	, uniform
	, calc
	) where

import Control.Monad
import Control.Monad.Trans.Reader
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
class ProgrammableType a => ProgramAttributableType a where
	data ProgramAttributeType a :: *

instance ProgramAttributableType Float where
	data ProgramAttributeType Float
		= ProgramAttributeFloat32
instance ProgramAttributableType Int where
	data ProgramAttributeType Int
		= ProgramAttributeInt32
		| ProgramAttributeInt16
		| ProgramAttributeInt8
instance ProgramAttributableType Word where
	data ProgramAttributeType Word
		= ProgramAttributeUint32
		| ProgramAttributeUint16
		| ProgramAttributeUint8

-- instance (ProgrammableScalarType a, ProgramAttributableType a) => ProgramAttributableType (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	instanceD (return [ClassP ''ProgrammableScalarType [VarT a], ClassP ''ProgramAttributableType [VarT a]]) (appT (conT ''ProgramAttributableType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''ProgramAttributeType [appT (conT v) (varT a)]
			[ normalC (mkName $ "ProgramAttributeVec" ++ [c]) [return (NotStrict, AppT (ConT ''ProgramAttributeType) $ VarT a)]
			] []
		]

-- | Program stage class.
class ProgramStage s

data VertexStage
instance ProgramStage VertexStage

data PixelStage
instance ProgramStage PixelStage

-- | Class of program generator.
class ProgramGenerator g where
	data ProgramNode g :: * -> * -> *
	programRegisterAttribute :: ProgramAttributableType a => g -> ProgramAttributeType a -> Int -> IO (ProgramNode g VertexStage a)
	programRegisterUniform :: (ProgrammableType a, ProgramStage s) => g -> Int -> Int -> IO (ProgramNode g s a)
	programRegisterValue :: (ProgrammableType a, ProgramStage s) => g -> ProgramNode g s a -> IO (ProgramNode g s a)
	programInterpolate :: ProgrammableType a => g -> ProgramNode g VertexStage a -> IO (ProgramNode g PixelStage a)
	programNodeConst :: (ProgrammableType a, ProgramStage s) => a -> ProgramNode g s a
	programNodeVec11 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec2 a)
	programNodeVec111 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec3 a)
	programNodeVec12 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec3 a)
	programNodeVec21 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s (Vec3 a)
	programNodeVec1111 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
	programNodeVec112 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec4 a)
	programNodeVec121 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
	programNodeVec13 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec3 a) -> ProgramNode g s (Vec4 a)
	programNodeVec211 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
	programNodeVec22 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec4 a)
	programNodeVec31 :: (ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec3 a) -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
	programNodeAdd :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeSubtract :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeMultiply :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeDivide :: (ProgrammableType a, Fractional a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeRecip :: (ProgrammableType a, Fractional a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeNegate :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAbs :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeSignum :: (ProgrammableType a, Num a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodePi :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a
	programNodeExp :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeSqrt :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeLog :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodePow :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeLogBase :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a
	programNodeSin :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeTan :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeCos :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAsin :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAtan :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAcos :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeSinh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeTanh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeCosh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAsinh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAtanh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeAcosh :: (ProgrammableType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a
	programNodeMul :: (Mul a b c, ProgramStage s) => ProgramNode g s a -> ProgramNode g s b -> ProgramNode g s c
	programNodeDot :: (ProgrammableScalarType a, ProgrammableVectorType v, Dot v a, ProgramStage s) => ProgramNode g s v -> ProgramNode g s v -> ProgramNode g s a
	programNodeInstanceID :: ProgramNode g VertexStage Word
	programNodeX :: (ProgrammableScalarType a, ProgrammableVectorType v, VecX v a, ProgramStage s) => ProgramNode g s v -> ProgramNode g s a
	programNodeY :: (ProgrammableScalarType a, ProgrammableVectorType v, VecY v a, ProgramStage s) => ProgramNode g s v -> ProgramNode g s a
	programNodeZ :: (ProgrammableScalarType a, ProgrammableVectorType v, VecZ v a, ProgramStage s) => ProgramNode g s v -> ProgramNode g s a
	programNodeW :: (ProgrammableScalarType a, ProgrammableVectorType v, VecW v a, ProgramStage s) => ProgramNode g s v -> ProgramNode g s a
	programNodeSwizzleX1 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecX1 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleX2 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecX2 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleX3 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecX3 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleX4 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecX4 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleY1 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecY1 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleY2 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecY2 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleY3 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecY3 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleY4 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecY4 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleZ1 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecZ1 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleZ2 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecZ2 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleZ3 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecZ3 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleZ4 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecZ4 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleW1 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecW1 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleW2 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecW2 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleW3 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecW3 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2
	programNodeSwizzleW4 :: (ProgrammableScalarType a, ProgrammableVectorType v1, ProgrammableVectorType v2, SwizzleVecW4 v1 v2 a, ProgramStage s) => ProgramNode g s v1 -> ProgramNode g s v2

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
		[ funD (mkName $ [c, '_']) [clause [] (normalB $ varE $ mkName $ "programNode" ++ [toUpper c]) []]
		]

cnst :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => a -> ProgramNode g s a
cnst = programNodeConst
vec11 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec2 a)
vec11 = programNodeVec11
vec111 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec3 a)
vec111 = programNodeVec111
vec12 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec3 a)
vec12 = programNodeVec12
vec21 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s (Vec3 a)
vec21 = programNodeVec21
vec1111 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
vec1111 = programNodeVec1111
vec112 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec4 a)
vec112 = programNodeVec112
vec121 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
vec121 = programNodeVec121
vec13 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s a -> ProgramNode g s (Vec3 a) -> ProgramNode g s (Vec4 a)
vec13 = programNodeVec13
vec211 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s a -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
vec211 = programNodeVec211
vec22 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec2 a) -> ProgramNode g s (Vec4 a)
vec22 = programNodeVec22
vec31 :: (ProgramGenerator g, ProgrammableScalarType a, ProgramStage s) => ProgramNode g s (Vec3 a) -> ProgramNode g s a -> ProgramNode g s (Vec4 a)
vec31 = programNodeVec31

-- | Program monad.
type ProgramM g a = ReaderT g IO a

-- | Create attribute.
attribute :: (ProgramGenerator g, ProgramAttributableType a) => ProgramAttributeType a -> Int -> ProgramM g (ProgramNode g VertexStage a)
attribute dataType offset = ReaderT $ \g -> programRegisterAttribute g dataType offset

-- | Create uniform variable.
uniform :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> Int -> ProgramM g (ProgramNode g s a)
uniform buffer offset = ReaderT $ \g -> programRegisterUniform g buffer offset

-- | Perform calculation (create temporary value).
calc :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => ProgramNode g s a -> ProgramM g (ProgramNode g s a)
calc a = ReaderT $ \g -> programRegisterValue g a
