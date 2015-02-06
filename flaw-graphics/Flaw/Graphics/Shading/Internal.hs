{-|
Module: Flaw.Graphics.Shading.Internal
Description: Internals of shader support.
License: MIT
-}

{-# LANGUAGE GADTs, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Flaw.Graphics.Shading.Internal
	( ShaderScalarType(..)
	, ShaderDimension(..)
	, ShaderType(..)
	, ShaderableScalar(..)
	, Shaderable(..)
	, ShaderAttributable(..)
	, ShaderNode(..)
	, ShaderGenerator(..)
	, ShaderM(..)
	, attribute
	, calc
	) where

import Control.Applicative
import Control.Monad
import Data.Word
import Language.Haskell.TH

import Flaw.Math

-- | Supported scalar types in shaders.
data ShaderScalarType
	= ShaderScalarFloat
	| ShaderScalarDouble
	| ShaderScalarInt
	| ShaderScalarUint
	| ShaderScalarBool

-- | Supported dimensions in shaders.
data ShaderDimension
	= ShaderDimension1
	| ShaderDimension2
	| ShaderDimension3
	| ShaderDimension4

-- | Supported types in shaders.
data ShaderType
	= ShaderScalar ShaderScalarType
	| ShaderVector ShaderDimension ShaderScalarType
	| ShaderMatrix ShaderDimension ShaderDimension ShaderScalarType

-- | Class of scalar types which can be used in shader.
class ShaderableScalar a where
	-- | Get shader scalar type.
	-- Argument is not used.
	shaderScalarType :: a -> ShaderScalarType

instance ShaderableScalar Float where
	shaderScalarType _ = ShaderScalarFloat
instance ShaderableScalar Double where
	shaderScalarType _ = ShaderScalarDouble
instance ShaderableScalar Int where
	shaderScalarType _ = ShaderScalarInt
instance ShaderableScalar Word where
	shaderScalarType _ = ShaderScalarUint
instance ShaderableScalar Bool where
	shaderScalarType _ = ShaderScalarBool

-- | Class of types which can be used in shader.
class Shaderable a where
	shaderType :: a -> ShaderType

instance Shaderable Float where
	shaderType _ = ShaderScalar ShaderScalarFloat
instance Shaderable Double where
	shaderType _ = ShaderScalar ShaderScalarDouble
instance Shaderable Int where
	shaderType _ = ShaderScalar ShaderScalarInt
instance Shaderable Word where
	shaderType _ = ShaderScalar ShaderScalarUint
instance Shaderable Bool where
	shaderType _ = ShaderScalar ShaderScalarBool

-- instance ShaderableScalar a => Shaderable (Vec{1..4} a)
liftM concat $ forM ['1'..'4'] $ \c -> do
	let t = conT $ mkName $ "Vec" ++ [c]
	let d = conE $ mkName $ "ShaderDimension" ++ [c]
	[d|
		instance ShaderableScalar a => Shaderable ($t a) where
			shaderType _ = ShaderVector $d $ shaderScalarType (undefined :: a)
		|]

-- instance ShaderableScalar a => Shaderable (Mat{1..4}x{1..4} a)
liftM concat $ forM [(x, y) | x <- ['1'..'4'], y <- ['1'..'4']] $ \(cx, cy) -> let
	t = conT $ mkName $ "Mat" ++ [cx, 'x', cy]
	dx = conE $ mkName $ "ShaderDimension" ++ [cx]
	dy = conE $ mkName $ "ShaderDimension" ++ [cy]
	in [d|
		instance ShaderableScalar a => Shaderable ($t a) where
			shaderType _ = ShaderMatrix $dx $dy $ shaderScalarType (undefined :: a)
		|]

-- | Class of types which can be used in vertex attribute.
class Shaderable a => ShaderAttributable a where
	data ShaderAttributeDataType a :: *

instance ShaderAttributable Float where
	data ShaderAttributeDataType Float
		= ShaderAttributeFloat32
instance ShaderAttributable Int where
	data ShaderAttributeDataType Int
		= ShaderAttributeInt32
		| ShaderAttributeInt16
		| ShaderAttributeInt8
instance ShaderAttributable Word where
	data ShaderAttributeDataType Word
		= ShaderAttributeUint32
		| ShaderAttributeUint16
		| ShaderAttributeUint8

-- instance (ShaderableScalar a, ShaderAttributable a) => ShaderAttributable (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	instanceD (return [ClassP ''ShaderableScalar [VarT a], ClassP ''ShaderAttributable [VarT a]]) (appT (conT ''ShaderAttributable) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''ShaderAttributeDataType [appT (conT v) (varT a)]
			[ normalC (mkName $ "ShaderAttributeVec" ++ [c]) [return (NotStrict, AppT (ConT ''ShaderAttributeDataType) $ VarT a)]
			] []
		]

-- | Node of abstract syntax tree.
data ShaderNode a where
	ShaderNodeConst :: Shaderable a => a -> ShaderNode a
	ShaderNodeAdd :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a -> ShaderNode a
	ShaderNodeSubtract :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a -> ShaderNode a
	ShaderNodeMultiply :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a -> ShaderNode a
	ShaderNodeDivide :: (Shaderable a, Fractional a) => ShaderNode a -> ShaderNode a -> ShaderNode a
	ShaderNodeRecip :: (Shaderable a, Fractional a) => ShaderNode a -> ShaderNode a
	ShaderNodeNegate :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a
	ShaderNodeAbs :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a
	ShaderNodeSignum :: (Shaderable a, Num a) => ShaderNode a -> ShaderNode a
	ShaderNodeMul :: Mul a b c => ShaderNode a -> ShaderNode b -> ShaderNode c
	ShaderAttribute :: Shaderable a => ShaderAttributeDataType a -> Int -> ShaderNode a
	ShaderNodeTemp :: Shaderable a => Int -> ShaderNode a

instance (Shaderable a, Num a) => Num (ShaderNode a) where
	(+) = ShaderNodeAdd
	(*) = ShaderNodeMultiply
	(-) = ShaderNodeSubtract
	negate = ShaderNodeNegate
	abs = ShaderNodeAbs
	signum = ShaderNodeSignum
	fromInteger = ShaderNodeConst . fromInteger

instance (Shaderable a, Fractional a) => Fractional (ShaderNode a) where
	(/) = ShaderNodeDivide
	recip = ShaderNodeRecip
	fromRational = ShaderNodeConst . fromRational

instance (Shaderable a, Shaderable b, Shaderable c, Mul a b c) => Mul (ShaderNode a) (ShaderNode b) (ShaderNode c) where
	mul = ShaderNodeMul

-- | Class of shader generator.
class ShaderGenerator g where
	shaderRegisterAttribute :: ShaderAttributable a => g -> ShaderAttributeDataType a -> Int -> IO (ShaderNode a)
	shaderRegisterValue :: Shaderable a => g -> ShaderNode a -> IO Int

-- | Shader monad.
data ShaderM g a = ShaderM (g -> IO a)

instance Functor (ShaderM g) where
	fmap f (ShaderM h) = ShaderM $ \g -> liftM f $ h g

instance Applicative (ShaderM g) where
	pure a = ShaderM $ \_g -> return a
	(ShaderM f) <*> (ShaderM h) = ShaderM $ \g -> do
		r1 <- h g
		r2 <- f g
		return $ r2 r1

instance Monad (ShaderM g) where
	return a = ShaderM $ \_g -> return a
	(>>=) (ShaderM f1) q = ShaderM $ \g -> do
		r1 <- f1 g
		let ShaderM f2 = q r1
		f2 g

-- | Create attribute.
attribute :: (ShaderAttributable a, ShaderGenerator g) => ShaderAttributeDataType a -> Int -> ShaderM g (ShaderNode a)
attribute dataType offset = ShaderM $ \g -> shaderRegisterAttribute g dataType offset

-- | Perform calculation (create temporary value).
calc :: (Shaderable a, ShaderGenerator g) => ShaderNode a -> ShaderM g (ShaderNode a)
calc a = ShaderM $ \g -> do
	index <- shaderRegisterValue g a
	return $ ShaderNodeTemp index
