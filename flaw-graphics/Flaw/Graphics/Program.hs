{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

module Flaw.Graphics.Program
	( ProgramM
	, AttributeFormat(..)
	, cnst
	, attribute
	, advancedAttribute
	, uniform
	, uniformArray
	, sampler1D
	, sampler2D
	, sampler3D
	, samplerCube
	, sample
	, temp
	-- re-exports from Program.Internal
	, AttributeType(..)
	, AttributeNormalization(..)
	) where

import Control.Monad.Trans.Reader

import Flaw.Graphics.Program.Internal
import Flaw.Math

-- | Program monad.
type ProgramM g a = ReaderT g IO a

cnst :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => a -> ProgramNode g s a
cnst = programNodeConst

-- | Create simple attribute (with slot = 0, divisor = 0).
attribute :: (ProgramGenerator g, AttributableType a) => Int -> AttributeFormat a -> ProgramM g (ProgramNode g VertexStage a)
attribute offset format = advancedAttribute 0 offset 0 format
-- | Create advanced attribute.
advancedAttribute :: (ProgramGenerator g, AttributableType a) => Int -> Int -> Int -> AttributeFormat a -> ProgramM g (ProgramNode g VertexStage a)
advancedAttribute slot offset divisor format = ReaderT $ \g -> programRegisterAttribute g slot offset divisor $ attributeType format

-- | Create uniform variable.
uniform :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> Int -> AttributeFormat a -> ProgramM g (ProgramNode g s a)
uniform slot offset _format = uniformArray slot offset 0
-- | Create unform array variable.
uniformArray :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> Int -> Int -> ProgramM g (ProgramNode g s a)
uniformArray slot offset size = ReaderT $ \g -> programRegisterUniform g slot offset size

-- | Create 1D sampler.
sampler1D :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> ProgramM g (ProgramSamplerNode g s a Float)
sampler1D = registerSampler ProgramSampler1D
-- | Create 2D sampler.
sampler2D :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> ProgramM g (ProgramSamplerNode g s a Vec2f)
sampler2D = registerSampler ProgramSampler2D
-- | Create 3D sampler.
sampler3D :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> ProgramM g (ProgramSamplerNode g s a Vec3f)
sampler3D = registerSampler ProgramSampler3D
-- | Create cube sampler.
samplerCube :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> ProgramM g (ProgramSamplerNode g s a Vec3f)
samplerCube = registerSampler ProgramSamplerCube

registerSampler :: (ProgramGenerator g, ProgramStage s, ProgrammableType a, ProgrammableType b) => ProgramSamplerDimension -> Int -> ProgramM g (ProgramSamplerNode g s a b)
registerSampler dimension slot = ReaderT $ \g -> programRegisterSampler g slot dimension

-- | Perform sample.
sample :: (ProgramGenerator g, ProgramStage s, ProgrammableType a, ProgrammableType b) => ProgramSamplerNode g s a b -> ProgramNode g s b -> ProgramNode g s a
sample = programNodeSample

-- | Perform calculation (create temporary value).
temp :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => ProgramNode g s a -> ProgramM g (ProgramNode g s a)
temp a = ReaderT $ \g -> programRegisterTemp g a
