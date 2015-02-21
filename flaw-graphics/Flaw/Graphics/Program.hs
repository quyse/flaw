{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

module Flaw.Graphics.Program
	( ProgramM
	, cnst
	, attribute
	, advancedAttribute
	, uniform
	, uniformArray
	, calc
	-- re-exports from Program.Internal
	, ProgramAttributeType(..)
	, ProgramAttributeNormalization(..)
	) where

import Control.Monad.Trans.Reader

import Flaw.Graphics.Program.Internal

-- | Program monad.
type ProgramM g a = ReaderT g IO a

cnst :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => a -> ProgramNode g s a
cnst = programNodeConst

-- | Create simple attribute (with slot = 0, divisor = 0).
attribute :: (ProgramGenerator g, ProgramAttributableType a) => Int -> ProgramAttributeType -> ProgramM g (ProgramNode g VertexStage a)
attribute offset attributeType = advancedAttribute 0 offset 0 attributeType
-- | Create advanced attribute.
advancedAttribute :: (ProgramGenerator g, ProgramAttributableType a) => Int -> Int -> Int -> ProgramAttributeType -> ProgramM g (ProgramNode g VertexStage a)
advancedAttribute slot offset divisor attributeType = ReaderT $ \g -> programRegisterAttribute g slot offset divisor attributeType

-- | Create uniform variable.
uniform :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> Int -> ProgramM g (ProgramNode g s a)
uniform slot offset = uniformArray slot offset 0
-- | Create unform array variable.
uniformArray :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => Int -> Int -> Int -> ProgramM g (ProgramNode g s a)
uniformArray slot offset size = ReaderT $ \g -> programRegisterUniform g slot offset size

-- | Perform calculation (create temporary value).
calc :: (ProgramGenerator g, ProgramStage s, ProgrammableType a) => ProgramNode g s a -> ProgramM g (ProgramNode g s a)
calc a = ReaderT $ \g -> programRegisterValue g a
