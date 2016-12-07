{-|
Module: Flaw.Physics
Description: Main file with types for physics.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Flaw.Physics
	( World(..)
	, Motion(..)
	) where

import qualified Data.Vector.Generic as VG

import Flaw.Math
import Flaw.Math.Transform

data Motion
	-- | Static body.
	= MotionStatic
	-- | Dynamic body with mass.
	| MotionDynamic Float

-- | Abstract physics world class.
class World w where
	data Shape w :: *
	data Body w :: *
	data Ghost w :: *
	createBoxShape :: w -> Float3 -> IO (Shape w, IO ())
	createSphereShape :: w -> Float -> IO (Shape w, IO ())
	createConvexHullShape :: VG.Vector v Float3 => w -> v Float3 -> IO (Shape w, IO ())
	-- | Create rigid body.
	createBody
		:: w
		-> Shape w
		-> Motion
		-> FloatQO -- ^ Initial transform.
		-> IO (Body w, IO ())
	getBodyTransform :: w -> Body w -> IO FloatQO
	-- | Create ghost object.
	createGhost :: w -> Shape w -> FloatQO -> IO (Ghost w, IO ())
	-- | Set ghost transform.
	setGhostTransform :: w -> Ghost w -> FloatQO -> IO ()
	-- | Advance simulation.
	simulateWorld
		:: w
		-> Float -- ^ Step time.
		-> Float -- ^ Substep time.
		-> IO ()
