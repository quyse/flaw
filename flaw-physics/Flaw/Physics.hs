{-|
Module: Flaw.Physics
Description: Main file with types for physics.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Physics
	( World(..)
	, Motion(..)
	) where

import Flaw.Math

data Motion
	-- | Static body.
	= MotionStatic
	-- | Dynamic body with mass.
	| MotionDynamic Float

-- | Abstract physics world class.
class World w where
	data Shape w :: *
	data Body w :: *
	createBoxShape :: w -> Float3 -> IO (Shape w, IO ())
	createSphereShape :: w -> Float -> IO (Shape w, IO ())
	-- | Create rigid body.
	createBody
		:: w
		-> Shape w
		-> Motion
		-> Float3 -- ^ Initial position.
		-> FloatQ -- ^ Initial orientation.
		-> IO (Body w, IO ())
	getBodyTransform :: w -> Body w -> IO (Float3, FloatQ)
	-- | Advance simulation.
	simulate
		:: w
		-> Float -- ^ Time of one step.
		-> Int -- ^ Number of steps.
		-> IO ()
