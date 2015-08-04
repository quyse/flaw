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
	createBoxShape :: w -> Vec3f -> IO (Shape w, IO ())
	createSphereShape :: w -> Float -> IO (Shape w, IO ())
	-- | Create rigid body.
	createBody
		:: w
		-> Shape w
		-> Motion
		-> Vec3f -- ^ Initial position.
		-> Quaternionf -- ^ Initial orientation.
		-> IO (Body w, IO ())
	getBodyTransform :: w -> Body w -> IO (Vec3f, Quaternionf)
	-- | Advance simulation.
	stepWorld
		:: w
		-- | Time of step.
		-> Float
		-> IO ()
