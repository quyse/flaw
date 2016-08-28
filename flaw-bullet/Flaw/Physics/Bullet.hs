{-|
Module: Flaw.Physics.Bullet
Description: Bullet Physics.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Physics.Bullet
	( BulletWorld(..)
	, initBulletWorld
	) where

import Data.Coerce
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Math
import Flaw.Physics

newtype BulletWorld = BulletWorld (Ptr C_BulletWorld)

instance World BulletWorld where

	newtype Shape BulletWorld = BulletShape (Ptr C_btCollisionShape)
	newtype Body BulletWorld = BulletBody (Ptr C_BulletRigidBody)
	newtype Ghost BulletWorld = BulletGhost (Ptr C_BulletGhost)

	createBoxShape (BulletWorld pWorld) halfSize = do
		pShape <- with halfSize $ \halfSizePtr -> flaw_bullet_newBoxShape pWorld (castPtr halfSizePtr)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createSphereShape (BulletWorld pWorld) radius = do
		pShape <- flaw_bullet_newSphereShape pWorld (coerce radius)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createConvexHullShape (BulletWorld pWorld) points = do
		pShape <- allocaArray (V.length points) $ \pPoints -> do
			V.imapM_ (pokeElemOff pPoints) points
			flaw_bullet_newConvexHullShape pWorld (castPtr pPoints) (fromIntegral $ V.length points)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createBody (BulletWorld pWorld) (BulletShape pShape) motion position (FloatQ orientation) = do
		let mass = case motion of
			MotionStatic -> 0
			MotionDynamic m -> m
		pBody <- with position $ \positionPtr -> with orientation $ \orientationPtr ->
			flaw_bullet_newRigidBody pWorld pShape (castPtr positionPtr) (castPtr orientationPtr) (coerce mass)
		return (BulletBody pBody, flaw_bullet_freeRigidBody pWorld pBody)

	getBodyTransform _ (BulletBody pBody) = alloca $ \positionPtr -> alloca $ \orientationPtr -> do
		flaw_bullet_getRigidBodyTransform pBody positionPtr orientationPtr
		position <- peek $ castPtr positionPtr
		orientation <- peek $ castPtr orientationPtr
		return (position, FloatQ orientation)

	createGhost (BulletWorld pWorld) (BulletShape pShape) position (FloatQ orientation) = do
		pGhost <- with position $ \positionPtr -> with orientation $ \orientationPtr -> flaw_bullet_newGhost pWorld pShape (castPtr positionPtr) (castPtr orientationPtr)
		return (BulletGhost pGhost, flaw_bullet_freeGhost pWorld pGhost)

	setGhostTransform (BulletWorld pWorld) (BulletGhost pGhost) position (FloatQ orientation) =
		with position $ \positionPtr -> with orientation $ \orientationPtr -> flaw_bullet_setGhostTransform pWorld pGhost (castPtr positionPtr) (castPtr orientationPtr)

	simulate (BulletWorld pWorld) step stepCount = flaw_bullet_stepWorld pWorld (coerce step) (fromIntegral stepCount)

initBulletWorld :: IO (BulletWorld, IO ())
initBulletWorld = do
	pWorld <- flaw_bullet_newWorld
	return (BulletWorld pWorld, flaw_bullet_freeWorld pWorld)

data C_BulletWorld
data C_btCollisionShape
data C_BulletRigidBody
data C_BulletGhost

foreign import ccall safe flaw_bullet_newWorld :: IO (Ptr C_BulletWorld)
foreign import ccall safe flaw_bullet_freeWorld :: Ptr C_BulletWorld -> IO ()
foreign import ccall safe flaw_bullet_newBoxShape :: Ptr C_BulletWorld -> Ptr CFloat -> IO (Ptr C_btCollisionShape)
foreign import ccall safe flaw_bullet_newSphereShape :: Ptr C_BulletWorld -> CFloat -> IO (Ptr C_btCollisionShape)
foreign import ccall safe flaw_bullet_newConvexHullShape :: Ptr C_BulletWorld -> Ptr CFloat -> CInt -> IO (Ptr C_btCollisionShape)
foreign import ccall safe flaw_bullet_freeShape :: Ptr C_btCollisionShape -> IO ()
foreign import ccall safe flaw_bullet_newRigidBody :: Ptr C_BulletWorld -> Ptr C_btCollisionShape -> Ptr CFloat -> Ptr CFloat -> CFloat -> IO (Ptr C_BulletRigidBody)
foreign import ccall safe flaw_bullet_freeRigidBody :: Ptr C_BulletWorld -> Ptr C_BulletRigidBody -> IO ()
foreign import ccall unsafe flaw_bullet_getRigidBodyTransform :: Ptr C_BulletRigidBody -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall safe flaw_bullet_newGhost :: Ptr C_BulletWorld -> Ptr C_btCollisionShape -> Ptr CFloat -> Ptr CFloat -> IO (Ptr C_BulletGhost)
foreign import ccall safe flaw_bullet_freeGhost :: Ptr C_BulletWorld -> Ptr C_BulletGhost -> IO ()
foreign import ccall unsafe flaw_bullet_setGhostTransform :: Ptr C_BulletWorld -> Ptr C_BulletGhost -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall safe flaw_bullet_stepWorld :: Ptr C_BulletWorld -> CFloat -> CInt -> IO ()
