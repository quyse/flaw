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
import qualified Data.Vector.Generic as VG
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Math
import Flaw.Math.Transform
import Flaw.Physics

newtype BulletWorld = BulletWorld (Ptr C_BulletWorld)

instance World BulletWorld where

	newtype Shape BulletWorld = BulletShape (Ptr C_btCollisionShape)
	newtype Body BulletWorld = BulletBody (Ptr C_BulletRigidBody)
	newtype Ghost BulletWorld = BulletGhost (Ptr C_BulletGhost)
	newtype Character BulletWorld = BulletCharacter (Ptr C_BulletCharacter)

	createBoxShape (BulletWorld pWorld) halfSize = do
		pShape <- with halfSize $ \halfSizePtr -> flaw_bullet_newBoxShape pWorld (castPtr halfSizePtr)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createSphereShape (BulletWorld pWorld) radius = do
		pShape <- flaw_bullet_newSphereShape pWorld (coerce radius)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createConvexHullShape (BulletWorld pWorld) points = do
		pShape <- allocaArray (VG.length points) $ \pPoints -> do
			VG.imapM_ (pokeElemOff pPoints) points
			flaw_bullet_newConvexHullShape pWorld (castPtr pPoints) (fromIntegral $ VG.length points)
		return (BulletShape pShape, flaw_bullet_freeShape pShape)

	createBody (BulletWorld pWorld) (BulletShape pShape) motion (QuatOffset (FloatQ orientation) position) = do
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
		return $ QuatOffset (FloatQ orientation) position

	createGhost (BulletWorld pWorld) (BulletShape pShape) (QuatOffset (FloatQ orientation) position) = do
		pGhost <- with position $ \positionPtr -> with orientation $ \orientationPtr -> flaw_bullet_newGhost pWorld pShape (castPtr positionPtr) (castPtr orientationPtr)
		return (BulletGhost pGhost, flaw_bullet_freeGhost pWorld pGhost)

	setGhostTransform (BulletWorld pWorld) (BulletGhost pGhost) (QuatOffset (FloatQ orientation) position) =
		with position $ \positionPtr -> with orientation $ \orientationPtr -> flaw_bullet_setGhostTransform pWorld pGhost (castPtr positionPtr) (castPtr orientationPtr)

	createCharacter (BulletWorld pWorld) (BulletShape pShape) maxStepHeight (QuatOffset (FloatQ orientation) position) = do
		pCharacter <- with position $ \positionPtr -> with orientation $ \orientationPtr -> flaw_bullet_newCharacter pWorld pShape (coerce maxStepHeight) (castPtr positionPtr) (castPtr orientationPtr)
		return (BulletCharacter pCharacter, flaw_bullet_freeCharacter pWorld pCharacter)

	walkCharacter (BulletWorld pWorld) (BulletCharacter pCharacter) movement =
		with movement $ \movementPtr -> flaw_bullet_walkCharacter pWorld pCharacter (castPtr movementPtr)

	getCharacterTransform _ (BulletCharacter pCharacter) = alloca $ \positionPtr -> alloca $ \orientationPtr -> do
		flaw_bullet_getCharacterTransform pCharacter positionPtr orientationPtr
		position <- peek $ castPtr positionPtr
		orientation <- peek $ castPtr orientationPtr
		return $ QuatOffset (FloatQ orientation) position

	simulateWorld (BulletWorld pWorld) stepTime substepTime = flaw_bullet_stepWorld pWorld (coerce stepTime) (coerce substepTime)

initBulletWorld :: IO (BulletWorld, IO ())
initBulletWorld = do
	pWorld <- flaw_bullet_newWorld
	return (BulletWorld pWorld, flaw_bullet_freeWorld pWorld)

data C_BulletWorld
data C_btCollisionShape
data C_BulletRigidBody
data C_BulletGhost
data C_BulletCharacter

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
foreign import ccall safe flaw_bullet_newCharacter :: Ptr C_BulletWorld -> Ptr C_btCollisionShape -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO (Ptr C_BulletCharacter)
foreign import ccall safe flaw_bullet_freeCharacter :: Ptr C_BulletWorld -> Ptr C_BulletCharacter -> IO ()
foreign import ccall unsafe flaw_bullet_walkCharacter :: Ptr C_BulletWorld -> Ptr C_BulletCharacter -> Ptr CFloat -> IO ()
foreign import ccall unsafe flaw_bullet_getCharacterTransform :: Ptr C_BulletCharacter -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe flaw_bullet_setGhostTransform :: Ptr C_BulletWorld -> Ptr C_BulletGhost -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall safe flaw_bullet_stepWorld :: Ptr C_BulletWorld -> CFloat -> CFloat -> IO ()
