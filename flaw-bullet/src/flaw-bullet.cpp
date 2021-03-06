#include "bullet/src/btBulletCollisionCommon.h"
#include "bullet/src/btBulletDynamicsCommon.h"
#include "bullet/src/BulletCollision/CollisionDispatch/btGhostObject.h"
#include "bullet/src/BulletDynamics/Character/btKinematicCharacterController.h"

struct BulletWorld
{
	btDefaultCollisionConfiguration* collisionConfiguration;
	btCollisionDispatcher* collisionDispatcher;
	btBroadphaseInterface* broadphase;
	btConstraintSolver* solver;
	btDynamicsWorld* dynamicsWorld;
};

struct BulletRigidBody : public btMotionState
{
	btRigidBody* rigidBody;
	btTransform transform;

	// btMotionState's methods
	void getWorldTransform(btTransform& transform) const
	{
		transform = this->transform;
	}
	void setWorldTransform(const btTransform& transform)
	{
		this->transform = transform;
	}
};

struct BulletCharacter
{
	btPairCachingGhostObject* ghost;
	btKinematicCharacterController* controller;
};

extern "C" BulletWorld* flaw_bullet_newWorld()
{
	BulletWorld* world = new BulletWorld();
	world->collisionConfiguration = new btDefaultCollisionConfiguration();
	world->collisionDispatcher = new btCollisionDispatcher(world->collisionConfiguration);
	world->broadphase = new btDbvtBroadphase();
	world->solver = new btSequentialImpulseConstraintSolver();
	world->dynamicsWorld = new btDiscreteDynamicsWorld(world->collisionDispatcher, world->broadphase, world->solver, world->collisionConfiguration);
	world->broadphase->getOverlappingPairCache()->setInternalGhostPairCallback(new btGhostPairCallback());
	world->dynamicsWorld->setGravity(btVector3(0, 0, -9.8));
	return world;
}

extern "C" void flaw_bullet_freeWorld(BulletWorld* world)
{
	delete world->dynamicsWorld;
	delete world->solver;
	delete world->broadphase;
	delete world->collisionDispatcher;
	delete world->collisionConfiguration;
	delete world;
}

extern "C" btCollisionShape* flaw_bullet_newBoxShape(BulletWorld* world, float* halfSize)
{
	return new btBoxShape(btVector3(halfSize[0], halfSize[1], halfSize[2]));
}

extern "C" btCollisionShape* flaw_bullet_newSphereShape(BulletWorld* world, float radius)
{
	return new btSphereShape(radius);
}

extern "C" btCollisionShape* flaw_bullet_newConvexHullShape(BulletWorld* world, float* points, int pointsCount)
{
	btConvexHullShape* shape = new btConvexHullShape(points, pointsCount, sizeof(float) * 3);
	shape->optimizeConvexHull();
	return shape;
}

extern "C" void flaw_bullet_freeShape(btCollisionShape* shape)
{
	delete shape;
}

extern "C" BulletRigidBody* flaw_bullet_newRigidBody(BulletWorld* world, btCollisionShape* shape, float* position, float* orientation, float mass)
{
	btVector3 localInertia(0, 0, 0);
	if(mass)
		shape->calculateLocalInertia(mass, localInertia);
	BulletRigidBody* rigidBody = new BulletRigidBody();
	rigidBody->transform = btTransform(btQuaternion(orientation[0], orientation[1], orientation[2], orientation[3]), btVector3(position[0], position[1], position[2]));
	btRigidBody::btRigidBodyConstructionInfo info(mass, rigidBody, shape, localInertia);
	rigidBody->rigidBody = new btRigidBody(info);
	world->dynamicsWorld->addRigidBody(rigidBody->rigidBody);
	return rigidBody;
}

extern "C" void flaw_bullet_freeRigidBody(BulletWorld* world, BulletRigidBody* rigidBody)
{
	world->dynamicsWorld->removeRigidBody(rigidBody->rigidBody);
	delete rigidBody->rigidBody;
	delete rigidBody;
}

extern "C" void flaw_bullet_getRigidBodyTransform(BulletRigidBody* rigidBody, float* position, float* orientation)
{
	btQuaternion q = rigidBody->transform.getRotation();
	orientation[0] = q.x();
	orientation[1] = q.y();
	orientation[2] = q.z();
	orientation[3] = q.w();
	btVector3 p = rigidBody->transform.getOrigin();
	position[0] = p.x();
	position[1] = p.y();
	position[2] = p.z();
}

extern "C" btGhostObject* flaw_bullet_newGhost(BulletWorld* world, btCollisionShape* shape, float* position, float* orientation)
{
	btGhostObject* ghostObject = new btGhostObject();
	ghostObject->setCollisionShape(shape);
	ghostObject->setWorldTransform(btTransform(btQuaternion(orientation[0], orientation[1], orientation[2], orientation[3]), btVector3(position[0], position[1], position[2])));
	world->dynamicsWorld->addCollisionObject(ghostObject);
	return ghostObject;
}

extern "C" void flaw_bullet_freeGhost(BulletWorld* world, btGhostObject* ghostObject)
{
	delete ghostObject;
}

extern "C" void flaw_bullet_setGhostTransform(BulletWorld* world, btGhostObject* ghostObject, float* position, float* orientation)
{
	ghostObject->setWorldTransform(btTransform(btQuaternion(orientation[0], orientation[1], orientation[2], orientation[3]), btVector3(position[0], position[1], position[2])));
}

extern "C" BulletCharacter* flaw_bullet_newCharacter(BulletWorld* world, btCollisionShape* shape, float maxStepHeight, float* position, float* orientation)
{
	btConvexShape* convexShape = dynamic_cast<btConvexShape*>(shape);
	if(!convexShape) return NULL;

	btPairCachingGhostObject* ghost = new btPairCachingGhostObject();
	ghost->setWorldTransform(btTransform(btQuaternion(orientation[0], orientation[1], orientation[2], orientation[3]), btVector3(position[0], position[1], position[2])));
	ghost->setCollisionShape(convexShape);
	ghost->setCollisionFlags(btCollisionObject::CF_CHARACTER_OBJECT);
	world->dynamicsWorld->addCollisionObject(ghost);

	btKinematicCharacterController* controller = new btKinematicCharacterController(ghost, convexShape, maxStepHeight, btVector3(0, 0, 1));
	world->dynamicsWorld->addAction(controller);

	BulletCharacter* character = new BulletCharacter();
	character->ghost = ghost;
	character->controller = controller;
	return character;
}

extern "C" void flaw_bullet_freeCharacter(BulletWorld* world, BulletCharacter* character)
{
	world->dynamicsWorld->removeCollisionObject(character->ghost);
	world->dynamicsWorld->removeAction(character->controller);
	delete character->controller;
	delete character->ghost;
	delete character;
}

extern "C" void flaw_bullet_walkCharacter(BulletWorld* world, BulletCharacter* character, float* movement)
{
	character->controller->setVelocityForTimeInterval(btVector3(movement[0], movement[1], movement[2]), 1);
}

extern "C" void flaw_bullet_getCharacterTransform(BulletCharacter* character, float* position, float* orientation)
{
	const btTransform& transform = character->ghost->getWorldTransform();
	btQuaternion q = transform.getRotation();
	orientation[0] = q.x();
	orientation[1] = q.y();
	orientation[2] = q.z();
	orientation[3] = q.w();
	btVector3 p = transform.getOrigin();
	position[0] = p.x();
	position[1] = p.y();
	position[2] = p.z();
}

extern "C" void flaw_bullet_stepWorld(BulletWorld* world, float stepTime, float substepTime)
{
	world->dynamicsWorld->stepSimulation(stepTime, 10, substepTime);
}
