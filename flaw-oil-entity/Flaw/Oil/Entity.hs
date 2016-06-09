{-|
Module: Flaw.Oil.Entity
Description: Entity level of Oil.
License: MIT
-}

{-# LANGUAGE DefaultSignatures, GADTs, GeneralizedNewtypeDeriving, PatternSynonyms #-}

module Flaw.Oil.Entity
	( EntityId(..)
	, pattern ENTITY_ID_SIZE
	, EntityTypeId(..)
	, pattern ENTITY_TYPE_ID_SIZE
	, EntityPtr(..)
	, EntityVar(..)
	, SomeEntityPtr(..)
	, SomeEntityVar(..)
	, Entity(..)
	, SomeEntity(..)
	, NullEntity(..)
	, EntityManager(..)
	, newEntityManager
	, registerEntityType
	, pullEntityManager
	, getEntityVar
	, newEntityVar
	, readEntityVar
	, readSomeEntityVar
	, writeEntityVar
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Crypto.Random.EntropyPool
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Serialize as S
import Data.Typeable
import System.Mem.Weak

import Flaw.Book
import Flaw.Flow
import Flaw.Oil.ClientRepo

-- | Entity id.
newtype EntityId = EntityId B.ByteString deriving (Eq, Ord)

-- | Entity id length in bytes.
pattern ENTITY_ID_SIZE = 16

instance S.Serialize EntityId where
	put (EntityId bytes) = S.putByteString bytes
	get = EntityId <$> S.getBytes ENTITY_ID_SIZE

-- | Entity type id.
newtype EntityTypeId = EntityTypeId B.ByteString deriving (Eq, Ord)

-- | Entity type id length in bytes.
pattern ENTITY_TYPE_ID_SIZE = 16

instance S.Serialize EntityTypeId where
	put (EntityTypeId bytes) = S.putByteString bytes
	get = EntityTypeId <$> S.getBytes ENTITY_ID_SIZE

-- | Entity "pointer" is a typed entity id.
-- Doesn't keep a reference to cached entity.
-- You can read or write entity by repo pointer in IO monad.
newtype EntityPtr a = EntityPtr EntityId deriving S.Serialize

-- | Entity var, stores cached entity.
-- Entity manager keeps updating the var until it GC'ed.
data EntityVar a = EntityVar
	{ entityVarEntityManager :: !EntityManager
	, entityVarEntityId :: !EntityId
	, entityVarValueVar :: {-# UNPACK #-} !(TVar EntityValue)
	}

-- | Entity value, stored in entity var.
data EntityValue = EntityValue
	{ entityValueEntity :: !SomeEntity
	, entityValueDirty :: !Bool
	}

-- | Untyped entity pointer.
newtype SomeEntityPtr a = SomeEntityPtr EntityId deriving S.Serialize

-- | Untyped entity var.
newtype SomeEntityVar = SomeEntityVar (TVar SomeEntity)

-- | Class of repo entity.
class Typeable a => Entity a where

	-- | Return type id of entity.
	-- Parameter is not used and can be 'undefined'.
	getEntityTypeId :: a -> EntityTypeId

	-- | Deserialize entity's data.
	deserializeEntity :: (B.ByteString -> IO B.ByteString) -> IO (Maybe a)
	-- default implementation simply deserializes main value.
	default deserializeEntity :: S.Serialize a => (B.ByteString -> IO B.ByteString) -> IO (Maybe a)
	deserializeEntity f = do
		value <- f B.empty
		return $ case S.decode value of
			Left _e -> Nothing
			Right r -> Just r

	-- | Serialize entity's data.
	serializeEntity
		:: a -- ^ Entity to serialize.
		-> (B.ByteString -> B.ByteString -> IO ()) -- ^ Write record function accepting key suffix and value.
		-> IO ()
	-- default implementation simply serializes entity into main value.
	default serializeEntity :: S.Serialize a => a -> (B.ByteString -> B.ByteString -> IO ()) -> IO ()
	serializeEntity a f = f B.empty $ S.encode a

-- | Container for any entity.
data SomeEntity where
	SomeEntity :: Entity a => a -> SomeEntity

-- | Null entity, used when no entity could be deserialized.
data NullEntity = NullEntity deriving Typeable

instance Entity NullEntity where
	getEntityTypeId _ = EntityTypeId $ B.replicate ENTITY_TYPE_ID_SIZE 0
	deserializeEntity _ = return $ Just NullEntity
	serializeEntity _ _ = return ()

-- | Entity manager based on client repo.
data EntityManager = EntityManager
	{ entityManagerFlow :: !Flow
	, entityManagerClientRepo :: !ClientRepo
	, entityManagerPushAction :: !(IO ())
	-- | Entropy pool to generate new entity ids.
	, entityManagerEntropyPool :: !EntropyPool
	, entityManagerNextTagRef :: {-# UNPACK #-} !(IORef Int)
	, entityManagerCacheRef :: {-# UNPACK #-} !(IORef (M.Map EntityId CachedEntity))
	-- | Deserialization functions.
	, entityManagerDeserializatorsRef :: {-# UNPACK #-} !(IORef (M.Map EntityTypeId Deserializator))
	-- | Dirty entities.
	, entityManagerDirtyEntitiesVar :: {-# UNPACK #-} !(TVar (M.Map EntityId (TVar EntityValue)))
	-- | Is push scheduled?
	, entityManagerPushScheduledVar :: {-# UNPACK #-} !(TVar Bool)
	}

-- | Type of deserialization function.
type Deserializator = (B.ByteString -> IO B.ByteString) -> IO SomeEntity

-- | Entity in cache.
data CachedEntity = CachedEntity
	{ cachedEntityTag :: {-# UNPACK #-} !Int
	, cachedEntityWeak :: {-# UNPACK #-} !(Weak (TVar EntityValue))
	}

-- | Initialize entity manager.
newEntityManager :: ClientRepo -> IO () -> IO (EntityManager, IO ())
newEntityManager clientRepo pushAction = withSpecialBook $ \bk -> do
	flow <- book bk newFlow
	entropyPool <- createEntropyPool
	nextTagRef <- newIORef 0
	cacheRef <- newIORef M.empty
	deserializatorsRef <- newIORef M.empty
	dirtyEntitiesVar <- newTVarIO M.empty
	pushScheduledVar <- newTVarIO False
	return EntityManager
		{ entityManagerFlow = flow
		, entityManagerClientRepo = clientRepo
		, entityManagerPushAction = pushAction
		, entityManagerEntropyPool = entropyPool
		, entityManagerNextTagRef = nextTagRef
		, entityManagerCacheRef = cacheRef
		, entityManagerDeserializatorsRef = deserializatorsRef
		, entityManagerDirtyEntitiesVar = dirtyEntitiesVar
		, entityManagerPushScheduledVar = pushScheduledVar
		}

-- | Register entity type.
registerEntityType :: EntityManager -> EntityTypeId -> Deserializator -> STM ()
registerEntityType EntityManager
	{ entityManagerFlow = flow
	, entityManagerDeserializatorsRef = deserializatorsRef
	} entityTypeId deserializator = asyncRunInFlow flow $ modifyIORef' deserializatorsRef $ M.insert entityTypeId deserializator

-- | Deserialize entity.
deserializeSomeEntity :: EntityManager -> EntityId -> IO SomeEntity
deserializeSomeEntity EntityManager
	{ entityManagerClientRepo = clientRepo
	, entityManagerDeserializatorsRef = deserializatorsRef
	} (EntityId entityIdBytes) = do
	mainValue <- clientRepoGetValue clientRepo entityIdBytes
	if B.length mainValue >= ENTITY_TYPE_ID_SIZE then do
		let (entityTypeId, mainValueSuffix) = B.splitAt ENTITY_TYPE_ID_SIZE mainValue
		deserializators <- readIORef deserializatorsRef
		case M.lookup (EntityTypeId entityTypeId) deserializators of
			Just deserializator -> deserializator $ \keySuffix ->
				if B.null keySuffix then return mainValueSuffix
				else clientRepoGetValue clientRepo $ entityIdBytes <> keySuffix
			Nothing -> return $ SomeEntity NullEntity
	else return $ SomeEntity NullEntity

-- | Provide entity manager with changes pulled from remote repo.
pullEntityManager :: EntityManager -> [(B.ByteString, B.ByteString)] -> STM ()
pullEntityManager entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerCacheRef = cacheRef
	} changes = asyncRunInFlow flow $ do
	let entityIds = map (EntityId . head) $ group $ sort $ map (B.take ENTITY_ID_SIZE) $ filter ((>= ENTITY_ID_SIZE) . B.length) $ map fst changes
	forM_ entityIds $ \entityId -> do
		cache <- readIORef cacheRef
		case M.lookup entityId cache of
			Just CachedEntity
				{ cachedEntityWeak = weak
				} -> do
				maybeEntityVar <- deRefWeak weak
				case maybeEntityVar of
					Just entityVar -> do
						entity <- deserializeSomeEntity entityManager entityId
						atomically $ do
							value@EntityValue
								{ entityValueDirty = dirty
								} <- readTVar entityVar
							unless dirty $ writeTVar entityVar $ value
								{ entityValueEntity = entity
								}
					Nothing -> writeIORef cacheRef $ M.delete entityId cache
			Nothing -> return ()

scheduleEntityManagerPush :: EntityManager -> STM ()
scheduleEntityManagerPush EntityManager
	{ entityManagerFlow = flow
	, entityManagerClientRepo = clientRepo
	, entityManagerPushAction = pushAction
	, entityManagerDirtyEntitiesVar = dirtyEntitiesVar
	, entityManagerPushScheduledVar = pushScheduledVar
	} = do
	-- only one push must be scheduled at all times
	pushScheduled <- readTVar pushScheduledVar
	unless pushScheduled $ do
		writeTVar pushScheduledVar True
		asyncRunInFlow flow $ do
			-- atomically get dirty entities, and reset their dirtiness status
			dirtyEntities <- atomically $ do
				-- get dirty entities and clear them
				dirtyEntities <- readTVar dirtyEntitiesVar
				writeTVar dirtyEntitiesVar $ M.empty
				-- reset scheduled state in the same transaction
				writeTVar pushScheduledVar False
				-- get entity values, reset dirty state
				forM (M.toList dirtyEntities) $ \(entityId, entityVar) -> do
					value@EntityValue
						{ entityValueEntity = someEntity
						} <- readTVar entityVar
					writeTVar entityVar value
						{ entityValueDirty = False
						}
					return (entityId, someEntity)
			-- serialize and write entities into repo
			forM_ dirtyEntities $ \(EntityId entityIdBytes, SomeEntity entity) -> let
				EntityTypeId entityTypeIdBytes = getEntityTypeId entity
				in serializeEntity entity $ \keySuffix value ->
					clientRepoChange clientRepo (entityIdBytes <> keySuffix) $ if B.null keySuffix then entityTypeIdBytes <> value else value
			-- run push action
			pushAction

cacheEntity :: EntityManager -> EntityId -> SomeEntity -> IO (EntityVar a)
cacheEntity entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerNextTagRef = nextTagRef
	, entityManagerCacheRef = cacheRef
	} entityId initialEntity = do
	-- create new var
	tag <- atomicModifyIORef' nextTagRef $ \nextVarId -> (nextVarId + 1, nextVarId)
	entityVar <- newTVarIO EntityValue
		{ entityValueEntity = initialEntity
		, entityValueDirty = False
		}
	-- put it into cache
	weak <- mkWeakTVar entityVar $ weakFinalizer tag
	modifyIORef' cacheRef $ M.insert entityId CachedEntity
		{ cachedEntityTag = tag
		, cachedEntityWeak = weak
		}
	return $ EntityVar
		{ entityVarEntityManager = entityManager
		, entityVarEntityId = entityId
		, entityVarValueVar = entityVar
		}
	where
		-- finalizer for weak references
		weakFinalizer tag = atomically $ asyncRunInFlow flow $ do
			cache <- readIORef cacheRef
			case M.lookup entityId cache of
				Just CachedEntity
					{ cachedEntityTag = t
					} -> when (tag == t) $ writeIORef cacheRef $ M.delete entityId cache
				Nothing -> return ()

-- | Get repo var for a given entity.
getEntityVar :: Entity a => EntityManager -> EntityPtr a -> IO (EntityVar a)
getEntityVar entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerCacheRef = cacheRef
	} (EntityPtr entityId) = runInFlow flow $ do

	cache <- readIORef cacheRef

	-- function to create new cached entity var
	let cacheExistingEntityVar = do
		-- get initial value of entity
		initialEntity <- deserializeSomeEntity entityManager entityId
		-- create new entity var
		cacheEntity entityManager entityId initialEntity

	-- check if there's cached entity
	case M.lookup entityId cache of
		-- if there's a cached var
		Just CachedEntity
			{ cachedEntityWeak = weak
			} -> do
			-- check if it's alive
			maybeEntityVar <- deRefWeak weak
			case maybeEntityVar of
				-- if it's still alive, return it
				Just entityVar -> return $ EntityVar
					{ entityVarEntityManager = entityManager
					, entityVarEntityId = entityId
					, entityVarValueVar = entityVar
					}
				-- otherwise cached var has been garbage collected
				Nothing -> cacheExistingEntityVar
		-- otherwise there's no cached var
		Nothing -> cacheExistingEntityVar

-- | Generate entity id and create new entity var.
-- Generated entity is "empty", i.e. contains NullEntity.
newEntityVar :: EntityManager -> IO (EntityVar a)
newEntityVar entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerEntropyPool = entropyPool
	} = runInFlow flow $ do
	-- generate entity id
	entityIdBytes <- getEntropyFrom entropyPool ENTITY_ID_SIZE
	-- create cached entity var
	cacheEntity entityManager (EntityId entityIdBytes) (SomeEntity NullEntity)

-- | Read entity var type safely.
-- If repo var contains entity of wrong type, throws RepoVarWrongTypeException.
readEntityVar :: Entity a => EntityVar a -> STM a
readEntityVar var = do
	SomeEntity entity <- readSomeEntityVar var
	case cast entity of
		Just correctEntity -> return correctEntity
		Nothing -> throwSTM RepoVarWrongTypeException

-- | Read untyped entity var.
readSomeEntityVar :: EntityVar a -> STM SomeEntity
readSomeEntityVar EntityVar
	{ entityVarValueVar = valueVar
	} = entityValueEntity <$> readTVar valueVar

-- | Write entity into entity var.
writeEntityVar :: Entity a => EntityVar a -> a -> STM ()
writeEntityVar EntityVar
	{ entityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyEntitiesVar = dirtyEntitiesVar
		}
	, entityVarEntityId = entityId
	, entityVarValueVar = valueVar
	} entity = do
	modifyTVar' valueVar $ \value -> value
		{ entityValueEntity = SomeEntity entity
		, entityValueDirty = True
		}
	modifyTVar' dirtyEntitiesVar $ M.insert entityId valueVar
	scheduleEntityManagerPush entityManager

data RepoException
	= RepoVarWrongTypeException
	deriving Show

instance Exception RepoException
