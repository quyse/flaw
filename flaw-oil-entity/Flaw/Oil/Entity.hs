{-|
Module: Flaw.Oil.Entity
Description: Entity level of Oil.
License: MIT

Entity is a special typeclass of objects which can be stored in Oil repo, edited in Oil editor,
and transmitted via Oil protocol.

Entity can be serialized into series of key-value records, and deserialized from them.
Each entity instance is identified by 'EntityId', and this entity id is a prefix for every
database record of entity. In other words, all records related to a given entity are key-prefixed
with entity id. Part of record's key after entity id is called /key suffix/ of record.

Every entity instance has a main record: that's a record with key equal to entity id
(in other words, with null key suffix). Existence of entity instance is determined by
correctness of main record. If there's no main record or its value is incorrect,
entity instance doesn't exist.

Value of valid main record always starts with fixed-length /entity type id/, which determines
Haskell type of entity. After entity type id there could be other data, depending on type.

Values of all entity's records constitute entity's data. The only exception is main record:
entity's data includes only part of the value after entity type id.

Entities can be accessed through means of an additional layer before low-level Oil protocols, namely 'EntityManager'.
'EntityManager' performs conversion between raw records and 'Entity' values and provides caching via 'EntityVar's.

'EntityVar' is a container for entity value. 'EntityManager' ensures that all alive 'EntityVar's contains up-to-date values
(using weak references to vars). 'EntityVar' also allows for changing a value of an entity's record,
and automatically transmits that change into repo.

Due to various implementation constraints, not every operation can be done in 'STM' monad. Specifically:

* Creation of new 'EntityVar' (which includes generating new 'EntityId') or getting an entity var for an existing entity
runs in 'IO' monad.

* Reading entity value from 'EntityVar' or writing record value runs in 'STM' monad.

-}

{-# LANGUAGE DefaultSignatures, GADTs, GeneralizedNewtypeDeriving, PatternSynonyms, TemplateHaskell #-}

module Flaw.Oil.Entity
	( EntityId(..)
	, pattern ENTITY_ID_SIZE
	, nullEntityId
	, EntityTypeId(..)
	, pattern ENTITY_TYPE_ID_SIZE
	, nullEntityTypeId
	, EntityPtr(..)
	, EntityVar(..)
	, SomeEntityPtr(..)
	, SomeEntityVar(..)
	, Entity(..)
	, BasicEntity(..)
	, SomeEntity(..)
	, NullEntity(..)
	, EntityManager(..)
	, Deserializator
	, newEntityManager
	, registerEntityType
	, pullEntityManager
	, getEntityVar
	, newEntityVar
	, readEntityVar
	, readSomeEntityVar
	, writeEntityVarRecord
	, writeBasicEntityVar
	, EntityException(..)
	, hashTextToEntityTypeId
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Crypto.Hash as C
import qualified Crypto.Random.EntropyPool as C
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import Data.Default
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH
import System.Mem.Weak

import Flaw.Book
import Flaw.Build
import Flaw.Flow
import Flaw.Oil.ClientRepo

-- | Entity id.
newtype EntityId = EntityId B.ByteString deriving (Eq, Ord)

instance Show EntityId where
	show (EntityId entityIdBytes) = "EntityId \"" <> T.unpack (T.decodeUtf8 $ BA.convertToBase BA.Base64 entityIdBytes) <> "\""

instance Default EntityId where
	def = nullEntityId

-- | Entity id length in bytes.
pattern ENTITY_ID_SIZE = 20

-- | Null entity id.
{-# NOINLINE nullEntityId #-}
nullEntityId :: EntityId
nullEntityId = EntityId $ B.replicate ENTITY_ID_SIZE 0

instance S.Serialize EntityId where
	put (EntityId bytes) = S.putByteString bytes
	get = EntityId <$> S.getBytes ENTITY_ID_SIZE

-- | Entity type id.
newtype EntityTypeId = EntityTypeId B.ByteString deriving (Eq, Ord)

-- | Entity type id length in bytes.
pattern ENTITY_TYPE_ID_SIZE = 20

-- | Null entity type id.
{-# NOINLINE nullEntityTypeId #-}
nullEntityTypeId :: EntityTypeId
nullEntityTypeId = EntityTypeId $ B.replicate ENTITY_TYPE_ID_SIZE 0

instance S.Serialize EntityTypeId where
	put (EntityTypeId bytes) = S.putByteString bytes
	get = EntityTypeId <$> S.getBytes ENTITY_ID_SIZE

-- | Entity "pointer" is a typed entity id.
-- Doesn't keep a reference to cached entity.
-- You can read or write entity by entity pointer in IO monad.
newtype EntityPtr a = EntityPtr EntityId deriving (S.Serialize, Default)

-- | Entity var, stores cached entity.
-- Entity manager keeps updating the var until it GC'ed.
data EntityVar a = EntityVar
	{ entityVarEntityManager :: !EntityManager
	, entityVarEntityId :: !EntityId
	, entityVarValueVar :: {-# UNPACK #-} !(TVar EntityValue)
	}

-- | Entity value, stored in entity var.
newtype EntityValue = EntityValue
	{ entityValueEntity :: SomeEntity
	}

-- | Untyped entity pointer.
newtype SomeEntityPtr a = SomeEntityPtr EntityId deriving S.Serialize

-- | Untyped entity var.
newtype SomeEntityVar = SomeEntityVar (TVar SomeEntity)

-- | Class of repo entity.
-- Entity should be able to deserialize from any data.
-- It must not throw exceptions. It's free to ignore invalid data
-- and/or provide some default values instead.
class Typeable a => Entity a where

	{-# MINIMAL getEntityTypeId #-}

	-- | Return type id of entity.
	-- Parameter is not used and can be 'undefined'.
	getEntityTypeId :: a -> EntityTypeId

	-- | Serialize entity's data.
	serializeEntity
		:: a -- ^ Entity to serialize.
		-> (B.ByteString -> B.ByteString -> IO ()) -- ^ Write record function accepting key suffix and value.
		-> IO ()
	default serializeEntity :: BasicEntity a => a -> (B.ByteString -> B.ByteString -> IO ()) -> IO ()
	serializeEntity a f = f B.empty $ serializeBasicEntity a

	-- | Deserialize entity's data.
	deserializeEntity :: (B.ByteString -> IO B.ByteString) -> IO a
	default deserializeEntity :: BasicEntity a => (B.ByteString -> IO B.ByteString) -> IO a
	deserializeEntity f = deserializeBasicEntity <$> f B.empty

	-- | Process change in entity's data.
	processEntityChange
		:: a -- ^ Current entity value.
		-> B.ByteString -- ^ Key suffix of changed record.
		-> B.ByteString -- ^ New value of changed record.
		-> a
	default processEntityChange :: BasicEntity a => a -> B.ByteString -> B.ByteString -> a
	processEntityChange oldEntity changedKeySuffix newValue = if B.null changedKeySuffix then deserializeBasicEntity newValue else oldEntity

-- | Basic entity is an entity consisting of main record only.
class Entity a => BasicEntity a where
	-- | Serialize basic entity into main record's value.
	serializeBasicEntity :: a -> B.ByteString
	default serializeBasicEntity :: S.Serialize a => a -> B.ByteString
	serializeBasicEntity = S.encode

	-- | Deserialize basic entity from main record's value.
	deserializeBasicEntity :: B.ByteString -> a
	default deserializeBasicEntity :: (S.Serialize a, Default a) => B.ByteString -> a
	deserializeBasicEntity bytes = case S.decode bytes of
		Left _e -> def
		Right r -> r

-- | Container for any entity.
data SomeEntity where
	SomeEntity :: Entity a => a -> SomeEntity

-- | Null entity, used when no entity could be deserialized.
data NullEntity = NullEntity deriving Typeable

instance Entity NullEntity where
	getEntityTypeId _ = nullEntityTypeId
	deserializeEntity _ = return NullEntity
	serializeEntity _ _ = return ()
	processEntityChange NullEntity _ _ = NullEntity

-- | Entity manager based on client repo.
data EntityManager = EntityManager
	{ entityManagerFlow :: !Flow
	, entityManagerClientRepo :: !ClientRepo
	-- | Push action, signal that client repo has client-side changes.
	-- Any actions with client repo must be synchronized with entity manager,
	-- so they must be performed either via entity manager flow,
	-- or synchronously in this push action.
	, entityManagerPushAction :: !(IO ())
	-- | Entropy pool to generate new entity ids.
	, entityManagerEntropyPool :: !C.EntropyPool
	, entityManagerNextTagRef :: {-# UNPACK #-} !(IORef Int)
	, entityManagerCacheRef :: {-# UNPACK #-} !(IORef (M.Map EntityId CachedEntity))
	-- | Deserialization functions.
	, entityManagerDeserializatorsVar :: {-# UNPACK #-} !(TVar (M.Map EntityTypeId Deserializator))
	-- | Dirty entities.
	, entityManagerDirtyRecordsVar :: {-# UNPACK #-} !(TVar (M.Map B.ByteString B.ByteString))
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
newEntityManager
	:: ClientRepo -- ^ Underlying client repo.
	-> IO () -- ^ Push action.
	-> IO (EntityManager, IO ())
newEntityManager clientRepo pushAction = withSpecialBook $ \bk -> do
	flow <- book bk newFlow
	entropyPool <- C.createEntropyPool
	nextTagRef <- newIORef 0
	cacheRef <- newIORef M.empty
	deserializatorsVar <- newTVarIO M.empty
	dirtyRecordsVar <- newTVarIO M.empty
	pushScheduledVar <- newTVarIO False
	return EntityManager
		{ entityManagerFlow = flow
		, entityManagerClientRepo = clientRepo
		, entityManagerPushAction = pushAction
		, entityManagerEntropyPool = entropyPool
		, entityManagerNextTagRef = nextTagRef
		, entityManagerCacheRef = cacheRef
		, entityManagerDeserializatorsVar = deserializatorsVar
		, entityManagerDirtyRecordsVar = dirtyRecordsVar
		, entityManagerPushScheduledVar = pushScheduledVar
		}

-- | Register entity type.
registerEntityType :: EntityManager -> EntityTypeId -> Deserializator -> STM ()
registerEntityType EntityManager
	{ entityManagerDeserializatorsVar = deserializatorsVar
	} entityTypeId deserializator = modifyTVar' deserializatorsVar $ M.insert entityTypeId deserializator

-- | Deserialize entity.
deserializeSomeEntity :: EntityManager -> EntityId -> IO SomeEntity
deserializeSomeEntity EntityManager
	{ entityManagerClientRepo = clientRepo
	, entityManagerDeserializatorsVar = deserializatorsVar
	} (EntityId entityIdBytes) = do
	mainValue <- clientRepoGetValue clientRepo entityIdBytes
	if B.length mainValue >= ENTITY_TYPE_ID_SIZE then do
		let (entityTypeId, mainValueSuffix) = B.splitAt ENTITY_TYPE_ID_SIZE mainValue
		deserializators <- readTVarIO deserializatorsVar
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
	, entityManagerClientRepo = clientRepo
	, entityManagerCacheRef = cacheRef
	, entityManagerDirtyRecordsVar = dirtyRecordsVar
	} changes = asyncRunInFlow flow $ forM_ (filter ((>= ENTITY_ID_SIZE) . B.length) $ map fst changes) $ \recordKey -> do
	-- get entity id
	let
		(entityIdBytes, recordKeySuffix) = B.splitAt ENTITY_ID_SIZE recordKey
		entityId = EntityId entityIdBytes
	-- get cached entity
	cache <- readIORef cacheRef
	case M.lookup entityId cache of
		Just CachedEntity
			{ cachedEntityWeak = weak
			} -> do
			maybeEntityVar <- deRefWeak weak
			case maybeEntityVar of
				Just entityVar -> do
					-- note that changes from pull info contain server value,
					-- i.e. it doesn't include non-pushed-yet changes on client side
					-- so we need to read real value from client repo
					recordValue <- clientRepoGetValue clientRepo recordKey
					join $ atomically $ do
						-- update entity only if record is not dirty
						dirtyRecords <- readTVar dirtyRecordsVar
						if M.member recordKey dirtyRecords then return $ return ()
						else do
							entityValue@EntityValue
								{ entityValueEntity = SomeEntity entity
								} <- readTVar entityVar
							-- get new entity type id (of course it's valid only in case of main record)
							let (newEntityTypeIdBytes, recordValueSuffix) = B.splitAt ENTITY_TYPE_ID_SIZE recordValue
							-- if entity type has changed
							if B.null recordKeySuffix && getEntityTypeId entity /= EntityTypeId newEntityTypeIdBytes then return $ do
								-- re-deserialize it completely
								-- we have to do it via two STM transactions. between these transactions
								-- the only thing which can happen is user will write something into entity var (not changing a type)
								-- it will be useless anyway, and typed entity var will have to be re-typed at least
								-- so hopefully it's ok to do two transactions
								newSomeEntity <- deserializeSomeEntity entityManager entityId
								atomically $ writeTVar entityVar entityValue
									{ entityValueEntity = newSomeEntity
									}
							else do
								let newEntity = processEntityChange entity recordKeySuffix $ if B.null recordKeySuffix then recordValueSuffix else recordValue
								writeTVar entityVar entityValue
									{ entityValueEntity = SomeEntity newEntity
									}
								return $ return ()
				Nothing ->
					-- expired cached entity, remove it
					writeIORef cacheRef $ M.delete entityId cache
		Nothing -> return ()

scheduleEntityManagerPush :: EntityManager -> STM ()
scheduleEntityManagerPush EntityManager
	{ entityManagerFlow = flow
	, entityManagerClientRepo = clientRepo
	, entityManagerPushAction = pushAction
	, entityManagerDirtyRecordsVar = dirtyRecordsVar
	, entityManagerPushScheduledVar = pushScheduledVar
	} = do
	-- only one push must be scheduled at all times
	pushScheduled <- readTVar pushScheduledVar
	unless pushScheduled $ do
		writeTVar pushScheduledVar True
		asyncRunInFlow flow $ do
			-- atomically get dirty records
			dirtyRecords <- atomically $ do
				-- get dirty entities and clear them
				dirtyRecords <- readTVar dirtyRecordsVar
				writeTVar dirtyRecordsVar $ M.empty
				-- reset scheduled state in the same transaction
				writeTVar pushScheduledVar False
				return dirtyRecords
			-- write dirty records
			forM_ (M.toList dirtyRecords) $ \(key, value) -> clientRepoChange clientRepo key value
			-- run push action
			pushAction

cacheEntity :: EntityManager -> EntityId -> IO (EntityVar a)
cacheEntity entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerNextTagRef = nextTagRef
	, entityManagerCacheRef = cacheRef
	} entityId = do
	-- get initial value of entity
	initialEntity <- deserializeSomeEntity entityManager entityId
	-- create new var
	tag <- atomicModifyIORef' nextTagRef $ \nextVarId -> (nextVarId + 1, nextVarId)
	entityVar <- newTVarIO EntityValue
		{ entityValueEntity = initialEntity
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

-- | Get entity var for a given entity.
getEntityVar :: Entity a => EntityManager -> EntityId -> IO (EntityVar a)
getEntityVar entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerCacheRef = cacheRef
	} entityId = runInFlow flow $ do

	cache <- readIORef cacheRef

	-- function to create new cached entity var
	let cacheExistingEntityVar = cacheEntity entityManager entityId

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
newEntityVar :: Entity a => EntityManager -> IO (EntityVar a)
newEntityVar entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerClientRepo = clientRepo
	, entityManagerPushAction = pushAction
	, entityManagerEntropyPool = entropyPool
	} = runInFlow flow $ f undefined where
	f :: Entity a => a -> IO (EntityVar a)
	f u = do
		-- generate entity id
		entityIdBytes <- C.getEntropyFrom entropyPool ENTITY_ID_SIZE
		-- write entity type id
		let EntityTypeId entityTypeIdBytes = getEntityTypeId u
		clientRepoChange clientRepo entityIdBytes entityTypeIdBytes
		pushAction
		-- create cached entity var
		cacheEntity entityManager (EntityId entityIdBytes)

-- | Read entity var type safely.
-- If entity var contains entity of wrong type, throws EntityVarWrongTypeException.
readEntityVar :: Entity a => EntityVar a -> STM a
readEntityVar var = do
	SomeEntity entity <- readSomeEntityVar var
	case cast entity of
		Just correctEntity -> return correctEntity
		Nothing -> throwSTM EntityVarWrongTypeException

-- | Read untyped entity var.
readSomeEntityVar :: EntityVar a -> STM SomeEntity
readSomeEntityVar EntityVar
	{ entityVarValueVar = valueVar
	} = entityValueEntity <$> readTVar valueVar

-- | Write record for entity var.
writeEntityVarRecord :: Entity a => EntityVar a -> B.ByteString -> B.ByteString -> STM ()
writeEntityVarRecord EntityVar
	{ entityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyRecordsVar = dirtyRecordsVar
		}
	, entityVarEntityId = EntityId entityIdBytes
	, entityVarValueVar = entityValueVar
	} recordKeySuffix recordNewValue = do
	-- modify entity
	entityValue@EntityValue
		{ entityValueEntity = SomeEntity entity
		} <- readTVar entityValueVar
	let newEntity = processEntityChange entity recordKeySuffix recordNewValue
	writeTVar entityValueVar $! entityValue
		{ entityValueEntity = SomeEntity newEntity
		}
	-- make change to record
	let newRecordValue =
		if B.null recordKeySuffix then let
			EntityTypeId entityTypeIdBytes = getEntityTypeId newEntity
			in entityTypeIdBytes <> recordNewValue
		else recordNewValue
	modifyTVar' dirtyRecordsVar $ M.insert (entityIdBytes <> recordKeySuffix) newRecordValue
	-- schedule push
	scheduleEntityManagerPush entityManager

-- | Write basic entity into entity var.
writeBasicEntityVar :: BasicEntity a => EntityVar a -> a -> STM ()
writeBasicEntityVar entityVar = writeEntityVarRecord entityVar B.empty . serializeBasicEntity

data EntityException
	= EntityVarWrongTypeException
	deriving Show

instance Exception EntityException

-- | Handy function to generate compile-time entity type id out of text.
hashTextToEntityTypeId :: T.Text -> Q Exp
hashTextToEntityTypeId str = do
	-- add hash as top decl
	cnt <- runIO $ atomicModifyIORef TH.counter $ \c -> (c + 1, c)
	n <- newName $ "entityTypeIdHash_" <> show cnt
	TH.addTopDecls =<< sequence
		[ pragInlD n NoInline FunLike AllPhases
		, sigD n [t| EntityTypeId |]
		, valD (varP n) (normalB [| EntityTypeId $(embedExp (BA.convert (C.hash (T.encodeUtf8 str) :: C.Digest C.SHA1) :: B.ByteString)) |]) []
		]
	varE n
