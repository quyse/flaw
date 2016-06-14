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
	, GetEntity
	, getBaseEntityGetter
	, Deserializator
	, SomeBaseEntityGetter(..)
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
import Control.Monad.Reader
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
newtype EntityTypeId = EntityTypeId B.ByteString deriving (Eq, Ord, Monoid, Show)

-- | Entity type id length in bytes.
pattern ENTITY_TYPE_ID_SIZE = 20

-- | Null entity type id.
{-# NOINLINE nullEntityTypeId #-}
nullEntityTypeId :: EntityTypeId
nullEntityTypeId = EntityTypeId B.empty

instance S.Serialize EntityTypeId where
	put (EntityTypeId bytes) = S.putByteString bytes
	get = EntityTypeId <$> S.getBytes ENTITY_ID_SIZE

-- | Entity "pointer" is a typed entity id.
-- Doesn't keep a reference to cached entity.
-- You can read or write entity by entity pointer in IO monad.
newtype EntityPtr a = EntityPtr EntityId deriving (Eq, Ord, S.Serialize, Default, Show)

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
data NullEntity = NullEntity deriving (Typeable, Show)

instance Entity NullEntity where
	getEntityTypeId _ = nullEntityTypeId
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
	, entityManagerDeserializatorsRef :: {-# UNPACK #-} !(IORef (M.Map EntityTypeId Deserializator))
	-- | Dirty entities.
	, entityManagerDirtyRecordsVar :: {-# UNPACK #-} !(TVar (M.Map B.ByteString B.ByteString))
	-- | Is push scheduled?
	, entityManagerPushScheduledVar :: {-# UNPACK #-} !(TVar Bool)
	}

-- | Monad for deserializing entities.
type GetEntity = ReaderT (S.Get SomeBaseEntityGetter) S.Get

-- | Deserialize entity type and get entity getter for this type.
getBaseEntityGetter :: GetEntity SomeBaseEntityGetter
getBaseEntityGetter = lift =<< ask

-- | Deserializator function type.
type Deserializator = GetEntity SomeBaseEntityGetter

-- | Getter for base entity.
data SomeBaseEntityGetter where
	SomeBaseEntityGetter :: Entity a => S.Get a -> SomeBaseEntityGetter

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
	deserializatorsRef <- newIORef M.empty
	dirtyRecordsVar <- newTVarIO M.empty
	pushScheduledVar <- newTVarIO False
	return EntityManager
		{ entityManagerFlow = flow
		, entityManagerClientRepo = clientRepo
		, entityManagerPushAction = pushAction
		, entityManagerEntropyPool = entropyPool
		, entityManagerNextTagRef = nextTagRef
		, entityManagerCacheRef = cacheRef
		, entityManagerDeserializatorsRef = deserializatorsRef
		, entityManagerDirtyRecordsVar = dirtyRecordsVar
		, entityManagerPushScheduledVar = pushScheduledVar
		}

-- | Register entity type.
registerEntityType :: EntityManager -> EntityTypeId -> Deserializator -> IO ()
registerEntityType EntityManager
	{ entityManagerDeserializatorsRef = deserializatorsRef
	} entityTypeId = modifyIORef' deserializatorsRef . M.insert entityTypeId

getRootGetter :: EntityManager -> IO (S.Get SomeBaseEntityGetter)
getRootGetter EntityManager
	{ entityManagerDeserializatorsRef = deserializatorsRef
	} = do
	deserializators <- readIORef deserializatorsRef
	let rootGetter = do
		firstEntityTypeId <- S.get
		case M.lookup firstEntityTypeId deserializators of
			Just deserializator -> runReaderT deserializator rootGetter
			Nothing -> fail "unknown entity type id"
	return rootGetter

-- | Deserialize entity.
deserializeSomeEntity :: EntityManager -> EntityId -> IO SomeEntity
deserializeSomeEntity entityManager@EntityManager
	{ entityManagerClientRepo = clientRepo
	} (EntityId entityIdBytes) = do
	mainValue <- clientRepoGetValue clientRepo entityIdBytes
	rootGetter <- getRootGetter entityManager
	let eitherReturnResult = flip S.runGet mainValue $ do
		SomeBaseEntityGetter baseEntityGetter <- rootGetter
		baseEntity <- baseEntityGetter
		return $ do
			let f entity key = if key == entityIdBytes then return entity else processEntityChange entity key <$> clientRepoGetValue clientRepo key
			entity <- foldM f baseEntity =<< clientRepoGetKeysPrefixed clientRepo entityIdBytes
			return $ SomeEntity entity
	case eitherReturnResult of
		Left _ -> return $ SomeEntity NullEntity
		Right returnResult -> returnResult

-- | Provide entity manager with changes pulled from remote repo.
pullEntityManager :: EntityManager -> [(B.ByteString, B.ByteString)] -> STM ()
pullEntityManager entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerClientRepo = clientRepo
	, entityManagerCacheRef = cacheRef
	, entityManagerDirtyRecordsVar = dirtyRecordsVar
	} changes = asyncRunInFlow flow $ do
	rootGetter <- getRootGetter entityManager
	forM_ (filter ((>= ENTITY_ID_SIZE) . B.length) $ map fst changes) $ \recordKey -> do
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
								-- if it's main record
								if B.null recordKeySuffix then do
									-- get new entity type id
									let getter = do
										SomeBaseEntityGetter baseEntityGetter <- rootGetter
										let
											f :: Entity a => S.Get a -> a -> EntityTypeId
											f _ = getEntityTypeId
										let newEntityTypeId = f baseEntityGetter undefined
										-- if entity type id hasn't changed, simply process change
										if getEntityTypeId entity == newEntityTypeId then do
											newValueSuffix <- S.getBytes =<< S.remaining
											return $ do
												writeTVar entityVar entityValue
													{ entityValueEntity = SomeEntity $ processEntityChange entity B.empty newValueSuffix
													}
												return $ return ()
										else return $ return $ do
											-- re-deserialize it completely
											-- we have to do it via two STM transactions. between these transactions
											-- the only thing which can happen is user will write something into entity var (not changing a type)
											-- it will be useless anyway, and typed entity var will have to be re-typed at least
											-- so hopefully it's ok to do two transactions
											newSomeEntity <- deserializeSomeEntity entityManager entityId
											atomically $ writeTVar entityVar entityValue
												{ entityValueEntity = newSomeEntity
												}
									case S.runGet getter recordValue of
										Right stm -> stm
										Left _e -> do
											writeTVar entityVar entityValue
												{ entityValueEntity = SomeEntity NullEntity
												}
											return $ return ()
								-- else it's non-main record
								else do
									-- simply process change
									writeTVar entityVar entityValue
										{ entityValueEntity = SomeEntity $ processEntityChange entity recordKeySuffix recordValue
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
-- Current entity in the var must be of the correct type.
writeEntityVarRecord :: Entity a => EntityVar a -> B.ByteString -> B.ByteString -> STM ()
writeEntityVarRecord entityVar@EntityVar
	{ entityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyRecordsVar = dirtyRecordsVar
		}
	, entityVarEntityId = EntityId entityIdBytes
	, entityVarValueVar = entityValueVar
	} recordKeySuffix recordNewValue = do
	-- get entity
	entityValue@EntityValue
		{ entityValueEntity = SomeEntity entity
		} <- readTVar entityValueVar
	-- check that current entity is the same as entity var underlying type
	let
		castEntityForVar :: (Typeable a, Typeable b) => EntityVar a -> b -> Maybe a
		castEntityForVar _ = cast
	case castEntityForVar entityVar entity of
		-- if entity is of the same type
		Just entityOfVarType -> do
			-- simply apply the change
			let newEntity = processEntityChange entityOfVarType recordKeySuffix recordNewValue
			-- write to var
			writeTVar entityValueVar $! entityValue
				{ entityValueEntity = SomeEntity newEntity
				}
			-- write record
			let newRecordValue =
				if B.null recordKeySuffix then let
					EntityTypeId entityTypeIdBytes = getEntityTypeId newEntity
					in entityTypeIdBytes <> recordNewValue
				else recordNewValue
			modifyTVar' dirtyRecordsVar $ M.insert (entityIdBytes <> recordKeySuffix) newRecordValue
		-- else entity is of another type
		Nothing -> throwSTM EntityVarWrongTypeException
	-- schedule push
	scheduleEntityManagerPush entityManager

-- | Write basic entity into entity var.
-- Entity is replaced completely. Var may contain entity of wrong type prior to operation.
writeBasicEntityVar :: BasicEntity a => EntityVar a -> a -> STM ()
writeBasicEntityVar EntityVar
	{ entityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyRecordsVar = dirtyRecordsVar
		}
	, entityVarEntityId = EntityId entityIdBytes
	, entityVarValueVar = entityValueVar
	} newEntity = do
	-- modify var
	modifyTVar' entityValueVar $ \entityValue -> entityValue
		{ entityValueEntity = SomeEntity newEntity
		}
	-- write record
	let EntityTypeId entityTypeIdBytes = getEntityTypeId newEntity
	modifyTVar' dirtyRecordsVar $ M.insert entityIdBytes $ entityTypeIdBytes <> serializeBasicEntity newEntity
	-- schedule push
	scheduleEntityManagerPush entityManager

data EntityException
	= EntityVarWrongTypeException
	deriving (Eq, Show)

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
