{-|
Module: Flaw.Editor.Entity
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

{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, PatternSynonyms, PolyKinds, TemplateHaskell, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Editor.Entity
	(
	-- * Ids
	  EntityId(..)
	, pattern ENTITY_ID_SIZE
	, EntityTypeId(..)
	, pattern ENTITY_TYPE_ID_SIZE
	-- * Entity pointers and vars
	, SomeEntityPtr(..)
	, EntityPtr(..)
	, InterfacedEntityPtr(..)
	, SomeEntityVar(..)
	, EntityVar(..)
	, entityVarEntityId
	-- * Entity classes
	, Entity(..)
	, BasicEntity(..)
	, processBasicEntityChange
	, applyBasicEntityChange
	-- * Entity interfaces
	, EntityInterfaceId(..)
	, EntityInterface(..)
	, EntityInterfaced(..)
	-- * Entity containers
	, SomeEntity(..)
	, SomeBasicEntity(..)
	, SomeBasicOrdEntity(..)
	, SomeInterfacedEntity(..)
	, SomeEntityInterface(..)
	-- * Null things
	, NullEntity(..)
	, nullEntityId
	, nullEntityTypeId
	, nullEntityInterfaceId
	, nullInterfacedEntityPtr
	-- * Deserializers
	, getRootBaseEntity
	, getRootBaseBasicEntity
	, getRootBaseBasicOrdEntity
	, deserializeEntityInterface
	-- * Entity manager
	, EntityManager(..)
	, GetEntity
	, newEntityManager
	, unsafePullEntityManager
	-- ** Registration
	, registerEntityType
	, registerBasicEntityType
	, registerBasicOrdEntityType
	, registerEntityInterface
	-- * Entity operations
	, getSomeEntityVar
	, getEntityVar
	, newEntityVar
	, readEntityVar
	, readSomeEntityVar
	, readSomeEntityWithRevisionVar
	, writeEntityVarRecord
	, writeBasicEntityVar
	-- * Entity history
	, EntityHistoryChan(..)
	, entityVarHistory
	, readEntityHistoryChan
	-- * Miscellaneous
	, EntityException(..)
	, GenericEntityChange
	, GenericEntityDatatype(..)
	, GenericEntityConstructor(..)
	, GenericEntitySelector(..)
	, GenericEntityValue(..)
	-- * TH helpers
	, hashTextToEntityId
	, hashTextToEntityTypeId
	, hashTextToEntityInterfaceId
	, interfaceEntityExp
	, EntityRegistration(..)
	, registerEntitiesAndInterfacesExp
	-- * Re-exports for convenience
	, Revision
	, Proxy(..)
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Crypto.Random.EntropyPool as C
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Default
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Word
import GHC.Exts(Constraint)
import qualified GHC.Generics as G
import Language.Haskell.TH
import System.Mem.Weak

import Flaw.Flow
import Flaw.Oil.ClientRepo
import Flaw.Editor.Entity.Internal
import Flaw.Oil.Repo

-- | Entity id.
newtype EntityId = EntityId BS.ShortByteString deriving (Eq, Ord)

instance Show EntityId where
	show (EntityId entityIdBytes) = "EntityId \"" <> T.unpack (T.decodeUtf8 $ BA.convertToBase BA.Base64 $ BS.fromShort entityIdBytes) <> "\""

instance Default EntityId where
	def = nullEntityId

-- | Entity id length in bytes.
pattern ENTITY_ID_SIZE = 20

-- | Null entity id.
{-# NOINLINE nullEntityId #-}
nullEntityId :: EntityId
nullEntityId = EntityId $ BS.toShort $ B.replicate ENTITY_ID_SIZE 0

instance S.Serialize EntityId where
	put (EntityId bytes) = S.putShortByteString bytes
	get = EntityId <$> S.getShortByteString ENTITY_ID_SIZE

-- | Entity type id.
newtype EntityTypeId = EntityTypeId BS.ShortByteString deriving (Eq, Ord, Monoid, Show)

-- | Entity type id length in bytes.
pattern ENTITY_TYPE_ID_SIZE = 20

-- | Null entity type id.
{-# NOINLINE nullEntityTypeId #-}
nullEntityTypeId :: EntityTypeId
nullEntityTypeId = EntityTypeId BS.empty

instance S.Serialize EntityTypeId where
	put (EntityTypeId bytes) = S.putShortByteString bytes
	get = EntityTypeId <$> S.getShortByteString ENTITY_ID_SIZE

-- | Entity "pointer" is a typed entity id.
-- Doesn't keep a reference to cached entity.
-- You can read or write entity by entity pointer in IO monad.
newtype EntityPtr (a :: *) = EntityPtr EntityId deriving (Eq, Ord, S.Serialize, Default, Show)

-- | Untyped entity var, stores cached entity.
-- Entity manager keeps updating the var until it's GC'ed.
data SomeEntityVar = SomeEntityVar
	{ someEntityVarEntityManager :: !EntityManager
	, someEntityVarEntityId :: !EntityId
	, someEntityVarValueVar :: {-# UNPACK #-} !(TVar EntityValue)
	}

-- | Typed entity var.
newtype EntityVar (a :: *) = EntityVar SomeEntityVar

-- | Get entity id from entity var.
entityVarEntityId :: EntityVar a -> EntityId
entityVarEntityId (EntityVar SomeEntityVar
	{ someEntityVarEntityId = entityId
	}) = entityId

-- | Entity value, stored in entity var.
data EntityValue = EntityValue
	{ entityValueEntity :: !SomeEntity
	, entityValueRevision :: {-# UNPACK #-} !Revision
	, entityValueHistoryVar :: {-# UNPACK #-} !(TVar EntityHistory)
	}

-- | This is basically own implementation for broadcast `TChan`.
data EntityHistory where
	EntityHistoryEnd :: EntityHistory
	EntityHistoryChange :: Entity a =>
		{ entityHistoryRevision :: {-# UNPACK #-} !Revision
		, entityHistoryEntity :: !a
		, entityHistoryChange :: !(EntityChange a)
		, entityHistoryNextVar :: {-# UNPACK #-} !(TVar EntityHistory)
		} -> EntityHistory
	EntityHistoryTypeChange :: Entity a =>
		{ entityHistoryRevision :: {-# UNPACK #-} !Revision
		, entityHistoryEntity :: !a
		, entityHistoryNextVar :: {-# UNPACK #-} !(TVar EntityHistory)
		} -> EntityHistory

writeEntityChange :: Entity a => TVar EntityValue -> Revision -> a -> EntityChange a -> STM ()
writeEntityChange valueVar revision entity change = do
	nextHistoryVar <- newTVar EntityHistoryEnd
	entityValue@EntityValue
		{ entityValueHistoryVar = historyVar
		} <- readTVar valueVar
	writeTVar historyVar EntityHistoryChange
		{ entityHistoryRevision = revision
		, entityHistoryEntity = entity
		, entityHistoryChange = change
		, entityHistoryNextVar = nextHistoryVar
		}
	writeTVar valueVar entityValue
		{ entityValueEntity = SomeEntity entity
		, entityValueRevision = revision
		, entityValueHistoryVar = nextHistoryVar
		}

writeEntityTypeChange :: Entity a => TVar EntityValue -> Revision -> a -> STM ()
writeEntityTypeChange valueVar revision entity = do
	nextHistoryVar <- newTVar EntityHistoryEnd
	entityValue@EntityValue
		{ entityValueHistoryVar = historyVar
		} <- readTVar valueVar
	writeTVar historyVar EntityHistoryTypeChange
		{ entityHistoryRevision = revision
		, entityHistoryEntity = entity
		, entityHistoryNextVar = nextHistoryVar
		}
	writeTVar valueVar entityValue
		{ entityValueEntity = SomeEntity entity
		, entityValueRevision = revision
		, entityValueHistoryVar = nextHistoryVar
		}

-- | Untyped entity pointer.
newtype SomeEntityPtr a = SomeEntityPtr EntityId deriving S.Serialize

-- | Class of repo entity.
-- Entity should be able to deserialize from any data.
-- It must not throw exceptions. It's free to ignore invalid data
-- and/or provide some default values instead.
class Typeable a => Entity (a :: *) where

	{-# MINIMAL getEntityTypeId #-}

	-- | Type representing change to entity.
	type EntityChange a :: *
	type EntityChange a = GenericEntityChange a

	-- | Return type id of entity.
	-- Parameter is not used and can be 'undefined'.
	getEntityTypeId :: a -> EntityTypeId

	-- | Process change in entity's data.
	-- Changes of one specific record (with fixed key) constitute a group.
	-- Changes to different records are commutative.
	-- Must not throw exceptions.
	-- Invalid values must be processed as well as valid,
	-- keeping all the laws above, possibly recording some error state into entity.
	processEntityChange
		:: a -- ^ Current entity value.
		-> B.ByteString -- ^ Key suffix of changed record.
		-> B.ByteString -- ^ New value of changed record.
		-> Maybe (a, EntityChange a)
	default processEntityChange :: (G.Generic a, GenericEntityDatatype (G.Rep a), EntityChange a ~ GenericEntityChange a) => a -> B.ByteString -> B.ByteString -> Maybe (a, EntityChange a)
	processEntityChange oldEntity keySuffix newValue = do -- Maybe monad
		(newEntity, change) <- processGenericEntityDatatypeChange (G.from oldEntity) keySuffix newValue
		return (G.to newEntity, change)

	-- | Apply change to get new entity and write changes to entity var.
	applyEntityChange
		:: EntityVar a -- ^ Entity var.
		-> EntityChange a -- ^ Entity change.
		-> STM ()
	default applyEntityChange :: (G.Generic a, GenericEntityDatatype (G.Rep a), EntityChange a ~ GenericEntityChange a) => EntityVar a -> EntityChange a -> STM ()
	applyEntityChange = applyGenericEntityDatatypeChange

	-- | Return witness for entity that it supports specified entity interface.
	interfaceEntity :: EntityInterface i => Proxy i -> a -> EntityInterfaced i a
	interfaceEntity _ _ = EntityNotInterfaced

	-- | Return some short human-friendly textual representation of entity.
	entityToText :: a -> T.Text
	default entityToText :: Show a => a -> T.Text
	entityToText = T.pack . show

-- | Basic entity is an entity consisting of main record only.
class (Entity a, EntityChange a ~ a) => BasicEntity a where
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

-- | Implementation of 'processEntityChange' for 'BasicEntity'es.
processBasicEntityChange :: BasicEntity a => a -> B.ByteString -> B.ByteString -> Maybe (a, a)
processBasicEntityChange _oldEntity changedKeySuffix newValue =
	if B.null changedKeySuffix then
		let newEntity = deserializeBasicEntity newValue in Just (newEntity, newEntity)
	else Nothing

-- | Implementation of 'applyEntityChange' for 'BasicEntity'es.
applyBasicEntityChange :: BasicEntity a => EntityVar a -> a -> STM ()
applyBasicEntityChange = writeBasicEntityVar

-- | Container for any entity.
data SomeEntity where
	SomeEntity :: Entity a => a -> SomeEntity

-- | Container for basic entity.
data SomeBasicEntity where
	SomeBasicEntity :: BasicEntity a => a -> SomeBasicEntity

-- | Container for basic ordered entity.
data SomeBasicOrdEntity where
	SomeBasicOrdEntity :: (BasicEntity a, Ord a) => a -> SomeBasicOrdEntity

-- | Null entity, used when no entity could be deserialized.
data NullEntity = NullEntity deriving (Typeable, Show)

instance Entity NullEntity where
	getEntityTypeId _ = nullEntityTypeId
	processEntityChange NullEntity _ _ = Nothing
	applyEntityChange _ _ = return ()

instance Default NullEntity where
	def = NullEntity

-- | Entity interface id.
newtype EntityInterfaceId = EntityInterfaceId BS.ShortByteString deriving (Eq, Ord)

instance Show EntityInterfaceId where
	show (EntityInterfaceId entityInterfaceIdBytes) = "EntityInterfaceId \"" <> T.unpack (T.decodeUtf8 $ BA.convertToBase BA.Base64 $ BS.fromShort entityInterfaceIdBytes) <> "\""

instance Default EntityInterfaceId where
	def = nullEntityInterfaceId

-- | Entity interface id length in bytes.
pattern ENTITY_INTERFACE_ID_SIZE = 20

-- | Null entity interface id.
{-# NOINLINE nullEntityInterfaceId #-}
nullEntityInterfaceId :: EntityInterfaceId
nullEntityInterfaceId = EntityInterfaceId $ BS.toShort $ B.replicate ENTITY_INTERFACE_ID_SIZE 0

instance S.Serialize EntityInterfaceId where
	put (EntityInterfaceId bytes) = S.putShortByteString bytes
	get = EntityInterfaceId <$> S.getShortByteString ENTITY_INTERFACE_ID_SIZE

-- | Class of entity interface.
-- Entity interface is itself a class.
class Typeable i => EntityInterface (i :: * -> Constraint) where
	-- | Get id of entity interface.
	getEntityInterfaceId :: p i -> EntityInterfaceId

-- | Witness for entity having an interface.
data EntityInterfaced i a where
	EntityInterfaced :: (EntityInterface i, Entity a, i a) => EntityInterfaced i a
	EntityNotInterfaced :: EntityInterfaced i a

-- | Some entity supporting specified interface.
data SomeInterfacedEntity i where
	SomeInterfacedEntity :: (EntityInterface i, Entity a, i a) => a -> SomeInterfacedEntity i

-- | Entity pointer pointing at some entity which supports specified interface.
newtype InterfacedEntityPtr (i :: * -> Constraint) = InterfacedEntityPtr EntityId deriving (Eq, Ord, S.Serialize, Default, Show)

-- | Some entity interface.
data SomeEntityInterface where
	SomeEntityInterface :: EntityInterface i => Proxy i -> SomeEntityInterface

nullInterfacedEntityPtr :: InterfacedEntityPtr a
nullInterfacedEntityPtr = InterfacedEntityPtr nullEntityId

-- | Entity manager based on client repo.
data EntityManager = EntityManager
	{
	-- | Every client repo operation must be performed in this flow,
	-- even not performed by entity manager itself.
	  entityManagerFlow :: !Flow
	, entityManagerClientRepo :: !ClientRepo
	-- | Push action, signal that client repo has client-side changes.
	-- Called by entity manager in the flow.
	, entityManagerPushAction :: !(IO ())
	-- | Entropy pool to generate new entity ids.
	, entityManagerEntropyPool :: !C.EntropyPool
	, entityManagerNextTagRef :: {-# UNPACK #-} !(IORef Int)
	, entityManagerCacheRef :: {-# UNPACK #-} !(IORef (M.Map EntityId CachedEntity))
	-- | Entity deserialization functions.
	, entityManagerDeserializatorsRef :: {-# UNPACK #-} !(IORef (M.Map EntityTypeId (GetEntity SomeEntity)))
	-- | Basic entity deserialization functions.
	, entityManagerBasicDeserializatorsRef :: {-# UNPACK #-} !(IORef (M.Map EntityTypeId (GetEntity SomeBasicEntity)))
	-- | Basic ordered entity deserialization functions.
	, entityManagerBasicOrdDeserializatorsRef :: {-# UNPACK #-} !(IORef (M.Map EntityTypeId (GetEntity SomeBasicOrdEntity)))
	-- | Entity interfaces.
	, entityManagerEntityInterfacesRef :: {-# UNPACK #-} !(IORef (M.Map EntityInterfaceId (GetEntity SomeEntityInterface)))
	-- | Dirty entities.
	, entityManagerDirtyRecordsVar :: {-# UNPACK #-} !(TVar (M.Map B.ByteString B.ByteString))
	-- | Is push scheduled?
	, entityManagerPushScheduledVar :: {-# UNPACK #-} !(TVar Bool)
	}

-- | Monad for deserializing entities.
type GetEntity = ReaderT GetEntityState S.Get

data GetEntityState = GetEntityState
	{ getEntityStateGetter :: !(S.Get SomeEntity)
	, getEntityStateBasicGetter :: !(S.Get SomeBasicEntity)
	, getEntityStateBasicOrdGetter :: !(S.Get SomeBasicOrdEntity)
	, getEntityStateInterfaceGetter :: !(S.Get SomeEntityInterface)
	}

-- | Deserialize entity type and get base entity for this type.
getRootBaseEntity :: GetEntity SomeEntity
getRootBaseEntity = lift . getEntityStateGetter =<< ask

-- | Deserialize entity type and get basic base entity for this type.
getRootBaseBasicEntity :: GetEntity SomeBasicEntity
getRootBaseBasicEntity = lift . getEntityStateBasicGetter =<< ask

-- | Deserialize entity type and get basic ordered base entity for this type.
getRootBaseBasicOrdEntity :: GetEntity SomeBasicOrdEntity
getRootBaseBasicOrdEntity = lift . getEntityStateBasicOrdGetter =<< ask

-- | Deserialize entity interface.
deserializeEntityInterface :: GetEntity SomeEntityInterface
deserializeEntityInterface = lift . getEntityStateInterfaceGetter =<< ask

-- | Entity in cache.
data CachedEntity = CachedEntity
	{ cachedEntityTag :: {-# UNPACK #-} !Int
	, cachedEntityWeak :: {-# UNPACK #-} !(Weak (TVar EntityValue))
	}

-- | Initialize entity manager.
newEntityManager
	:: Flow -- ^ Flow to run operations in.
	-> ClientRepo -- ^ Underlying client repo.
	-> IO () -- ^ Push action.
	-> IO EntityManager
newEntityManager flow clientRepo pushAction = do
	entropyPool <- C.createEntropyPool
	nextTagRef <- newIORef 0
	cacheRef <- newIORef M.empty
	deserializatorsRef <- newIORef M.empty
	basicDeserializatorsRef <- newIORef M.empty
	basicOrdDeserializatorsRef <- newIORef M.empty
	entityInterfacesRef <- newIORef M.empty
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
		, entityManagerBasicDeserializatorsRef = basicDeserializatorsRef
		, entityManagerBasicOrdDeserializatorsRef = basicOrdDeserializatorsRef
		, entityManagerEntityInterfacesRef = entityInterfacesRef
		, entityManagerDirtyRecordsVar = dirtyRecordsVar
		, entityManagerPushScheduledVar = pushScheduledVar
		}

-- | Register entity type.
registerEntityType :: EntityManager -> EntityTypeId -> (GetEntity SomeEntity) -> IO ()
registerEntityType EntityManager
	{ entityManagerDeserializatorsRef = deserializatorsRef
	} entityTypeId = modifyIORef' deserializatorsRef . M.insert entityTypeId

-- | Register basic entity type.
registerBasicEntityType :: EntityManager -> EntityTypeId -> (GetEntity SomeBasicEntity) -> IO ()
registerBasicEntityType entityManager@EntityManager
	{ entityManagerBasicDeserializatorsRef = basicDeserializatorsRef
	} entityTypeId deserializator = do
	modifyIORef' basicDeserializatorsRef $ M.insert entityTypeId deserializator
	registerEntityType entityManager entityTypeId $ do
		SomeBasicEntity entity <- deserializator
		return $ SomeEntity entity

-- | Register basic ordered entity type.
registerBasicOrdEntityType :: EntityManager -> EntityTypeId -> (GetEntity SomeBasicOrdEntity) -> IO ()
registerBasicOrdEntityType entityManager@EntityManager
	{ entityManagerBasicOrdDeserializatorsRef = basicOrdDeserializatorsRef
	} entityTypeId deserializator = do
	modifyIORef' basicOrdDeserializatorsRef $ M.insert entityTypeId deserializator
	registerBasicEntityType entityManager entityTypeId $ do
		SomeBasicOrdEntity entity <- deserializator
		return $ SomeBasicEntity entity

-- | Register entity interface.
registerEntityInterface :: EntityManager -> EntityInterfaceId -> (GetEntity SomeEntityInterface) -> IO ()
registerEntityInterface EntityManager
	{ entityManagerEntityInterfacesRef = entityInterfacesRef
	} entityInterfaceId = modifyIORef' entityInterfacesRef . M.insert entityInterfaceId

internalGetEntityState :: EntityManager -> IO GetEntityState
internalGetEntityState EntityManager
	{ entityManagerDeserializatorsRef = deserializatorsRef
	, entityManagerBasicDeserializatorsRef = basicDeserializatorsRef
	, entityManagerBasicOrdDeserializatorsRef = basicOrdDeserializatorsRef
	, entityManagerEntityInterfacesRef = entityInterfacesRef
	} = do
	deserializators <- readIORef deserializatorsRef
	basicDeserializators <- readIORef basicDeserializatorsRef
	basicOrdDeserializators <- readIORef basicOrdDeserializatorsRef
	entityInterfaces <- readIORef entityInterfacesRef
	let
		f :: M.Map EntityTypeId (GetEntity a) -> S.Get a
		f ds = do
			firstEntityTypeId <- S.get
			case M.lookup firstEntityTypeId ds of
				Just deserializator -> runReaderT deserializator s
				Nothing -> fail "unknown entity type id"
		s = GetEntityState
			{ getEntityStateGetter = f deserializators
			, getEntityStateBasicGetter = f basicDeserializators
			, getEntityStateBasicOrdGetter = f basicOrdDeserializators
			, getEntityStateInterfaceGetter = do
				entityInterfaceId <- S.get
				case M.lookup entityInterfaceId entityInterfaces of
					Just deserializator -> runReaderT deserializator s
					Nothing -> fail "unknown entity interface id"
			}
	return s

-- | Combine revisions of entity's records to get entity's revision.
combineRevisions :: Revision -> Revision -> Revision
combineRevisions a b = if a > 0 && b > 0 then max a b else 0

-- | Deserialize entity.
deserializeSomeEntity :: EntityManager -> EntityId -> IO (Revision, SomeEntity)
deserializeSomeEntity entityManager@EntityManager
	{ entityManagerClientRepo = clientRepo
	} (EntityId (BS.fromShort -> entityIdBytes)) = do
	(mainRecordRevision, mainValue) <- clientRepoGetRevisionValue clientRepo entityIdBytes
	getEntity <- getEntityStateGetter <$> internalGetEntityState entityManager
	let eitherReturnResult = flip S.runGet mainValue $ do
		SomeEntity baseEntity <- getEntity
		mainValueSuffix <- S.getBytes =<< S.remaining
		return $ do
			let f (revision, entity) key = do
				(recordRevision, value) <-
					if key == entityIdBytes then return (mainRecordRevision, mainValueSuffix)
					else clientRepoGetRevisionValue clientRepo key
				return
					( combineRevisions revision recordRevision
					, maybe entity fst $ processEntityChange entity (B.drop (B.length entityIdBytes) key) value
					)
			(revision, entity) <- foldM f (mainRecordRevision, baseEntity) =<< clientRepoGetKeysPrefixed clientRepo entityIdBytes
			return (revision, SomeEntity entity)
	case eitherReturnResult of
		Left _ -> return (0, SomeEntity NullEntity)
		Right returnResult -> returnResult

-- | Provide entity manager with changes pulled from remote repo.
-- Must be called in entity manager's flow, in the same task as pulling changes into client repo.
unsafePullEntityManager :: EntityManager -> [(Revision, B.ByteString, B.ByteString)] -> IO ()
unsafePullEntityManager entityManager@EntityManager
	{ entityManagerClientRepo = clientRepo
	, entityManagerCacheRef = cacheRef
	, entityManagerDirtyRecordsVar = dirtyRecordsVar
	} changes = do
	getEntity <- getEntityStateGetter <$> internalGetEntityState entityManager
	forM_ (filter ((>= ENTITY_ID_SIZE) . B.length) $ map (\(_revision, key, _value) -> key) changes) $ \recordKey -> do
		-- get entity id
		let
			(entityIdBytes, recordKeySuffix) = B.splitAt ENTITY_ID_SIZE recordKey
			entityId = EntityId $ BS.toShort entityIdBytes
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
						(recordRevision, recordValue) <- clientRepoGetRevisionValue clientRepo recordKey
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
										SomeEntity baseEntity <- getEntity
										-- if entity type id hasn't changed, simply process change
										if getEntityTypeId entity == getEntityTypeId baseEntity then do
											newValueSuffix <- S.getBytes =<< S.remaining
											return $ do
												case processEntityChange entity B.empty newValueSuffix of
													Just (newEntity, entityChange) -> writeEntityChange entityVar recordRevision newEntity entityChange
													Nothing -> return ()
												return $ return ()
										else return $ return $ do
											-- re-deserialize it completely
											-- we have to do it via two STM transactions. between these transactions
											-- the only thing which can happen is user will write something into entity var (not changing a type)
											-- it will be useless anyway, and typed entity var will have to be re-typed at least
											-- so hopefully it's ok to do two transactions
											(newRevision, SomeEntity newEntity) <- deserializeSomeEntity entityManager entityId
											atomically $ writeEntityTypeChange entityVar newRevision newEntity
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
									case processEntityChange entity recordKeySuffix recordValue of
										Just (newEntity, entityChange) -> writeEntityChange entityVar recordRevision newEntity entityChange
										Nothing -> return ()
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

cacheEntity :: EntityManager -> EntityId -> IO SomeEntityVar
cacheEntity entityManager@EntityManager
	{ entityManagerFlow = flow
	, entityManagerNextTagRef = nextTagRef
	, entityManagerCacheRef = cacheRef
	} entityId = do
	-- get initial value of entity
	(revision, entity) <- deserializeSomeEntity entityManager entityId
	-- create new var
	tag <- atomicModifyIORef' nextTagRef $ \nextVarId -> (nextVarId + 1, nextVarId)
	historyVar <- newTVarIO EntityHistoryEnd
	entityVar <- newTVarIO EntityValue
		{ entityValueEntity = entity
		, entityValueRevision = revision
		, entityValueHistoryVar = historyVar
		}
	-- put it into cache
	weak <- mkWeakTVar entityVar $ weakFinalizer tag
	modifyIORef' cacheRef $ M.insert entityId CachedEntity
		{ cachedEntityTag = tag
		, cachedEntityWeak = weak
		}
	return SomeEntityVar
		{ someEntityVarEntityManager = entityManager
		, someEntityVarEntityId = entityId
		, someEntityVarValueVar = entityVar
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

-- | Get untyped entity var for given entity id.
getSomeEntityVar :: EntityManager -> EntityId -> IO SomeEntityVar
getSomeEntityVar entityManager@EntityManager
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
				Just entityVar -> return SomeEntityVar
					{ someEntityVarEntityManager = entityManager
					, someEntityVarEntityId = entityId
					, someEntityVarValueVar = entityVar
					}
				-- otherwise cached var has been garbage collected
				Nothing -> cacheExistingEntityVar
		-- otherwise there's no cached var
		Nothing -> cacheExistingEntityVar

-- | Get typed entity var for given entity id.
getEntityVar :: EntityManager -> EntityId -> IO (EntityVar a)
getEntityVar entityManager entityId = EntityVar <$> getSomeEntityVar entityManager entityId

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
		clientRepoChange clientRepo entityIdBytes $ BS.fromShort entityTypeIdBytes
		pushAction
		-- create cached entity var
		EntityVar <$> cacheEntity entityManager (EntityId $ BS.toShort entityIdBytes)

-- | Read entity var type safely.
-- If entity var contains entity of wrong type, throws EntityWrongTypeException.
readEntityVar :: Entity a => EntityVar a -> STM a
readEntityVar (EntityVar someEntityVar) = do
	SomeEntity entity <- readSomeEntityVar someEntityVar
	case cast entity of
		Just correctEntity -> return correctEntity
		Nothing -> throwSTM EntityWrongTypeException

-- | Read untyped entity from var.
readSomeEntityVar :: SomeEntityVar -> STM SomeEntity
readSomeEntityVar SomeEntityVar
	{ someEntityVarValueVar = valueVar
	} = entityValueEntity <$> readTVar valueVar

-- | Read untyped entity and revision from var.
readSomeEntityWithRevisionVar :: SomeEntityVar -> STM (SomeEntity, Revision)
readSomeEntityWithRevisionVar SomeEntityVar
	{ someEntityVarValueVar = valueVar
	} = do
	EntityValue
		{ entityValueEntity = someEntity
		, entityValueRevision = revision
		} <- readTVar valueVar
	return (someEntity, revision)

-- | Write record for entity var.
-- Current entity in the var must be of the correct type.
writeEntityVarRecord :: Entity a => EntityVar a -> B.ByteString -> B.ByteString -> STM ()
writeEntityVarRecord entityVar@(EntityVar SomeEntityVar
	{ someEntityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyRecordsVar = dirtyRecordsVar
		}
	, someEntityVarEntityId = EntityId entityIdBytes
	, someEntityVarValueVar = entityValueVar
	}) recordKeySuffix recordNewValue = do
	-- get entity
	EntityValue
		{ entityValueEntity = SomeEntity entity
		} <- readTVar entityValueVar
	-- check that current entity is the same as entity var underlying type
	let
		castEntityForVar :: (Typeable a, Typeable b) => EntityVar a -> b -> Maybe a
		castEntityForVar _ = cast
	case castEntityForVar entityVar entity of
		-- if entity is of the same type, simply apply the change
		Just entityOfVarType -> case processEntityChange entityOfVarType recordKeySuffix recordNewValue of
			Just (newEntity, entityChange) -> do
				-- write to var
				writeEntityChange entityValueVar 0 newEntity entityChange
				-- write record
				let newRecordValue =
					if B.null recordKeySuffix then let
						EntityTypeId entityTypeIdBytes = getEntityTypeId newEntity
						in BS.fromShort entityTypeIdBytes <> recordNewValue
					else recordNewValue
				modifyTVar' dirtyRecordsVar $ M.insert (BS.fromShort entityIdBytes <> recordKeySuffix) newRecordValue
			Nothing -> return ()
		-- else entity is of another type
		Nothing -> throwSTM EntityWrongTypeException
	-- schedule push
	scheduleEntityManagerPush entityManager

-- | Write basic entity into entity var.
-- Entity is replaced completely. Var may contain entity of wrong type prior to operation.
writeBasicEntityVar :: BasicEntity a => EntityVar a -> a -> STM ()
writeBasicEntityVar (EntityVar SomeEntityVar
	{ someEntityVarEntityManager = entityManager@EntityManager
		{ entityManagerDirtyRecordsVar = dirtyRecordsVar
		}
	, someEntityVarEntityId = EntityId entityIdBytes
	, someEntityVarValueVar = entityValueVar
	}) newEntity = do
	-- modify var
	writeEntityChange entityValueVar 0 newEntity newEntity
	-- write record
	let EntityTypeId entityTypeIdBytes = getEntityTypeId newEntity
	modifyTVar' dirtyRecordsVar $ M.insert (BS.fromShort entityIdBytes) $ BS.fromShort entityTypeIdBytes <> serializeBasicEntity newEntity
	-- schedule push
	scheduleEntityManagerPush entityManager

-- | Entity history chan is a stream of changes to entity.
newtype EntityHistoryChan a = EntityHistoryChan (TVar (TVar EntityHistory))

-- | Get entity's history chan.
entityVarHistory :: Typeable a => EntityVar a -> STM (EntityHistoryChan a)
entityVarHistory (EntityVar SomeEntityVar
	{ someEntityVarValueVar = valueVar
	}) = do
	EntityValue
		{ entityValueEntity = SomeEntity entity
		, entityValueHistoryVar = historyVar
		} <- readTVar valueVar
	-- check that entity is of correct type
	let
		f :: a -> STM (EntityHistoryChan a)
		f _ = EntityHistoryChan <$> newTVar historyVar
	case cast entity of
		Just castedEntity -> f castedEntity
		Nothing -> throwSTM EntityWrongTypeException

-- | Get history event.
-- Retries if no history event available.
-- Throws `EntityWrongTypeException` on type change events,
-- or if entity is of wrong type.
readEntityHistoryChan :: Typeable a => EntityHistoryChan a -> STM (a, EntityChange a)
readEntityHistoryChan (EntityHistoryChan chanVar) = do
	historyVar <- readTVar chanVar
	history <- readTVar historyVar
	case history of
		EntityHistoryEnd -> retry
		EntityHistoryChange
			{ entityHistoryEntity = entity
			, entityHistoryChange = change
			, entityHistoryNextVar = nextHistoryVar
			} -> let
			f :: Maybe (a :~: b) -> a -> EntityChange a -> STM (b, EntityChange b)
			f q e ec = case q of
				Just Refl -> do
					writeTVar chanVar nextHistoryVar
					return (e, ec)
				Nothing -> throwSTM EntityWrongTypeException
			in f eqT entity change
		EntityHistoryTypeChange {} -> throwSTM EntityWrongTypeException

data EntityException
	= EntityWrongTypeException
	deriving (Eq, Show)

instance Exception EntityException


type GenericEntityChange a = GenericEntityDatatypeChange (G.Rep a)

-- | Type of index of a field.
-- We support up to 256 fields.
type FieldIndex = Word8

-- | Serialize field index.
encodeFieldIndex :: FieldIndex -> B.ByteString
encodeFieldIndex = B.singleton

-- | Deserialize field index.
decodeFieldIndex :: B.ByteString -> Maybe FieldIndex
decodeFieldIndex bytes = if B.length bytes == 1 then Just $ B.head bytes else Nothing

class GenericEntityDatatype f where
	type GenericEntityDatatypeChange f :: *
	processGenericEntityDatatypeChange :: f p -> B.ByteString -> B.ByteString -> Maybe (f p, GenericEntityDatatypeChange f)
	applyGenericEntityDatatypeChange :: (Entity a, G.Generic a, G.Rep a ~ f, EntityChange a ~ GenericEntityDatatypeChange f) => EntityVar a -> EntityChange a -> STM ()

class GenericEntityConstructor f where
	type GenericEntityConstructorChange f :: *
	processGenericEntityConstructorChange :: f p -> FieldIndex -> B.ByteString -> Maybe (f p, GenericEntityConstructorChange f)
	applyGenericEntityConstructorChange :: f p -> GenericEntityConstructorChange f -> (B.ByteString, B.ByteString)

class GenericEntitySelector f where
	type GenericEntitySelectorChange f :: *
	processGenericEntitySelectorChange :: f p -> FieldIndex -> B.ByteString -> Maybe (f p, GenericEntitySelectorChange f)
	applyGenericEntitySelectorChange :: f p -> FieldIndex -> GenericEntitySelectorChange f -> (B.ByteString, B.ByteString)
	genericEntitySelectorFieldsCount :: f p -> Word8

class GenericEntityValue f where
	type GenericEntityValueChange f :: *
	processGenericEntityValueChange :: f p -> B.ByteString -> Maybe (f p, GenericEntityValueChange f)
	applyGenericEntityValueChange :: f p -> GenericEntityValueChange f -> B.ByteString

instance GenericEntityConstructor f => GenericEntityDatatype (G.M1 G.D c f) where
	type GenericEntityDatatypeChange (G.M1 G.D c f) = GenericEntityConstructorChange f

	processGenericEntityDatatypeChange oldEntity keySuffix newValue = do -- Maybe monad
		fieldIndex <- decodeFieldIndex keySuffix
		(newEntity, change) <- processGenericEntityConstructorChange (G.unM1 oldEntity) fieldIndex newValue
		return (G.M1 newEntity, change)

	applyGenericEntityDatatypeChange var change = do
		entity <- readEntityVar var
		let (keySuffix, newValue) = applyGenericEntityConstructorChange (G.unM1 $ G.from entity) change
		writeEntityVarRecord var keySuffix newValue

	{-# INLINEABLE processGenericEntityDatatypeChange #-}
	{-# INLINEABLE applyGenericEntityDatatypeChange #-}

instance GenericEntitySelector f => GenericEntityConstructor (G.M1 G.C c f) where
	type GenericEntityConstructorChange (G.M1 G.C c f) = GenericEntitySelectorChange f

	processGenericEntityConstructorChange oldEntity fieldIndex newValue = do -- Maybe monad
		(newEntity, change) <- processGenericEntitySelectorChange (G.unM1 oldEntity) fieldIndex newValue
		return (G.M1 newEntity, change)

	applyGenericEntityConstructorChange oldEntity = applyGenericEntitySelectorChange (G.unM1 oldEntity) 0

	{-# INLINEABLE processGenericEntityConstructorChange #-}
	{-# INLINEABLE applyGenericEntityConstructorChange #-}

instance GenericEntityValue f => GenericEntitySelector (G.M1 G.S c f) where
	type GenericEntitySelectorChange (G.M1 G.S c f) = GenericEntityValueChange f

	-- field index must be zero here
	processGenericEntitySelectorChange oldEntity 0 newValue = do -- Maybe monad
		(newEntity, change) <- processGenericEntityValueChange (G.unM1 oldEntity) newValue
		return (G.M1 newEntity, change)
	processGenericEntitySelectorChange _oldEntity _fieldIndex _newValue = Nothing

	genericEntitySelectorFieldsCount _ = 1

	applyGenericEntitySelectorChange oldEntity fieldIndex change = (encodeFieldIndex fieldIndex, applyGenericEntityValueChange (G.unM1 oldEntity) change)

	{-# INLINEABLE processGenericEntitySelectorChange #-}
	{-# INLINEABLE genericEntitySelectorFieldsCount #-}
	{-# INLINEABLE applyGenericEntitySelectorChange #-}

instance (GenericEntitySelector a, GenericEntitySelector b) => GenericEntitySelector (a G.:*: b) where
	type GenericEntitySelectorChange (a G.:*: b) = Either (GenericEntitySelectorChange a) (GenericEntitySelectorChange b)

	processGenericEntitySelectorChange (a G.:*: b) fieldIndex newValue = do -- Maybe monad
		let aFieldCount = genericEntitySelectorFieldsCount a
		if fieldIndex < aFieldCount then do
			(newEntity, change) <- processGenericEntitySelectorChange a fieldIndex newValue
			return (newEntity G.:*: b, Left change)
		else do
			(newEntity, change) <- processGenericEntitySelectorChange b (fieldIndex - aFieldCount) newValue
			return (a G.:*: newEntity, Right change)

	genericEntitySelectorFieldsCount (a G.:*: b) = genericEntitySelectorFieldsCount a + genericEntitySelectorFieldsCount b

	applyGenericEntitySelectorChange (a G.:*: b) fieldIndex change = case change of
		Left l -> applyGenericEntitySelectorChange a fieldIndex l
		Right r -> applyGenericEntitySelectorChange b (fieldIndex + genericEntitySelectorFieldsCount a) r

	{-# INLINEABLE processGenericEntitySelectorChange #-}
	{-# INLINEABLE genericEntitySelectorFieldsCount #-}
	{-# INLINEABLE applyGenericEntitySelectorChange #-}

-- value
instance BasicEntity a => GenericEntityValue (G.K1 G.R a) where
	type GenericEntityValueChange (G.K1 G.R a) = EntityChange a

	processGenericEntityValueChange _oldEntity newValue = Just (G.K1 newEntity, newEntity) where
		newEntity = deserializeBasicEntity newValue

	applyGenericEntityValueChange _oldEntity = serializeBasicEntity

	{-# INLINEABLE processGenericEntityValueChange #-}
	{-# INLINEABLE applyGenericEntityValueChange #-}


-- | Handy function to generate compile-time entity id out of text.
hashTextToEntityId :: T.Text -> Q Exp
hashTextToEntityId = hashTextDecl "entityIdHash_" [t| EntityId |] $ \e -> [| EntityId (BS.toShort $e) |]

-- | Handy function to generate compile-time entity type id out of text.
hashTextToEntityTypeId :: T.Text -> Q Exp
hashTextToEntityTypeId = hashTextDecl "entityTypeIdHash_" [t| EntityTypeId |] $ \e -> [| EntityTypeId (BS.toShort $e) |]

-- | Handy function to generate compile-time entity interface id out of text.
hashTextToEntityInterfaceId :: T.Text -> Q Exp
hashTextToEntityInterfaceId = hashTextDecl "entityInterfaceIdHash_" [t| EntityInterfaceId |] $ \e -> [| EntityInterfaceId (BS.toShort $e) |]

-- | Helper method for comparing interfaces' types.
-- Helps solving problems with kinds.
{-# INLINE eqEntityInterfaces #-}
eqEntityInterfaces :: (EntityInterface a, EntityInterface b) => Proxy a -> Proxy b -> Maybe ((Proxy a) :~: (Proxy b))
eqEntityInterfaces _ _ = eqT

-- | Handy 'interfaceEntity' implementation generator.
interfaceEntityExp :: [Name] -> ExpQ
interfaceEntityExp interfaceNames = do
	p <- newName "p"
	let f n q = caseE [| eqEntityInterfaces $(varE p) (Proxy :: Proxy $(conT n)) |]
		[ match [p| Just Refl |] (normalB [| EntityInterfaced |]) []
		, match [p| Nothing |] (normalB q) []
		]
	lamE [varP p, wildP] $ foldr f (conE 'EntityNotInterfaced) interfaceNames

-- | Typeclass for registration of complex entities.
class EntityRegistration (a :: k) where
	performEntityRegistration :: EntityManager -> Proxy a -> IO ()

-- | Register all simple instances of 'Entity' and 'EntityInterface',
-- perform registration from any 'EntityRegistration' instances in scope.
-- Type is :: EntityManager -> IO ()
-- Beware of TH declaration groups!
registerEntitiesAndInterfacesExp :: ExpQ
registerEntitiesAndInterfacesExp = do
	em <- newName "entityManager"

	-- simple Entity instances
	entityInstancesStmts <- do
		ClassI _ decs <- reify ''Entity
		(concat <$>) . forM decs $ \(InstanceD Nothing context (AppT _ t) _) ->
			if null context then do
				supportDefault <- isInstance ''Default [t]
				if supportDefault then do
					supportBasicEntity <- isInstance ''BasicEntity [t]
					if supportBasicEntity then do
						supportOrd <- isInstance ''Ord [t]
						if supportOrd then return [noBindS [| let a = def :: $(return t) in registerBasicOrdEntityType $(varE em) (getEntityTypeId a) $ return $ SomeBasicOrdEntity a |] ]
						else return [noBindS [| let a = def :: $(return t) in registerBasicEntityType $(varE em) (getEntityTypeId a) $ return $ SomeBasicEntity a |] ]
					else return [noBindS [| let a = def :: $(return t) in registerEntityType $(varE em) (getEntityTypeId a) $ return $ SomeEntity a |] ]
				else do
					reportWarning $ shows t " doesn't have Default instance. Cannot automatically register its Entity."
					return []
			else return []

	-- EntityInterface instances
	entityInterfaceInstancesStmts <- do
		ClassI _ decs <- reify ''EntityInterface
		(concat <$>) . forM decs $ \(InstanceD Nothing context (AppT _ t) _) ->
			if null context then return [noBindS [| registerEntityInterface $(varE em) (getEntityInterfaceId (Proxy :: Proxy $(return t))) $ return $ SomeEntityInterface (Proxy :: Proxy $(return t)) |] ]
			else return []

	-- EntityRegistration instances
	entityRegistrationInstancesStmts <- do
		ClassI _ decs <- reify ''EntityRegistration
		(concat <$>) . forM decs $ \(InstanceD Nothing context (AppT _ t) _) ->
			if null context then return [noBindS [| performEntityRegistration $(varE em) (Proxy :: Proxy $(return t)) |] ]
			else do
				reportError $ shows t " has non-empty context for EntityRegistration instance"
				return []

	lamE [varP em] $ doE $ entityInstancesStmts ++ entityInterfaceInstancesStmts ++ entityRegistrationInstancesStmts
