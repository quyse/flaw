{-|
Module: Flaw.Editor.Processing
Description: Entity processing.
License: MIT
-}

{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Flaw.Editor.Processing
	( ProcessingContext(..)
	, ProcessingException(..)
	, ProcessingM(..)
	, ProcessableEntity(..)
	, readEntityPtr
	, readInterfacedEntityPtr
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Serialize as S
import Data.Typeable

import Flaw.Editor.BlobService
import Flaw.Editor.Entity

data ProcessingContext = ProcessingContext
	{ processingContextEntityManager :: !EntityManager
	, processingContextBlobService :: !BlobService
	}

data ProcessingException
	-- | General processing error.
	= ProcessingException SomeException
	-- | Error processing prerequisite.
	| ProcessingPrerequisiteException ProcessingException

-- | Processing monad.
newtype ProcessingM a = ProcessingM (ExceptT ProcessingException (ReaderT ProcessingContext IO) a) deriving
	( Functor
	, Applicative
	, Monad
	, MonadIO
	, MonadError ProcessingException
	, MonadReader ProcessingContext
	)

-- | Entity which require processing.
-- Entity's result can be cached and reused instead of processing entity again.
class (Entity a, S.Serialize (ProcessableEntityResult a)) => ProcessableEntity a where
	{-# MINIMAL #-}
	type ProcessableEntityResult a :: *
	type ProcessableEntityResult a = a
	processEntity :: a -> ProcessingM (ProcessableEntityResult a)
	default processEntity :: a -> ProcessingM a
	processEntity = return

instance EntityInterface ProcessableEntity where
	getEntityInterfaceId _ = $(hashTextToEntityInterfaceId "ProcessableEntity")

-- | Read entity pointer.
readEntityPtr :: Entity a => EntityPtr a -> ProcessingM a
readEntityPtr (EntityPtr entityId) = do
	ProcessingContext
		{ processingContextEntityManager = entityManager
		} <- ask
	SomeEntity entity <- liftIO $ atomically . readSomeEntityVar =<< getSomeEntityVar entityManager entityId
	case cast entity of
		Just castedEntity -> return castedEntity
		Nothing -> throwError $ ProcessingException $ SomeException EntityWrongTypeException

-- | Read interfaced entity pointer.
readInterfacedEntityPtr :: EntityInterface i => InterfacedEntityPtr i -> ProcessingM (SomeInterfacedEntity i)
readInterfacedEntityPtr (InterfacedEntityPtr entityId) = f Proxy where
	f :: EntityInterface i => Proxy i -> ProcessingM (SomeInterfacedEntity i)
	f proxy = do
		ProcessingContext
			{ processingContextEntityManager = entityManager
			} <- ask
		SomeEntity entity <- liftIO $ atomically . readSomeEntityVar =<< getSomeEntityVar entityManager entityId
		case interfaceEntity proxy entity of
			EntityInterfaced -> return $ SomeInterfacedEntity entity
			EntityNotInterfaced -> throwError $ ProcessingException $ SomeException EntityWrongTypeException
