{-|
Module: Flaw.Exception
Description: Useful things for exceptions.
License: MIT
-}

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GADTs, StandaloneDeriving #-}

module Flaw.Exception
	( DescribeException(..)
	, describeException
	) where

import Control.Exception
import qualified Control.Exception.Lifted as Lifted
import Control.Monad.Trans.Control
import Data.Typeable

-- | Exception data wrapping exception with textual description.
data DescribeException where
	DescribeFirstException :: Show a => a -> DescribeException
	DescribeException :: Show a => a -> SomeException -> DescribeException
deriving instance Show DescribeException
deriving instance Typeable DescribeException

instance Exception DescribeException

-- | Wrap possible exceptions with textual description.
describeException :: (MonadBaseControl IO m, Show msg) => msg -> m a -> m a
describeException message work = Lifted.catch work $ \e -> Lifted.throwIO $ DescribeException message e
