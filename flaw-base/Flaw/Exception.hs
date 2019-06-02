{-|
Module: Flaw.Exception
Description: Useful things for exceptions.
License: MIT
-}

{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving #-}

module Flaw.Exception
  ( DescribeException(..)
  , describeException
  ) where

import Control.Exception
import Data.Typeable

-- | Exception data wrapping exception with textual description.
data DescribeException where
  DescribeFirstException :: Show a => !a -> DescribeException
  DescribeException :: Show a => !a -> !SomeException -> DescribeException
deriving instance Show DescribeException
deriving instance Typeable DescribeException

instance Exception DescribeException

-- | Wrap possible exceptions with textual description.
{-# INLINE describeException #-}
describeException :: Show msg => msg -> IO a -> IO a
describeException message work = catch work $ \e -> throwIO $ DescribeException message e
