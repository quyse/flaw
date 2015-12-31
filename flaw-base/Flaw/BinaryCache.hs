{-|
Module: Flaw.BinaryCache
Description: Class of binary cache.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.BinaryCache
	( BinaryCache(..)
	, SomeBinaryCache(..)
	, NullBinaryCache(..)
	, BinaryCacheHashMap(..)
	, newBinaryCacheHashMap
	) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.IORef

class BinaryCache c where
	getCachedBinary :: c -> B.ByteString -> IO B.ByteString
	setCachedBinary :: c -> B.ByteString -> B.ByteString -> IO ()

data SomeBinaryCache where
	SomeBinaryCache :: BinaryCache c => c -> SomeBinaryCache

data NullBinaryCache = NullBinaryCache

instance BinaryCache NullBinaryCache where
	getCachedBinary _ _ = return B.empty
	setCachedBinary _ _ _ = return ()

newtype BinaryCacheHashMap = BinaryCacheHashMap (IORef (HM.HashMap B.ByteString B.ByteString))

instance BinaryCache BinaryCacheHashMap where
	getCachedBinary (BinaryCacheHashMap hmRef) key = liftM (HM.lookupDefault B.empty key) $ readIORef hmRef
	setCachedBinary (BinaryCacheHashMap hmRef) key value = modifyIORef' hmRef $ HM.insert key value

newBinaryCacheHashMap :: IO BinaryCacheHashMap
newBinaryCacheHashMap = liftM BinaryCacheHashMap $ newIORef HM.empty
