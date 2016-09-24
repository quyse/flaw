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

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.IORef

class BinaryCache c where
	getCachedBinary :: c -> B.ByteString -> IO (Maybe B.ByteString)
	putCachedBinary :: c -> B.ByteString -> B.ByteString -> IO ()

data SomeBinaryCache where
	SomeBinaryCache :: BinaryCache c => c -> SomeBinaryCache

data NullBinaryCache = NullBinaryCache

instance BinaryCache NullBinaryCache where
	getCachedBinary _ _ = return Nothing
	putCachedBinary _ _ _ = return ()

newtype BinaryCacheHashMap = BinaryCacheHashMap (IORef (HM.HashMap B.ByteString B.ByteString))

instance BinaryCache BinaryCacheHashMap where
	getCachedBinary (BinaryCacheHashMap hmRef) key = HM.lookup key <$> readIORef hmRef
	putCachedBinary (BinaryCacheHashMap hmRef) key value = modifyIORef' hmRef $ HM.insert key value

newBinaryCacheHashMap :: IO BinaryCacheHashMap
newBinaryCacheHashMap = BinaryCacheHashMap <$> newIORef HM.empty
