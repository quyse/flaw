{-|
Module: Flaw.Book
Description: Book is a helper structure for resource management.
License: MIT
-}

module Flaw.Book
	( Book
	, newBook
	, freeBook
	, releaseBook
	, newDynamicBook
	, book
	, withBook
	, withSpecialBook
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad

newtype Book = Book (TVar [IO ()])

{-# INLINE newBook #-}
newBook :: IO Book
newBook = Book <$> newTVarIO []

-- | Free the book.
{-# INLINE freeBook #-}
freeBook :: Book -> IO ()
freeBook = join . releaseBook

-- | Return IO action freeing everything, and clear the book.
-- Returned action captures state of the book at the moment of call,
-- so it won't free resources added after.
{-# INLINE releaseBook #-}
releaseBook :: Book -> IO (IO ())
releaseBook (Book var) = return $ sequence_ =<< atomically q
	where q = do
		finalizers <- readTVar var
		writeTVar var []
		return finalizers

-- | Create a dynamic book which could be safely freed multiple times.
{-# INLINE newDynamicBook #-}
newDynamicBook :: IO (Book, IO ())
newDynamicBook = do
	bk <- newBook
	return (bk, freeBook bk)

{-# INLINE book #-}
book :: Book -> IO (a, IO ()) -> IO a
book (Book var) q = do
	(a, r) <- q
	atomically $ modifyTVar' var (r :)
	return a

{-# INLINE withBook #-}
withBook :: (Book -> IO a) -> IO a
withBook f = do
	bk <- newBook
	finally (f bk) $ freeBook bk

-- | Helper method for dealing with possible exceptions during construction of objects.
-- User function uses separate book for construction, and if exception is thrown, book got freed.
{-# INLINE withSpecialBook #-}
withSpecialBook :: (Book -> IO a) -> IO (a, IO ())
withSpecialBook f = do
	bk <- newBook
	r <- onException (f bk) (freeBook bk)
	return (r, freeBook bk)
