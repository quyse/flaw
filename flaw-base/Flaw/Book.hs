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

import Control.Exception
import Data.IORef

newtype Book = Book (IORef [IO ()])

{-# INLINE newBook #-}
newBook :: IO Book
newBook = fmap Book $ newIORef []

-- | Free the book.
{-# INLINE freeBook #-}
freeBook :: Book -> IO ()
freeBook (Book ref) = do
	sequence_ =<< readIORef ref
	writeIORef ref []

-- | Return IO action freeing everything, and clear the book.
-- Returned action captures state of the book at the moment of call,
-- so it won't free resources added after.
{-# INLINE releaseBook #-}
releaseBook :: Book -> IO (IO ())
releaseBook (Book ref) = do
	finalizers <- readIORef ref
	writeIORef ref []
	return $ sequence_ finalizers

-- | Create a dynamic book which could be safely freed multiple times.
{-# INLINE newDynamicBook #-}
newDynamicBook :: IO (Book, IO ())
newDynamicBook = do
	bk <- newBook
	return (bk, freeBook bk)

{-# INLINE book #-}
book :: Book -> IO (a, IO ()) -> IO a
book (Book ref) q = do
	(a, r) <- q
	rs <- readIORef ref
	writeIORef ref $ r : rs
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
