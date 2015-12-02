{-|
Module: Flaw.Book
Description: Book is a helper structure for resource management.
License: MIT
-}

module Flaw.Book
	( Book
	, newBook
	, freeBook
	, book
	, withBook
	, withSpecialBook
	) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Book = Book (IORef [IO ()])

newBook :: IO Book
newBook = liftM Book $ newIORef []

freeBook :: Book -> IO ()
freeBook (Book ref) = do
	sequence_ =<< readIORef ref
	writeIORef ref []

book :: Book -> IO (a, IO ()) -> IO a
book (Book ref) q = do
	(a, r) <- q
	rs <- readIORef ref
	writeIORef ref $ r : rs
	return a

withBook :: (Book -> IO a) -> IO a
withBook f = do
	bk <- newBook
	finally (f bk) $ freeBook bk

-- | Helper method for dealing with possible exceptions during construction of objects.
-- User function uses separate book for construction, and if exception is thrown, book got freed.
withSpecialBook :: (Book -> IO a) -> IO (a, IO ())
withSpecialBook f = do
	bk <- newBook
	r <- onException (f bk) (freeBook bk)
	return (r, freeBook bk)
