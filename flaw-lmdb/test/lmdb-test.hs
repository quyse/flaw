{-|
Module: Main
Description: LMDB test.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Control.Monad
import qualified Data.Serialize as S

import Flaw.Book
import Flaw.Data.Lmdb

main :: IO ()
main = withBook $ \bk -> do
	lmdb <- book bk $ lmdbOpen "test/db" (256 * 1024 * 1024)
	lmdbWrite lmdb $ \txn -> do
		forM_ [0..10000 :: Int] $ \i -> do
			lmdbPut txn (S.encode i) (S.encode i)
		lmdbCommit txn
	lmdbRead lmdb $ \txn -> do
		forM_ [0..10000 :: Int] $ \i -> do
			r <- lmdbGet txn (S.encode i)
			unless (r == Just (S.encode i)) $ fail $ show i
	lmdbWrite lmdb $ \txn -> do
		forM_ [0..10000 :: Int] $ \i -> do
			lmdbDelete txn (S.encode i)
		lmdbCommit txn
	lmdbRead lmdb $ \txn -> do
		forM_ [0..10000 :: Int] $ \i -> do
			r <- lmdbGet txn (S.encode i)
			unless (r == Nothing) $ fail $ show i
