{-|
Module: Flaw.Stack
Description: Stack monad for faking "destructors".
License: MIT
-}

{-# LANGUAGE RankNTypes #-}

module Flaw.Stack
	( StackT(..)
	, after
	, runStackT
	, scope
	) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Stacking monad transformer.
newtype StackT m a = StackT (forall b. (a -> m b) -> m b)

instance Functor (StackT m) where
	fmap f (StackT h) = StackT $ \q -> h $ \r -> q $ f r

instance Applicative (StackT m) where
	pure a = StackT $ \q -> q a
	(StackT f) <*> (StackT h) = StackT $ \q -> h $ \r -> f $ \z -> q $ z r

instance Monad (StackT m) where
	return a = StackT $ \q -> q a
	(StackT h) >>= f = StackT $ \q -> h $ \r -> let StackT z = f r in z q

instance MonadTrans StackT where
	lift a = StackT $ \q -> a >>= q

instance MonadIO m => MonadIO (StackT m) where
	liftIO = lift . liftIO

after :: Monad m => m () -> StackT m ()
after f = StackT $ \q -> do
	r <- q ()
	f
	return r

runStackT :: Monad m => StackT m a -> m a
runStackT (StackT f) = f return

scope :: Monad m => StackT m a -> StackT m a
scope (StackT f) = StackT $ \q -> q =<< f return
