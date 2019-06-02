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

import Control.Monad.Catch
import Control.Monad.Fail as F
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Stacking monad transformer.
newtype StackT m a = StackT (forall b. (a -> m b) -> m b)

instance Functor (StackT m) where
  {-# INLINE fmap #-}
  fmap f (StackT h) = StackT $ \q -> h $ \r -> q $ f r

instance Applicative (StackT m) where
  {-# INLINE pure #-}
  pure a = StackT $ \q -> q a
  {-# INLINE (<*>) #-}
  (StackT f) <*> (StackT h) = StackT $ \q -> h $ \r -> f $ \z -> q $ z r

instance Monad (StackT m) where
  {-# INLINE return #-}
  return a = StackT $ \q -> q a
  {-# INLINE (>>=) #-}
  (StackT h) >>= f = StackT $ \q -> h $ \r -> let StackT z = f r in z q

instance MonadFail m => MonadFail (StackT m) where
  {-# INLINE fail #-}
  fail s = StackT (F.fail s >>=)

instance MonadTrans StackT where
  {-# INLINE lift #-}
  lift a = StackT $ \q -> a >>= q

instance MonadIO m => MonadIO (StackT m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (StackT m) where
  {-# INLINE throwM #-}
  throwM e = StackT (=<< throwM e)

instance MonadCatch m => MonadCatch (StackT m) where
  {-# INLINE catch #-}
  catch (StackT h) f = StackT $ \q -> catch (h q) (\e -> let StackT z = f e in z q)

instance MonadMask m => MonadMask (StackT m) where
  {-# INLINE mask #-}
  mask f = f $ \(StackT h) -> StackT $ \q -> mask $ \restore -> restore $ h q
  {-# INLINE uninterruptibleMask #-}
  uninterruptibleMask f = f $ \(StackT h) -> StackT $ \q -> uninterruptibleMask $ \restore -> restore $ h q
  {-# INLINE generalBracket #-}
  generalBracket (StackT acquire) release f = StackT $ \q ->
    q =<< generalBracket
      (acquire return)
      (\e exitCase -> let StackT h = release e exitCase in h return)
      (\e -> let StackT h = f e in h return)

{-# INLINE after #-}
after :: Monad m => m () -> StackT m ()
after f = StackT $ \q -> do
  r <- q ()
  f
  return r

{-# INLINE runStackT #-}
runStackT :: Monad m => StackT m a -> m a
runStackT (StackT f) = f return

{-# INLINE scope #-}
scope :: Monad m => StackT m a -> StackT m a
scope (StackT f) = StackT $ \q -> q =<< f return
