{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Control.Concurrent.Async.Lifted.Extra where
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MSem (new, with)
import Data.Traversable 
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Fix
import Control.Monad.Base

-- | Implementation derived from Petr Pudlák's answer on StackOverflow
--   <http://stackoverflow.com/a/18898822/230050>
sequencePool :: (Traversable t, MonadBaseControl IO m) 
             => Int -> t (m a) -> m (t a)
sequencePool max xs = do
    sem <- liftBase $ new max
    runConcurrently $ traverse (Concurrently . liftBaseOp_ (with sem)) xs
    
-- | Implementation copied from Petr Pudlák's answer on StackOverflow
--   <http://stackoverflow.com/a/18898822/230050>
mapPool :: (Traversable t, MonadBaseControl IO m) 
        => Int 
        -> (a -> m b) 
        -> t a 
        -> m (t b)
mapPool max f xs = do
    sem <- liftBase $ new max
    mapConcurrently (liftBaseOp_ (with sem) . f) xs
    
sequenceConcurrently :: (Traversable t, MonadBaseControl IO m) 
                     => t (m a) -> m (t a)
sequenceConcurrently = runConcurrently . traverse Concurrently

-- | Create an 'Async' and pass it to itself.
fixAsync :: (MonadFix m, MonadBaseControl IO m) 
         => (Async (StM m a) -> m a) -> m (Async (StM m a))
fixAsync f = mdo 
    this <- async $ f this
    return this

-- | Like 'fixAsync' but using 'forkOS' internally.
fixAsyncBound :: (MonadFix m, MonadBaseControl IO m) 
              => (Async (StM m a) -> m a) -> m (Async (StM m a))
fixAsyncBound f = mdo 
    this <- asyncBound $ f this
    return this

-- | Like 'fixAsync' but using 'forkOn' internally.
fixAsyncOn :: (MonadFix m, MonadBaseControl IO m) 
           => Int -> (Async (StM m a) -> m a) -> m (Async (StM m a))
fixAsyncOn cpu f = mdo 
    this <- asyncOn cpu $ f this
    return this

-- | Like 'fixAsync' but using 'forkIOWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
fixAsyncWithUnmask :: (MonadFix m, MonadBaseControl IO m) 
                   => (Async (StM m a) -> (forall b . m b -> m b) -> m a) 
                   -> m (Async (StM m a))
fixAsyncWithUnmask f = mdo 
    this <- asyncWithUnmask $ f this
    return this

-- | Like 'fixAsyncOn' but using 'forkOnWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
fixAsyncOnWithUnmask :: (MonadFix m, MonadBaseControl IO m) 
                     => Int -> (Async (StM m a) -> (forall b . m b -> m b) -> m a) 
                     -> m (Async (StM m a))
fixAsyncOnWithUnmask cpu f = mdo 
    this <- asyncWithUnmask $ f this
    return this

-- | Create an async that is linked to a parent. If the parent
--   dies so does this async
withParent :: MonadBaseControl IO m 
           => Async (StM m a) -> m b -> m (Async (StM m b))
withParent parent act = async $ link parent >> act


-- | 'Promise' is like 'Concurrently' but includes a sequential monad instance
newtype Promise (b :: * -> *) m a = Promise { unPromise :: m a }

instance (b ~ IO, Functor m) => Functor (Promise b m) where
  fmap f (Promise a) = Promise $ f <$> a

instance (b ~ IO, MonadBaseControl b m) => Applicative (Promise b m) where
  pure = Promise . return
  Promise f <*> Promise x = Promise $ uncurry ($) <$> concurrently f x
  
instance (b ~ IO, MonadBaseControl b m) => Alternative (Promise b m) where
  empty = Promise $ liftBaseWith . const $ forever (threadDelay maxBound)
  Promise x <|> Promise y = Promise $ either id id <$> race x y

instance (b ~ IO, MonadBaseControl b m) => Monad (Promise b m) where
  return = pure
  Promise m >>= f = Promise $ async m >>= wait >>= unPromise . f 

instance (b ~ IO, MonadBaseControl b m) => MonadPlus (Promise b m) where
  mzero = empty
  mplus = (<|>)
