{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Concurrent.Async.Extra where
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MSem (new, with)
import Data.Traversable 
import Control.Applicative
import Control.Monad

-- | Implementation derived from Petr Pudlák's answer on StackOverflow
--   <http://stackoverflow.com/a/18898822/230050>
sequencePool :: Traversable t => Int -> t (IO a) -> IO (t a)
sequencePool max xs = do
    sem <- new max
    runConcurrently $ traverse (Concurrently . with sem) xs
    
-- | Implementation copied from Petr Pudlák's answer on StackOverflow
--   <http://stackoverflow.com/a/18898822/230050>
mapPool :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs
    
sequenceConcurrently :: Traversable t => t (IO a) -> IO (t a)
sequenceConcurrently = runConcurrently . traverse Concurrently

-- | Create an 'Async' and pass it to itself.
fixAsync :: (Async a -> IO a) -> IO (Async a)
fixAsync f = mdo 
    this <- async $ f this
    return this

-- | Like 'fixAsync' but using 'forkOS' internally.
fixAsyncBound :: (Async a -> IO a) -> IO (Async a)
fixAsyncBound f = mdo 
    this <- asyncBound $ f this
    return this

-- | Like 'fixAsync' but using 'forkOn' internally.
fixAsyncOn :: Int -> (Async a -> IO a) -> IO (Async a)
fixAsyncOn cpu f = mdo 
    this <- asyncOn cpu $ f this
    return this

-- | Like 'fixAsync' but using 'forkIOWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
fixAsyncWithUnmask :: (Async a -> (forall b . IO b -> IO b) -> IO a) -> IO (Async a)
fixAsyncWithUnmask f = mdo 
    this <- asyncWithUnmask $ f this
    return this

-- | Like 'fixAsyncOn' but using 'forkOnWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
fixAsyncOnWithUnmask :: Int -> (Async a -> (forall b . IO b -> IO b) -> IO a) -> IO (Async a)
fixAsyncOnWithUnmask cpu f = mdo 
    this <- asyncWithUnmask $ f this
    return this

-- | Create an async that is linked to a parent. If the parent
--   dies so does this async
withParent :: Async a -> IO b -> IO (Async b)
withParent parent act = async $ link parent >> act

-- | 'Promise' is like 'Concurrently' but includes a sequential monad instance
newtype Promise a = Promise { unPromise :: IO a }
  deriving (Functor)

instance Applicative Promise where
  pure = Promise . return
  Promise f <*> Promise x = Promise $ uncurry ($) <$> concurrently f x
  
instance Alternative Promise where
  empty = Promise $ forever (threadDelay maxBound)
  Promise x <|> Promise y = Promise $ either id id <$> race x y

instance Monad Promise where
  return = pure
  Promise m >>= f = Promise $ async m >>= wait >>= unPromise . f 

instance MonadPlus Promise where
  mzero = empty
  mplus = (<|>)