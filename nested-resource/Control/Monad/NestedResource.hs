module Control.Monad.NestedResource
  ( NestedResource
  , runNestedResource
  , nestIO
  , nestIO_
  , nestedLift
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Concurrent.Async (waitCatch, withAsync)
import           Control.Exception (SomeException, throwIO)

import           Control.Monad (MonadPlus (..), void)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as MF
import           Control.Monad.IO.Class (MonadIO, liftIO)


-- NestedResource is opaque.
newtype NestedResource a
  = NestedResource { nestedResource :: NestedState -> IO (a, NestedState) }

runNestedResource :: NestedResource a -> IO a
runNestedResource m = do
  (a, s) <- nestedResource m (NestedState [])
  mapM_ tryIO $ undoStack s
  pure a

nestIO :: IO a -> (a -> IO ()) -> NestedResource a
nestIO acquire release = do
  either cleanupAndThrow success =<< tryIO acquire
  where
    success value =
      NestedResource $ \ s ->
        pure (value, s { undoStack = release value : undoStack s })


nestIO_ :: IO a -> (a -> IO ()) -> NestedResource ()
nestIO_ action = void . nestIO action

nestedLift :: IO a -> NestedResource a
nestedLift action = either cleanupAndThrow pure =<< tryIO action

-- -----------------------------------------------------------------------------
-- Private data structures and internal helpers.

data NestedState = NestedState
  { undoStack :: ![IO ()] -- A stack of undo actions.
  }

-- Run all undo actions and rethrow the exception.
cleanupAndThrow :: SomeException -> NestedResource a
cleanupAndThrow expn = do
  runUndoActions
  liftIO $ throwIO expn

-- When running in the `NestedResource` and we get a failure, we need to run
-- all the undo actions in the stack and then clear the state.
runUndoActions :: NestedResource ()
runUndoActions = NestedResource $ \ s -> do
  mapM_ tryIO $ undoStack s
  pure ((), NestedState [])

runUndoAndThrow :: NestedState -> SomeException -> IO a
runUndoAndThrow s expn = do
  mapM_ tryIO $ undoStack s
  throwIO expn

tryIO :: MonadIO m => IO a -> m (Either SomeException a)
tryIO action = liftIO $ withAsync action waitCatch

runIO :: NestedState -> IO (a, NestedState) -> IO (a, NestedState)
runIO s action = do
  either (runUndoAndThrow s) pure =<< withAsync action waitCatch

-- -----------------------------------------------------------------------------
-- Type class instances.

instance Functor NestedResource where
  {-# INLINE fmap #-}
  fmap f (NestedResource m) = NestedResource $ \ s ->
    fmap (\ (a, s') -> (f a, s')) $ runIO s (m s)

instance Applicative NestedResource where
  {-# INLINE pure #-}
  pure a = NestedResource $ \ s -> pure (a, s)

  {-# INLINE (<*>) #-}
  NestedResource mf <*> NestedResource mx = NestedResource $ \ s -> do
    (f, s') <- runIO s $ mf s
    (x, s'') <- runIO s $ mx s'
    pure (f x, s'')

  (*>) = (>>)

instance Alternative NestedResource where
  {-# INLINE empty #-}
  empty = runUndoActions >> liftIO mzero

  {-# INLINE (<|>) #-}
  NestedResource m <|> NestedResource n =
    NestedResource $ \ s -> m s `mplus` n s

instance Monad NestedResource where
  return = pure

  {-# INLINE (>>=) #-}
  NestedResource m >>= k = NestedResource $ \ s -> do
    (a, s') <- runIO s (m s)
    nestedResource (k a) s'

  {-# INLINE fail #-}
  fail str = runUndoActions >> liftIO (fail str)

instance MonadFail NestedResource where
  {-# INLINE fail #-}
  fail str = runUndoActions >> liftIO (fail str)

instance MonadPlus NestedResource where
  {-# INLINE mzero #-}
  mzero = runUndoActions >> liftIO mzero

  {-# INLINE mplus #-}
  NestedResource m `mplus` NestedResource n =
    NestedResource $ \ s -> m s `mplus` n s

instance MonadIO NestedResource where
  {-# INLINE liftIO #-}
  liftIO m =
    NestedResource $ \ s ->
      either (runUndoAndThrow s) (\ a -> pure (a, s)) =<< tryIO m
