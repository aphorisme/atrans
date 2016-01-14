{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-|
Module      : Control.Monad.Backend
Description : A variant of the state transformer.
Copyright   : (c) Philipp Pfeiffer, 2016
License     : MIT
Maintainer  : pfiff@hax-f.net
Stability   : experimental
Portability : POSIX

Defines the 'BackendT' transformer, which is a variant of the 'StateT' transformer, such that the state is within an 'MVar'.
-}
module Control.Monad.Backend where



import Control.Monad.Except
import Control.Monad.State
import Control.Concurrent.MVar (MVar, isEmptyMVar, modifyMVar_, takeMVar, putMVar, readMVar)
import Control.Arrow (second)


-- | The 'BackendT' transformer is a 'StateT', where the state is encapsulated into a 'MVar'. The initialized 'MVar' has to be non empty, otherwise every state change is blocking.
newtype BackendT s m a = BackendT (MVar s -> m (MVar s, a))

-- | 'runBackendT' unwraps the 'BackendT' monad.
runBackendT :: BackendT s m a -> MVar s -> m (MVar s, a)
runBackendT (BackendT f) = f

instance (Functor m) => Functor (BackendT s m) where
  fmap f bt = BackendT $ \s -> fmap (second f) (runBackendT bt s)

instance (Monad m) => Applicative (BackendT s m) where
  pure x = BackendT $ \s -> pure (s, x)
  pf <*> q = BackendT $ \s -> do { (s', f) <- runBackendT pf s; (s'', x) <- runBackendT q s'; return (s'', f x) }

instance (Monad m) => Monad (BackendT s m) where
  return x = BackendT $ \s -> return (s, x)
  p >>= (fq) = BackendT $ \s -> do { (s', x) <- runBackendT p s; runBackendT (fq x) s' }

instance (Monad m, MonadIO m) => MonadState s (BackendT s m) where
  get = BackendT $ \s -> do { v <- liftIO (readMVar s); return (s, v) }
  put s = BackendT $
    \st -> do { liftIO (modifyMVar_ st (\_ -> return s)); return (st, ()) }
  state f = BackendT $
    \ms -> do s <- liftIO (takeMVar ms)
              let (x, s') = f s
              liftIO (putMVar ms s')
              return (ms, x)

instance (Monad m, MonadIO m) => MonadIO (BackendT s m) where
  liftIO io = BackendT $ \s -> do { x <- liftIO io; return (s, x) }
