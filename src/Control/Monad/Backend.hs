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
import Control.Concurrent.MVar (MVar, isEmptyMVar, modifyMVar_, takeMVar, putMVar, readMVar, newMVar)
import Control.Arrow (second)


{-| The 'BackendT' transformer is a 'StateT', where the state is encapsulated into a 'MVar'. We have

    BackendT s m a

where `s` is the state and `m` is a monad, usually with a 'MonadIO' constraint. The state is only unwrapped, when 'get', 'set' or 'state' is used. A 'bind' will not unwrap.  -}
newtype BackendT s m a = BackendT (MVar s -> m (MVar s, a))

-- | 'runBackendT' unwraps the 'BackendT' monad on a given initial state. This given state is wrapped into an 'MVar'.
runBackendT :: (Monad m, MonadIO m) => BackendT s m a -> s -> m (MVar s, a)
runBackendT (BackendT f) s = do { v <- liftIO (newMVar s); f v }

-- | 'rawRunBackendT' unwraps then 'BackendT' monad on a given initial state, where this state is wrapped into an 'MVar'. **The 'MVar' shouldn't be empty, otherwise every state reading/writing is blocking.** Usually, one should use 'runBackendT'.
rawRunBackendT :: BackendT s m a -> MVar s -> m (MVar s, a)
rawRunBackendT (BackendT f) = f

instance (Functor m) => Functor (BackendT s m) where
  fmap f bt = BackendT $ \s -> fmap (second f) (rawRunBackendT bt s)

instance (Monad m) => Applicative (BackendT s m) where
  pure x = BackendT $ \s -> pure (s, x)
  pf <*> q = BackendT $ \s -> do { (s', f) <- rawRunBackendT pf s; (s'', x) <- rawRunBackendT q s'; return (s'', f x) }

instance (Monad m) => Monad (BackendT s m) where
  return x = BackendT $ \s -> return (s, x)
  p >>= fq = BackendT $ \s -> do { (s', x) <- rawRunBackendT p s; rawRunBackendT (fq x) s' }

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
