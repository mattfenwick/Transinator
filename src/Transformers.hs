{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             NoMonomorphismRestriction #-}
module Transformers (
  
    Id(..)
  
  , MaybeT(..)
  , TMaybe
  
  , StateT(..)
  , TState(..)
  
  , ErrorT(..)
  , TError
  
) where

-- data definitions

newtype Id a =
    Id {getId :: a}

newtype MaybeT m a =
    MaybeT {getMaybeT :: m (Maybe a)}

newtype StateT s m a =
    StateT {getStateT :: s -> m (s, a)}

newtype ErrorT e m a =
    ErrorT {getErrorT :: m (Either e a)}

-- Show instances

instance Show (m (Maybe a)) => Show (MaybeT m a) where
  show (MaybeT x) = show x


-- monad instances

instance Monad m => Monad (MaybeT m) where
  -- a -> m (Maybe a)
  return = MaybeT . return . Just
  -- m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
  MaybeT x >>= f = MaybeT (x >>= g)
    where
      g Nothing  = return Nothing 
      g (Just z) = getMaybeT (f z)

instance Monad m => Monad (StateT s m) where
  -- a -> s -> m (s, a)
  return x = StateT (\s -> return (s, x))
  -- (s -> m (s, a)) -> (a -> s -> m (s, b)) -> s -> m (s, b)
  x >>= f = StateT y
    where
      y s =  
          getStateT x s >>= \(s', a) -> 
          getStateT (f a) s'

instance Monad m => Monad (ErrorT e m) where
  -- a -> m (Either e a)
  return = ErrorT . return . Right
  -- m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
  x >>= f = ErrorT (getErrorT x >>= g)
    where
      g (Left e)   =  return (Left e)
      g (Right z)  =  getErrorT (f z)


-- monad transformer type class

class Trans t m where
  lift :: Monad m => m a -> t m a


-- monad transformer instances

instance Trans MaybeT m where
  -- m a -> m (Maybe a)
  lift m = MaybeT (m >>= return . Just)

instance Trans (StateT s) m where
  -- m a -> (s -> m (s, a))
  lift m = StateT (\s -> m >>= \a -> return (s, a))

instance Trans (ErrorT e) m where
  -- m a -> m (Error e a)
  lift m = ErrorT (m >>= return . Right)


-- transformer type classes

class Monad m => TMaybe m where
  zero :: m a

class Monad m => TState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => TError e m | m -> e where
  throwE :: e -> m a
  catchE :: m a -> (e -> m a) -> m a
  
  
-- TMaybe instances

instance Monad m => TMaybe (MaybeT m) where
  -- m (Maybe a)
  zero = MaybeT (return Nothing)

instance TMaybe m => TMaybe (StateT s m) where
  zero = lift zero

instance TMaybe m => TMaybe (ErrorT e m) where
  zero = lift zero


-- TState instances

instance Monad m => TState s (StateT s m) where
  -- s -> m (s, a)
  get = StateT (\s -> return (s, s))
  -- s -> s -> m (s, ())
  put s = StateT (\_ -> return (s, ()))

instance TState s m => TState s (MaybeT m) where
  get = lift get
  put = lift . put

instance TState s m => TState s (ErrorT e m) where
  get = lift get
  put = lift . put


-- TError instances

instance Monad m => TError e (ErrorT e m) where
  -- e -> m (Either e a)
  throwE = ErrorT . return . Left
  -- m (Either e a) -> (e -> m (Either e a)) -> m (Either e a)
  catchE err f = ErrorT (getErrorT err >>= g)
    where
      g (Left e)  = getErrorT (f e)
      g (Right z) = return (Right z)

instance TError e m => TError e (MaybeT m) where
  throwE = lift . throwE 
  catchE m f = MaybeT $ catchE (getMaybeT m) (getMaybeT . f)

instance TError e m => TError e (StateT s  m) where
  throwE = lift . throwE
  catchE m f = StateT (\s -> catchE (getStateT m s) (\e -> getStateT (f e) s))
