{-# LANGUAGE 
             FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances #-}

module Instances.Transformers (
  
) where

import Datums.Transformers
import Classes.Base
import Classes.Transformers
import Instances.Base



-- monad transformer instances

instance Trans MaybeT m where
  -- m a -> m (Maybe a)
  lift m = MaybeT (fmap Just m)

instance Trans (StateT s) m where
  -- m a -> (s -> m (s, a))
  lift m = StateT (\s -> fmap ((,) s) m)

instance Trans (ErrorT e) m where
  -- m a -> m (Error e a)
  lift m = ErrorT (fmap Right m)


  
-- TMaybe instances

instance Monad' m => TMaybe (MaybeT m) where
  mzero = MaybeT (pure Nothing)

instance TMaybe m => TMaybe (StateT s m) where
  mzero = lift mzero

instance TMaybe m => TMaybe (ErrorT e m) where
  mzero = lift mzero


-- TState instances

instance Monad' m => TState s (StateT s m) where
  get = StateT (\s -> pure (s, s))
  put s = StateT (\_ -> pure (s, ()))

instance TState s m => TState s (MaybeT m) where
  get = lift get
  put = lift . put

instance TState s m => TState s (ErrorT e m) where
  get = lift get
  put = lift . put


-- TError instances

instance Monad' m => TError e (ErrorT e m) where
  throwE = ErrorT . pure . Left
  catchE err f = ErrorT (getErrorT err >>== g)
    where
      g (Left e) = getErrorT (f e)
      g (Right z) = pure (Right z)

instance TError e m => TError e (MaybeT m) where
  throwE = lift . throwE
  catchE m f = MaybeT $ catchE (getMaybeT m) (getMaybeT . f)

instance TError e m => TError e (StateT s m) where
  throwE = lift . throwE
  catchE m f = StateT (\s -> catchE (getStateT m s) (\e -> getStateT (f e) s))

