{-# LANGUAGE FunctionalDependencies #-}
module Classes.Transformers (
  
    Trans   (..)
  , TMaybe  (..)
  , TState  (..)
  , TError  (..)
  , Composer(..)
  
) where

import Classes.Base


class Trans t m where
  lift :: Monad' m => m a -> t m a


class Monad' m => TMaybe m where
  mzero :: m a

class Monad' m => TState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad' m => TError e m | m -> e where
  throwE :: e -> m a
  catchE :: m a -> (e -> m a) -> m a

  
class Composer c g | c -> g where
  open  :: c f a -> f (g a)
  close :: f (g a) -> c f a
