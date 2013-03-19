{-# LANGUAGE NoMonomorphismRestriction #-}
module Instances.Transformers (
  
) where

import Transformers         (Id(..), MaybeT(..), ErrorT(..), StateT(..))
import Classes              

-- Functor

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Monad m => Functor (MaybeT m) where
  fmap = 

instance Monad m => Functor (ErrorT e m) where
  fmap = liftM
  
instance Monad m => Functor (StateT s m) where
  fmap = liftM


-- Applicative 

app :: Monad m => m (a -> b) -> m a -> m b
app = liftM2 ($)

instance Applicative Id where
  pure  = return
  (<*>) = app
  
instance Monad m => Applicative (MaybeT m) where
  pure  = return
  (<*>) = app
  
instance Monad m => Applicative (ErrorT e m) where
  pure  = return
  (<*>) = app
  
instance Monad m => Applicative (StateT s m) where
  pure  = return
  (<*>) = app

-- MonadPlus

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT (return Nothing)
  mplus (MaybeT m) (MaybeT n)  =  MaybeT o
    where o = m >>= \x -> case x of
                               Nothing -> n;
                               Just _  -> return x;

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero = StateT (const mzero)
  mplus (StateT f) (StateT g) = StateT (\s -> f s `mplus` g s)

  
-- Traversable

instance Foldable (MaybeT m) where
  --    
  foldr = error "oops -- undefined!"

instance (Monad m, Traversable m) => Traversable (MaybeT m) where
  -- based on:     fmap f (MaybeT x) = MaybeT (fmap (fmap f) x)
  traverse f (MaybeT x)  =  pure MaybeT  <*>  traverse (traverse f) x

-- Switch

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()
  
instance Monad m => Switch (MaybeT m) where
  -- MaybeT m a -> MaybeT m ()
  -- m (Maybe a) -> m (Maybe ())
  switch (MaybeT m) = MaybeT (liftM switch m)

instance (Switch m, Monad m) => Switch (StateT s m) where
  -- StateT s m a -> StateT s m ()
  -- (s -> m (s, a)) -> s -> m (s, ())
  switch (StateT f) = StateT g
    where
      g s = switch (f s)    >> 
            return (s, ())
