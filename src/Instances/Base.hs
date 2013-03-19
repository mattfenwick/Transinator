-- {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Instances.Base (
  
) where

import Datums.Transformers
import Classes.Base



{-
instance Monad' f => Functor f where
  fmap f m = m >>== (pure . f)
-}


instance Functor Id where
  fmap f (Id x) = Id (f x)
  
instance Pointed Id where
  pure = Id
  
instance Applicative' Id where
  Id f <*> Id x = Id (f x)
  
instance Monad' Id where
  join (Id x) = x
  
  
  
instance Pointed Maybe where
  pure = Just
  
instance Applicative' Maybe where
  Just f  <*>  Just x  =  Just (f x)
  _       <*>     _    =  Nothing
  
instance Monad' Maybe where
  join (Just (Just x)) = Just x
  join        _        = Nothing

instance Plus Maybe where
  Just x   <+>   _  =  Just x
  Nothing  <+>   y  =  y

instance Zero Maybe where
  zero = Nothing

instance Switch Maybe where
  switch  (Just _)  =  zero
  switch  Nothing   =  pure ()

instance Traversable' Maybe where
  traverse f (Just x) = pure Just <*> f x
  traverse _ Nothing  = pure Nothing
  
  
  
instance Functor (Either e) where
  fmap f (Right x) = Right (f x)
  fmap _ (Left e)  = Left e
  
instance Pointed (Either e) where
  pure = Right
  
instance Applicative' (Either e) where
  Right f <*> Right x = Right (f x)
  Left e1 <*>   _     = Left e1
  _       <*> Left e2 = Left e2
  
instance Monad' (Either e) where
  join (Right (Right x))  =  Right x
  join (Right (Left e1))  =  Left e1
  join (Left e2)          =  Left e2

instance Traversable' (Either e) where
  traverse f (Right x) = pure Right <*> f x
  traverse _ (Left e)  = pure (Left e)



-- just used by the Functor instance of StateT ?????
instance Functor ((,) w) where
  fmap f (x, y) = (x, f y)




instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (fmap (fmap f) . g)

instance Pointed m => Pointed (StateT s m) where
  pure x = StateT (\s -> pure (s, x))

instance Monad' m => Applicative' (StateT s m) where
  StateT f <*> StateT x = StateT h
    where
      h s1 =
          f s1 >>== \(s2, f') ->
          x s2 >>== \(s3, x') ->
          pure (s3, f' x')

instance Monad' m => Monad' (StateT s m) where
  join (StateT v1) = StateT h
    where
      h s1 = 
          v1 s1 >>== \(s2, StateT v2) ->
          v2 s2

instance Plus m => Plus (StateT s m) where
  StateT f  <+>  StateT g  =  StateT (\s -> f s <+> g s)
  
instance Zero m => Zero (StateT s m) where
  zero = StateT (const zero)

instance Switch m => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const (s, ())) . switch $ f s)



instance Functor m => Functor (ErrorT e m) where
  fmap f (ErrorT m) = ErrorT (fmap (fmap f) m)

instance Pointed m => Pointed (ErrorT e m) where
  pure = ErrorT . pure . pure

instance Applicative' m => Applicative' (ErrorT e m) where
  ErrorT f <*> ErrorT x = ErrorT (pure (<*>) <*> f <*> x)

instance Monad' m => Monad' (ErrorT e m) where
  -- ErrorT e m (ErrorT e m a) -> ErrorT e m a
  -- m (Either e (m (Either e a))) -> m (Either e a)
  join =
      ErrorT                 .
      fmap join              .
      join                   .
      fmap commute           .
      fmap (fmap getErrorT)  .
      getErrorT

instance Plus m => Plus (ErrorT e m) where
  -- m (Either e a) -> m (Either e a) -> m (Either e a)
  ErrorT l  <+>  ErrorT r  =  ErrorT (l <+> r)
  
instance Zero m => Zero (ErrorT e m) where
  zero = ErrorT zero

instance Switch m => Switch (ErrorT e m) where
  -- m (Either e a) -> m (Either e ())
  -- switch $ Just (Left  e) = Nothing
  -- switch $ Just (Right r) = Nothing
  -- switch $ Nothing        = Just (Right ())
  switch (ErrorT e) =  ErrorT (fmap Right $ switch e)



instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT (fmap (fmap f) m)

instance Pointed m => Pointed (MaybeT m) where
  pure = MaybeT . pure . pure

instance Applicative' m => Applicative' (MaybeT m) where
  MaybeT f <*> MaybeT x = MaybeT (pure (<*>) <*> f <*> x)

instance Monad' m => Monad' (MaybeT m) where
  join =
      MaybeT                 .
      fmap join              .
      join                   .
      fmap commute           .
      fmap (fmap getMaybeT)  .
      getMaybeT

instance Monad' m => Plus (MaybeT m) where
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>== \y -> case y of Nothing -> r;
                                 Just _  -> pure y;

instance Monad' m => Zero (MaybeT m) where
  zero = MaybeT (pure Nothing)

instance Monad' m => Switch (MaybeT m) where
  switch (MaybeT x) = MaybeT (fmap switch x)
