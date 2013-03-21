module Instances.Base (
  
) where

import Datums.Transformers
import Classes.Base



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
  
  
  
instance Pointed [] where
  pure = (:[])
  
instance Semigroup [a] where
  append = (++)
  
instance Monoid' [a] where
  empty = []

instance Traversable' [] where
  -- fmap f (x:xs) = f x : fmap f xs
  traverse f [] = pure []
  traverse f (x:xs) = pure (:) <*> f x <*> traverse f xs



instance Functor ((,) w) where
  fmap f (q, y) = (q, f y)
  
instance Monoid' w => Pointed ((,) w) where
  pure x = (empty, x)
  
instance Monoid' w => Applicative' ((,) w) where
  (w1, f) <*> (w2, x) = (w1 `append` w2, f x)

instance Monoid' w => Monad' ((,) w) where
  join (w1, (w2, x)) = (w1 `append` w2, x)
  
instance Traversable' ((,) w) where
  traverse f (q, x) =  pure (,) <*> pure q <*> f x



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

-- Plus, Zero, Switch:  'lift' semantics
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
  join =
      ErrorT                 .
      fmap join              .
      join                   .
      fmap commute           .
      fmap (fmap getErrorT)  .
      getErrorT

-- Plus, Zero, Switch:  'lift' semantics
instance Plus m => Plus (ErrorT e m) where
  ErrorT l  <+>  ErrorT r  =  ErrorT (l <+> r)
  
instance Zero m => Zero (ErrorT e m) where
  zero = ErrorT zero

instance Switch m => Switch (ErrorT e m) where
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

-- Plus, Zero, Switch:  deal with them here
instance Monad' m => Plus (MaybeT m) where
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>== \y -> case y of Nothing -> r;
                                 Just _  -> pure y;

instance Monad' m => Zero (MaybeT m) where
  zero = MaybeT (pure Nothing)

instance Monad' m => Switch (MaybeT m) where
  switch (MaybeT x) = MaybeT (fmap switch x)
  
  

instance Functor m => Functor (WriterT w m) where
  fmap f (WriterT x) = WriterT (fmap (fmap f) x)

instance (Pointed m, Monoid' w) => Pointed (WriterT w m) where
  pure = WriterT . pure . pure
  
instance (Applicative' m, Monoid' w) => Applicative' (WriterT w m) where
  WriterT f <*> WriterT x = WriterT (pure (<*>) <*> f <*> x)
  
instance (Monad' m, Monoid' w) => Monad' (WriterT w m) where
  join = 
      WriterT                 .
      fmap join               .
      join                    .
      fmap commute            .
      fmap (fmap getWriterT)  .
      getWriterT

instance Plus m => Plus (WriterT w m) where
  WriterT l  <+>  WriterT r  =  WriterT (l <+> r)
  
instance Zero m => Zero (WriterT w m) where
  zero = WriterT zero
  
instance (Switch m, Monoid' w) => Switch (WriterT w m) where
  switch (WriterT x) = WriterT (fmap (const (empty, ())) $ switch x)
