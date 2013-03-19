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
  