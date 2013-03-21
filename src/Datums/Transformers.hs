{-# LANGUAGE UndecidableInstances #-}
module Datums.Transformers (

    Id(..)
  
  , MaybeT(..)
  
  , StateT(..)
  
  , ErrorT(..)
  
  , WriterT(..)

) where


newtype Id a =
    Id {getId :: a}
  deriving (Show)

newtype MaybeT m a =
    MaybeT {getMaybeT :: m (Maybe a)}

newtype StateT s m a =
    StateT {getStateT :: s -> m (s, a)}

newtype ErrorT e m a =
    ErrorT {getErrorT :: m (Either e a)}
    
newtype WriterT w m a =
    WriterT {getWriterT :: m (w, a)}


-- Show instances

instance Show (m (Maybe a)) => Show (MaybeT m a) where
  show (MaybeT x) = show x

instance Show (m (Either e a)) => Show (ErrorT e m a) where
  show (ErrorT x) = show x
