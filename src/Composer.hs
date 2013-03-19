{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Composer (

    Composer

  , pure2
  , fmap2
  , app2
  , join2
  
) where

import Classes


class Composer c g | c -> g where
  open  :: c f a -> f (g a)
  close :: f (g a) -> c f a


pure2 :: (Pointed f, Pointed g, Composer c g) => a -> c f a
pure2 = close . pure . pure

fmap2 :: (Functor f, Functor g, Composer c g) => (a -> b) -> c f a -> c f b
fmap2 f = close . fmap (fmap f) . open

-- could this type be more general?
app2 :: (Applicative' f, Applicative' g, Composer c g) => c f (a -> b) -> c f a -> c f b
app2 f x = close (pure (<*>) <*> open f <*> open x)

join2 :: (Traversable' g, Monad' g, Monad' f, Composer c g) => c f (c f a) -> c f a
join2 = 
    close              .
    fmap join          .
    join               .
    fmap commute       .
    fmap (fmap open)   .
    open
