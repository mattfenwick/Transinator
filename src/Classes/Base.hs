module Classes.Base (
  
    Pointed
  , pure
  
  , Applicative'
  , (<*>)
  , (<*)
  , (*>)
  , liftA2
  
  , Monad'
  , join
  , (>>==)
  
  , Semigroup
  , append
  , Monoid'
  , empty

  , Plus
  , (<+>)
  -- , sum ???
  
  , many0
  , many1
  
  , Zero
  , zero
  , guard
  
  , Switch
  , switch
  
  , Traversable'
  , commute
  , traverse
  
) where



class Functor f => Pointed f where
  pure :: a -> f a

class Pointed f => Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b
  
(<*) :: Applicative' f => f a -> f b -> f a
l <* r = pure const <*> l <*> r

(*>) :: Applicative' f => f a -> f b -> f b
l *> r = pure (flip const) <*> l <*> r
  
liftA2 :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = pure f <*> x <*> y
  
class Applicative' m => Monad' m where
  join :: m (m a) -> m a
  
(>>==) :: Monad' m => m a -> (a -> m b) -> m b
m >>== f = join (fmap f m)
  
class Semigroup a where
  append :: a -> a -> a
  
class Semigroup a => Monoid' a where
  empty :: a
  
-- associative:  a <+> (b <+> c) = (a <+> b) <+> c
class Plus f where
  (<+>) :: f a -> f a -> f a
  
many0 :: (Applicative' f, Plus f) => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: (Applicative' f, Plus f) => f a -> f [a]
many1 p = pure (:) <*> p <*> many0 p

-- left zero:   zero  <+>    a   =  a
-- right zero:   b    <+>  zero  =  b
class Plus f => Zero f where
  zero :: f a

guard :: (Pointed f, Zero f) => Bool -> f ()
guard True = pure ()
guard False = zero

-- law 1:  switch zero     = pure ()
-- law 2:  switch (pure _) = zero
class Pointed f => Switch f where
  switch :: f a -> f ()

class Functor f => Traversable' f where
  commute :: Applicative' m => f (m a) -> m (f a)
  traverse :: Applicative' m => (a -> m b) -> f a -> m (f b)
  commute = traverse id
  traverse f = commute . fmap f
