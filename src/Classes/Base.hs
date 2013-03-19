module Classes.Base (
  
    Pointed
  , pure
  
  , Applicative'
  , (<*>)
  , (<*)
  , (*>)
  
  , Monad'
  , join
  , (>>==)
  
  , Plus
  , (<+>)
  -- , sum ???

  , many0
  , many1
  
  , Zero
  , zero
  
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
  
class Applicative' m => Monad' m where
  join :: m (m a) -> m a
  
(>>==) :: Monad' m => m a -> (a -> m b) -> m b
m >>== f = join (fmap f m)
  
-- associative:  a <+> (b <+> c) = (a <+> b) <+> c
class Plus f where
  (<+>) :: f a -> f a -> f a
  
-- left zero:   zero  <+>    a   =  a
-- right zero:   b    <+>  zero  =  b
class Plus f => Zero f where
  zero :: f a

many0 :: (Applicative' f, Plus f) => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: (Applicative' f, Plus f) => f a -> f [a]
many1 p = pure (:) <*> p <*> many0 p

-- law 1:  switch zero     = pure ()
-- law 2:  switch (pure _) = zero
class Pointed f => Switch f where
  switch :: f a -> f ()

class Functor f => Traversable' f where
  commute :: Applicative' m => f (m a) -> m (f a)
  traverse :: Applicative' m => (a -> m b) -> f a -> m (f b)
  commute = traverse id
  traverse f = commute . fmap f
