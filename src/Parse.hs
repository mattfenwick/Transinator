{-# LANGUAGE  NoMonomorphismRestriction
            , FlexibleContexts #-}
module Parse (

    item
  , check
  , satisfy
  , literal
  
  , optional
  , optionalM
  , sepBy0
  , sepBy1
  
  , end
  , not1
  , pnot
  , pnone
  , string
  
) where


import Datums.Transformers
import Classes.Base
import Classes.Transformers
import Instances.Base
import Instances.Transformers


item :: (Zero m, TState [t] m) => m t
item = 
    get >>== \xs -> case xs of
                         (t:ts) -> put ts *> pure t;
                         []     -> zero;

check :: (Monad' m, Zero m) => (a -> Bool) -> m a -> m a
check f p =
    p            >>== \x ->
    guard (f x)  *>
    pure x

satisfy :: (Zero m, TState [t] m) => (t -> Bool) -> m t
satisfy p = check p item

literal :: (Eq t, Zero m, TState [t] m) => t -> m t
literal tok = satisfy (== tok)

optional :: (Plus f, Pointed f) => f a -> f (Maybe a)
optional p = fmap Just p  <+>  pure Nothing

optionalM :: (Plus f, Pointed f) => a -> f a -> f a
optionalM x p = p <+> pure x

sepBy1 :: (Plus m, Applicative' m) => m a -> m b -> m ([a], [b])
sepBy1 p s = pure f <*> p <*> many0 (liftA2 (,) s p)
  where
    f p1 rest = (p1 : map snd rest, map fst rest)

sepBy0 :: (Plus m, Applicative' m) => m a -> m b -> m ([a], [b])
sepBy0 p s = sepBy1 p s <+> pure ([], [])

end :: (Zero m, Switch m, TState [t] m) => m ()
end = switch item

not1 :: (Zero m, Switch m, TState [t] m) => m a -> m t
not1 p = switch p *> item

pnot :: (Eq t, TState [t] m, Zero m) => t -> m t
pnot x = satisfy (/= x)

pnone :: (Eq t, TState [t] m, Zero m) => [t] -> m t
pnone xs = satisfy (\x -> not $ elem x xs)

string :: (Eq t, TState [t] f, Zero f) => [t] -> f [t]
string = commute . map literal
