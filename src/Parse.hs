{-# LANGUAGE  NoMonomorphismRestriction
            , FlexibleContexts #-}
module Parse (

) where


import Classes
import Transformers
import Instances
import Control.Applicative     (Applicative(..), liftA, liftA2)
import Control.Monad           (MonadPlus(..), guard, liftM)


(<+>) = mplus

item :: (MonadPlus m, TState [t] m) => m t
item = 
    get >>= \xs -> case xs of
                        (t:ts) -> put ts >> return t;
                        []     -> mzero;

check :: MonadPlus m => (a -> Bool) -> m a -> m a
check f p =
    p            >>= \x ->
    guard (f x)  >>
    return x

satisfy :: (MonadPlus m, TState [t] m) => (t -> Bool) -> m t
satisfy p = check p item

literal :: (Eq t, MonadPlus m, TState [t] m) => t -> m t
literal tok = satisfy (== tok)

many0 :: (Applicative m, MonadPlus m) => m a -> m [a]
many0 p = many1 p <+> return []

many1 :: (Applicative m, MonadPlus m) => m a -> m [a]
many1 p = liftM (:) p <*> many0 p

optional :: (MonadPlus m, MonadPlus n) => m a -> m (n a)
optional p = liftM return p  <+>  return mzero

optionalM :: MonadPlus m => a -> m a -> m a
optionalM x p = p <+> return x

sepBy1 :: (MonadPlus m, Applicative m) => m a -> m b -> m ([a], [b])
sepBy1 p s = pure f <*> p <*> many0 (liftA2 (,) s p)
  where
    f p1 rest = (p1 : map snd rest, map fst rest)

sepBy0 :: (MonadPlus m, Applicative m) => m a -> m b -> m ([a], [b])
sepBy0 p s = sepBy1 p s <+> pure ([], [])

end :: (MonadPlus m, Switch m, TState [t] m) => m ()
end = switch item

not1 :: (MonadPlus m, Applicative m, Switch m, TState [t] m) => m a -> m t
not1 p = switch p *> item

{-

pnot :: (Eq a, MonadState [a] m, AZero' m) => a -> m a
pnot x = satisfy (/= x)

pnone :: (Eq a, MonadState [a] m, AZero' m) => [a] -> m a
pnone xs = satisfy (\x -> not $ elem x xs)

string :: (Eq a, MonadState [a] f, AZero' f) => [a] -> f [a]
string = commute . map literal
-}