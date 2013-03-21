{-# LANGUAGE NoMonomorphismRestriction #-}
module Examples.Partial (

) where

import Datums.Transformers
import Classes.Base
import Classes.Transformers
import Instances.Base
import Instances.Transformers

import Parse


-- State, Writer, Either, Maybe
-- [t] -> Either e (Maybe (w, ([t], a)))
-- Either e (Maybe (w, a))
-- Either e (Maybe a)
-- Either e a
type Parse1 e t w a = StateT [t] (WriterT w (MaybeT (Either e))) a

pgood = "(a(b)c(d)e(f)g(h))"
pbad  = "(a(b)c(d)e(f)g(h)i"

runParse1 :: Parse1 e t w a -> [t] -> Either e (Maybe (w, ([t], a)))
runParse1 p ts = getMaybeT $ getWriterT $ getStateT p ts

data Out = C Char | Block [Out] deriving (Show)

op = literal '('
cls = literal ')'
character = fmap C $ not1 (op <+> cls)
body block = many0 (character <+> block)
block1 = fmap Block (op *> body block1 <* cls)

-- [t] -> (w, Maybe ([t], a))
-- (w, Maybe a)
-- (w, a)
type Parse2 t w a = StateT [t] (MaybeT (WriterT w Id)) a

runParse2 :: Parse2 t w a -> [t] -> (w, Maybe ([t], a))
runParse2 p ts = getId $ getWriterT $ getMaybeT $ getStateT p ts

block2 = fmap Block (op *> body block2 <* cls) >>== \b -> tell [b] *> pure b
