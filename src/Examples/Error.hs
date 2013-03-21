{-# LANGUAGE  NoMonomorphismRestriction #-}
module Examples.Error (

) where

import Datums.Transformers
import Classes.Base
import Classes.Transformers
import Instances.Base
import Instances.Transformers

import Parse
import Examples.Common


type Parser s e t a = StateT [t] (StateT s (MaybeT (ErrorT e Id))) a


type Parse1 e t a = StateT [t] (MaybeT (ErrorT e Id)) a

runParse1 :: Parse1 e t a -> [t] -> Either e (Maybe ([t], a))
runParse1 p ts = getId $ getErrorT $ getMaybeT $ getStateT p ts

data Out = C Char | Block [Out] deriving (Show)


-- error reporting examples

body block = many0 (character <+> block)
character = fmap C $ not1 (op <+> cls)
op = literal '('
cls = literal ')'
block1 = fmap Block (op *> body block1 <* cls)

good = "(a(b)c)(qr)"
bad = "(a(b\n)c(d\nq\n(e((oopsqr)syz"

block2 = fmap Block (op *> (rest <+> throwE "unable to match block:  missing )"))
  where
    rest = body block2 <* cls
    
block3 = fmap Block (op *> (rest <+> err))
  where
    rest = body block3 <* cls
    err = get >>== \ts -> throwE ("unable to match block: missing )", ts)

mapError m f = catchE m (throwE . f)

block4 = fmap Block (op *> mapError (rest <+> err) ("missing )":))
  where 
    rest = body block4 <* cls
    err = throwE []

block5 = fmap Block (o >>== \op -> mapError (rest <+> err) (ferr op))
  where
    rest = many0 (char_ <+> block5) <* c
    err = throwE []
    char_ = fmap (C . chr) $ not1 (o <+> c)
    o = satisfy ((== '(') . chr)
    c = satisfy ((== ')') . chr)
    ferr (_, l, c) stack = ("unmatched (, line: " ++ show l ++ " , column: " ++ show c) : stack
