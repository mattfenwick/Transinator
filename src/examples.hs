{-# LANGUAGE  NoMonomorphismRestriction
            , FlexibleContexts #-}

import Datums.Transformers
import Classes.Base
import Classes.Transformers
import Instances.Base
import Instances.Transformers

import Parse


type Parser s e t a = StateT [t] (StateT s (MaybeT (ErrorT e Id))) a


i1 :: Parser () String Char String
i1 = commute [item, item]

omg :: Parser () String Integer Float
omg = 
    get >>== f
  where
    f (y:ys) = put ys *> pure (fromInteger y)
    f   []   = mzero

    
run p s1 s2 = getId $ getErrorT $ getMaybeT $ getStateT (getStateT p s1) s2

type Token = (Char, Int, Int)
chr  (a, _, _)  =  a
line (_, b, _)  =  b
col  (_, _, c)  =  c

addLineCol :: [Char] -> [Token]
addLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((line, col), ts) '\n' = ((line + 1, 1), ('\n', line, col):ts)
    f ((line, col), ts)  c   = ((line, col + 1), (c, line, col):ts)


-- examples
-- 1. error reporting
--     - simple error messages
--     - nested messages -- i.e. stack of errors
--     - complex error info:  rest of token stream
--     - complex error info:  position
-- 2. change underlying monad
--     - Maybe: deterministic, prioritized
--     - List: non-deterministic, depth-first
--     - Logic: non-deterministic, breadth-first
-- 3. partial results (can also work for monadic parsers)
--     - global log
--     - 'local' log
--     - multiple writers for different types
--     - algebraic data type for different types
-- 4. brace matching
-- 5. miscellaneous
--     - could also show how to 'observe' backtracking using logging

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


-- partial results examples

pgood = "(a(b)c(d)e(f)g(h))"
pbad  = "(a(b)c(d)e(f)g(h)i"