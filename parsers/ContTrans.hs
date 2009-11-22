

--------------------------------------------------
-- the continuation monad transformer from section 3.3.1


module ContTrans (ContTrans) where

import Parser


newtype ContTrans m a = CT (forall b . (a -> m b) -> m b)


instance Monoid m => Monoid (ContTrans m) where
  zero          = CT (\fut -> zero)
  CT p <+> CT q = CT (\fut -> p fut <+> q fut)


instance Monad (ContTrans m) where
  return a   = CT (\fut -> fut a)
  CT p >>= k = CT (\fut -> p (\a -> let CT q = k a in q fut))


instance Functor (ContTrans m) where
  fmap f p = do a <- p ; return (f a)
{--
  fmap f (CT p) = CT (\fut -> p (fut . f))
--}


instance Sequence (ContTrans m) 
{--
  CT p <*> CT q = CT (\fut -> p (\f -> q (fut.f)))
                = CT (\fut -> p (q . (fut.)))
--}


instance SymbolCont m s => Symbol (ContTrans m) s where
  sat p = CT (satCont p)


instance (Monad m, Parser m s) => Parser (ContTrans m) s where
  parse (CT p) inp = parse (p return) inp


instance Lookahead m s => Lookahead (ContTrans m) s where
  lookahead f = CT (\fut -> lookahead (\inp -> let CT p = f inp in p fut))

