

--------------------------------------------------
-- the stack continuation transformer from section 3.4


module StackTrans (StackTrans) where

import Parser


newtype StackTrans m a = ST (forall b c . (b -> m c) -> (a -> b) -> m c)


instance Monoid m => Monoid (StackTrans m) where
  zero          = ST (\fut stack -> zero)
  ST p <+> ST q = ST (\fut stack -> p fut stack <+> q fut stack)


instance Monad (StackTrans m) where
  return a = ST (\fut stack -> fut (stack a))
  (>>=)    = error "StackTrans: (>>=) is not implemented"


instance Functor (StackTrans m) where
  fmap f (ST p) = ST (\fut stack -> p fut (stack . f))


instance Sequence (StackTrans m) where
  ST p <*> ST q = ST (\fut stack -> p (q fut) (stack .))


instance SymbolCont m s => Symbol (StackTrans m) s where
  sat p = ST (\fut stack -> satCont p (fut . stack))


instance (Monad m, Parser m s) => Parser (StackTrans m) s where
  parse (ST p) inp  = parse (p return id) inp


instance Lookahead m s => Lookahead (StackTrans m) s where
  lookahead f = ST (\fut stack -> lookahead (\inp -> let ST p = f inp in p fut stack))



