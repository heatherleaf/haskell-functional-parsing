

--------------------------------------------------
-- the trie parser from section 4.2.1


module Trie (Trie) where

import OrdMap
import Parser


data Trie s a = Shift (Map s (Trie s a))  |
		a ::: Trie s a


instance Ord s => Monoid (Trie s) where
  zero                      = Shift emptyMap

  (a ::: p)  <+> q          = a ::: (p <+> q)
  p          <+> (b ::: q)  = b ::: (p <+> q)
  Shift pmap <+> Shift qmap = Shift (mergeWith (<+>) pmap qmap)


instance Ord s => Monad (Trie s) where
  return a         = a ::: zero

  (a ::: p)  >>= k = k a <+> (p >>= k)
  Shift pmap >>= k = Shift (mapMap (>>=k) pmap)


instance Ord s => Functor (Trie s) where
  fmap f p = do a <- p ; return (f a)
{--
  fmap f (a ::: p)    = f a ::: fmap f p
  fmap f (Shift pmap) = Shift (mapMap (fmap f) pmap)
--}


instance Ord s => Sequence (Trie s)


instance InputSymbol s => Symbol (Trie s) s where
  sym s = Shift (s |-> return s)
  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (Trie s) s where
  parse (a ::: p)    inp     = (inp, a) : parse p inp
  parse _            []      = []
  parse (Shift pmap) (s:inp) = case pmap ? s of
			         Just p  -> parse p inp
				 Nothing -> []

  parseFull p            []      = collect p
    where collect (a:::p)        = a : collect p
	  collect  _             = []
  parseFull (_ ::: p)    inp     = parseFull p inp
  parseFull (Shift pmap) (s:inp) = case pmap ? s of
				     Just p  -> parseFull p inp
				     Nothing -> []




