

--------------------------------------------------
-- the ambiguous trie from section 4.2.2


module AmbTrie (AmbTrie) where

import OrdMap
import Parser


data AmbTrie s a = [a] :&: Map s (AmbTrie s a)


instance Ord s => Monoid (AmbTrie s) where
  zero                        = [] :&: emptyMap

  (as:&:pmap) <+> (bs:&:qmap) = (as++bs) :&: mergeWith (<+>) pmap qmap


instance Ord s => Monad (AmbTrie s) where
  return a          = [a] :&: emptyMap

  (as:&:pmap) >>= k = foldr (<+>) ([]:&:mapMap (>>=k) pmap) (map k as)


instance Ord s => Functor (AmbTrie s) where
  fmap f p = do a <- p ; return (f a)
{--
  fmap f (as :&: pmap) = map f as :&: mapMap (fmap f) pmap
--}


instance Ord s => Sequence (AmbTrie s)


instance InputSymbol s => Symbol (AmbTrie s) s where
  sym s = [] :&: (s |-> return s)
  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (AmbTrie s) s where
  parse = error "AmbTrie: parse is not implemented"

  parseFull (as :&: _)    []      = as
  parseFull (_  :&: pmap) (s:inp) = case pmap ? s of
				      Just p  -> parseFull p inp
				      Nothing -> []




