

--------------------------------------------------
-- the extended trie from section 4.3.3


module ExTrie (ExTrie) where

import OrdMap
import Parser


data ExTrie s a = Shift (Map s (ExTrie s a))    |
                  a ::: ExTrie s a              |
		  forall b . FMap (b -> a) (ExTrie s b)


unfold :: Ord s => (a -> b) -> ExTrie s a -> ExTrie s b
unfold f (Shift pmap) = Shift (mapMap (FMap f) pmap)
unfold f (a ::: p)    = f a ::: FMap f p
unfold f (FMap g p)   = FMap (f . g) p


instance Ord s => Monoid (ExTrie s) where
  zero                      = Shift emptyMap

  (a ::: p)  <+> q          = a ::: (p <+> q)
  p          <+> (b ::: q)  = b ::: (p <+> q)
  FMap f p   <+> q          = unfold f p <+> q
  p          <+> FMap f q   = p <+> unfold f q
  Shift pmap <+> Shift qmap = Shift (mergeWith (<+>) pmap qmap)


instance Ord s => Monad (ExTrie s) where
  return a         = a ::: zero

  (a ::: p)  >>= k = k a <+> (p >>= k)
  FMap f p   >>= k = unfold f p >>= k
  Shift pmap >>= k = Shift (mapMap (>>=k) pmap)


instance Ord s => Functor (ExTrie s) where
  fmap = FMap


instance Ord s => Sequence (ExTrie s)


instance InputSymbol s => Symbol (ExTrie s) s where
  sym s = Shift (s |-> return s)
  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (ExTrie s) s where
  parse     p inp = parse'     p inp id
  parseFull p inp = parseFull' p inp id


parse' :: Ord s => ExTrie s a -> [s] -> (a -> b) -> [([s], b)]
parse' (FMap f p)   inp     k = parse' p inp (k . f)
parse' (a ::: p)    inp     k = (inp, k a) : parse' p inp k
parse' _            []      k = []
parse' (Shift pmap) (s:inp) k = case pmap ? s of
 			          Just p  -> parse' p inp k
				  Nothing -> []


parseFull' :: Ord s => ExTrie s a -> [s] -> (a -> b) -> [b]
parseFull' (FMap f p)   inp     k = parseFull' p inp (k . f)
parseFull' (a ::: p)    []      k = k a : parseFull' p [] k
parseFull' (a ::: p)    inp     k = parseFull' p inp k
parseFull' _            []      k = []
parseFull' (Shift pmap) (s:inp) k = case pmap ? s of
				      Just p  -> parseFull' p inp k
				      Nothing -> []




