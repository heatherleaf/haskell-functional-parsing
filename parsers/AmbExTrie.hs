

--------------------------------------------------
-- the ambiguous extended trie from section 4.3.4


module AmbExTrie (AmbExTrie) where

import OrdMap
import Parser


data AmbExTrie s a = [a] :&: Map s (AmbExTrie s a)    |
                     forall b . FMap (b -> a) (AmbExTrie s b)


unfold :: Ord s => (a -> b) -> AmbExTrie s a -> AmbExTrie s b
unfold f (as :&: pmap) = map f as :&: mapMap (FMap f) pmap
unfold f (FMap g p)    = FMap (f . g) p


instance Ord s => Monoid (AmbExTrie s) where
  zero                        = [] :&: emptyMap

  FMap f p    <+> q           = unfold f p <+> q
  p           <+> FMap f q    = p <+> unfold f q
  (as:&:pmap) <+> (bs:&:qmap) = (as++bs) :&: mergeWith (<+>) pmap qmap


instance Ord s => Monad (AmbExTrie s) where
  return a          = [a] :&: emptyMap

  FMap f p    >>= k = unfold f p >>= k
  (as:&:pmap) >>= k = foldr (<+>) ([]:&:mapMap (>>=k) pmap) (map k as)


instance Ord s => Functor (AmbExTrie s) where
  fmap = FMap


instance Ord s => Sequence (AmbExTrie s)


instance InputSymbol s => Symbol (AmbExTrie s) s where
  sym s = [] :&: (s |-> return s)
  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (AmbExTrie s) s where
  parse           = error "AmbExTrie: parse is not implemented"
  parseFull p inp = parseFull' p inp id


parseFull' :: Ord s => AmbExTrie s a -> [s] -> (a -> b) -> [b]
parseFull' (FMap f p)    inp     k = parseFull' p inp (k . f)
parseFull' (as :&: _)    []      k = map k as
parseFull' (_  :&: pmap) (s:inp) k = case pmap ? s of
				       Just p  -> parseFull' p inp k
				       Nothing -> []


