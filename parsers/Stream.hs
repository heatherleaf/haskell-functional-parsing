

--------------------------------------------------
-- the stream processor from section 3.5.2


module Stream (Stream) where

import Parser


data Stream s a = Shift (s -> Stream s a)  |
		  a ::: Stream s a         |
		  Nil


instance Monoid (Stream s) where
  zero                  = Nil

  Nil      <+> bs       = bs
  as       <+> Nil      = as
  (a:::as) <+> bs       = a ::: (as <+> bs)
  as       <+> (b:::bs) = b ::: (as <+> bs)
  Shift f  <+> Shift g  = Shift (\s -> f s <+> g s)


instance Monad (Stream s) where
  return a = a ::: Nil

  Shift f  >>= k = Shift (\s -> f s >>= k)
  (a:::as) >>= k = k a <+> (as >>= k)
  Nil      >>= k = Nil


instance Functor (Stream s) where
  fmap f p = do a <- p ; return (f a)
{--
  fmap f (Shift g) = Shift (fmap f . g)
  fmap f (a:::as)  = f a ::: fmap f as
  fmap f Nil       = Nil
--}


instance Sequence (Stream s)


instance Eq s => Symbol (Stream s) s where
  skip  = Shift return
  sat p = Shift (\s -> if p s then return s else zero)


instance Eq s => SymbolCont (Stream s) s where
  satCont p fut = Shift (\s -> if p s then fut s else zero)


instance Parser (Stream s) s where
  parse (Shift f) (s:inp) = parse (f s) inp
  parse (a:::p)   inp     = (inp, a) : parse p inp
  parse _         _       = []

  parseFull (Shift f) (s:inp) = parseFull (f s) inp
  parseFull p         []      = collect p
    where collect (a:::p)     = a : collect p
	  collect _           = []
  parseFull (a:::p)   inp     = parseFull p inp
  parseFull _         _       = []


