

--------------------------------------------------
-- the standard parser from section 3.2


module Standard (Standard) where

import Parser


newtype Standard s a = Std ([s] -> [([s], a)])


instance Monoid (Standard s) where
  zero            = Std (\inp -> [])
  Std p <+> Std q = Std (\inp -> p inp ++ q inp)


instance Monad (Standard s) where
  return a    = Std (\inp -> [(inp,a)])
  Std p >>= k = Std (\inp -> concat [ q inp' | 
				      (inp', a) <- p inp, 
				      let Std q = k a ])


instance Functor (Standard s) where
  fmap f p = do a <- p ; return (f a)
{--
  fmap f (Std p) = Std (\inp -> [ (inp', f a) | (inp', a) <- p inp ])
--}


instance Sequence (Standard s) 
{--
  Std p <*> Std q = Std (\inp -> [ (inp'', f a) | (inp', f) <- p inp, (inp'', a) <- q inp' ])
--}


instance Eq s => Symbol (Standard s) s where
  sat p = Std sat'
    where sat' (s:inp) | p s = [(inp, s)]
          sat' _             = []


instance Eq s => SymbolCont (Standard s) s where
  satCont p fut = Std sat'
    where sat' (s:inp) | p s = let Std p = fut s in p inp
          sat' _             = []


instance Parser (Standard s) s where
  parse (Std p) inp = p inp


instance Lookahead (Standard s) s where
  lookahead f = Std (\inp -> let Std p = f inp in p inp)


