

--------------------------------------------------
-- the standard endomorphism parser from section 3.3.3

-- only the instances necessary for applying 
-- a continuation transformer are defined


module StandardEndo (StandardEndo) where

import Parser


newtype StandardEndo s a = StdE ([s] -> [([s], a)] -> [([s], a)])


instance Monoid (StandardEndo s) where
  zero              = StdE (\inp -> id)
  StdE p <+> StdE q = StdE (\inp -> p inp . q inp)


instance Monad (StandardEndo s) where
  return a = StdE (\inp -> ((inp,a):))
  (>>=)    = error "StandardEndo: (>>=) is not implemented"


instance Eq s => Symbol (StandardEndo s) s where
  sat p = StdE sat'
    where sat' (s:inp) | p s = ((inp, s) :)
          sat' _             = id


instance Eq s => SymbolCont (StandardEndo s) s where
  satCont p fut = StdE sat'
    where sat' (s:inp) | p s = let StdE p = fut s in p inp
          sat' _             = id


instance Parser (StandardEndo s) s where
  parse (StdE p) inp = p inp []


instance Lookahead (StandardEndo s) s where
  lookahead f = StdE (\inp -> let StdE p = f inp in p inp)



