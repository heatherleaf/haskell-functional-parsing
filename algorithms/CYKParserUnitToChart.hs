

--------------------------------------------------
-- CYK parsing to a chart of passive edges
-- as described in the final exercise in section 7.3

-- an extra feature is that the grammar can consist 
-- of both binary and unit productions
-- (by using /limit/ in the definition of /product/)


module CYKParserUnitToChart (process) where

import Grammar
import OrdSet
import OrdMap
import FinalChart (Passive)

err str = error ("CYKParserUnitToChart: " ++ str)


type Cell   c = Set c
type Vector c = [(Int, Cell c)]


process :: Ord c => Grammar c t -> [t] -> [Passive c]
process (grammar@(_, start, terminal, productions) :: Grammar c t) 
    | emptyProductions = err "input grammar has empty productions"
    | longProductions  = err "input grammar has right-hand sides longer than two"
    | cyclic grammar   = err "input grammar is cyclic"
    | otherwise        = process'
  where
    longProductions  = not (null (filter (> 2) (map (length.snd) (elems productions))))
    emptyProductions = not (null (filter null (map snd (elems productions))))

    grammarMap :: Map (c,c) (Set c)
    grammarMap = makeMapWith (<++>) [ ((a,b), unitSet c) |
				      (c, [a,b]) <- elems productions ]
    
    process' :: [t] -> [Passive c]
    process' input = chart
      where (size, vectors) = foldl nextInputToken (0, []) input
	    chart           = [ (i,j,cat) |
				(vector,i) <- zip vectors (reverse [0..size-1]),
				(j,cats) <- vector,
				cat <- elems cats ]
	    (ncell, cell)   = last (last vectors)

    nextInputToken :: (Int, [Vector c]) -> t -> (Int, [Vector c])
    nextInputToken (size, vectors) token = (size', vectors')
      where size'    = size + 1
	    vectors' = [(size', cell)] : updateVectors vectors [(size, cell)] size size'
	    cell     = terminal token

    updateVectors :: [Vector c] -> Vector c -> Int -> Int -> [Vector c]
    updateVectors [] _ _ _ = []
    updateVectors (row:rows) col nrow ncol 
	| isEmpty product =  row                    : updateVectors rows                  col  nrow' ncol
	| otherwise       = (row++[(ncol,product)]) : updateVectors rows ((nrow',product):col) nrow' ncol
      where product = scalarProduct row col
	    nrow'   = nrow - 1

    scalarProduct :: Vector c -> Vector c -> Cell c
    scalarProduct [] _ = emptySet
    scalarProduct _ [] = emptySet
    scalarProduct as@((i,a):as') bs@((j,b):bs')
	= case compare i j of
	    LT -> scalarProduct as' bs
	    GT -> scalarProduct as  bs'
	    EQ -> scalarProduct as' bs' <++> product a b

    product :: Cell c -> Cell c -> Cell c
    product acell bcell = limit (lookupWith emptySet unitProd) baseCell
      where baseCell = union [ lookupWith emptySet grammarMap (a,b) |
			       a <- elems acell,
			       b <- elems bcell ]

    unitProd :: Map c (Set c)
    unitProd = makeMapWith (<++>) [ (a, unitSet c) | (c, [a]) <- elems productions ]




