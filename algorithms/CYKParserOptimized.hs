

--------------------------------------------------
-- CYK parsing from section 7.3
-- with optimized lookup of grammar productions


module CYKParserOptimized (parse) where

import Grammar
import OrdSet
import OrdMap

err str = error ("CYKParserOptimized: " ++ str)


type Cell   c t = Map c [ParseTree c t]
type Vector c t = [(Int, Cell c t)]

(??) :: Ord s => Map s [a] -> s -> [a]
(??) = lookupWith []


parse :: Ord c => Grammar c t -> [t] -> [ParseTree c t]
parse (grammar@(_, start, terminal, productions) :: Grammar c t) 
    | chomskyNormalForm grammar = process
    | otherwise                 = err "input grammar is not on Chomsky Normal Form"
  where
    grammarMap :: Map (c,c) [c]
    grammarMap = makeMapWith (++) [ ((a,b), [c]) |
				    (c, [a,b]) <- elems productions ]
    
    process :: [t] -> [ParseTree c t]
    process input
	| size == ncell = cell ?? start
	| otherwise     = []
      where (size, vectors) = foldl nextInputToken (0, []) input
	    (ncell, cell)   = last (last vectors)

    nextInputToken :: (Int, [Vector c t]) -> t -> (Int, [Vector c t])
    nextInputToken (size, vectors) token = (size', vectors')
      where size'    = size + 1
	    vectors' = [(size', cell)] : updateVectors vectors [(size, cell)] size size'
	    cell     = terminalCell token

    updateVectors :: [Vector c t] -> Vector c t -> Int -> Int -> [Vector c t]
    updateVectors [] _ _ _ = []
    updateVectors (row:rows) col nrow ncol 
	| isEmptyMap product =  row                    : updateVectors rows                  col  nrow' ncol
	| otherwise          = (row++[(ncol,product)]) : updateVectors rows ((nrow',product):col) nrow' ncol
      where product = scalarProduct row col
	    nrow'   = nrow - 1

    scalarProduct :: Vector c t -> Vector c t -> Cell c t
    scalarProduct [] _ = emptyMap
    scalarProduct _ [] = emptyMap
    scalarProduct as@((i,a):as') bs@((j,b):bs')
	= case compare i j of
	    LT -> scalarProduct as' bs
	    GT -> scalarProduct as  bs'
	    EQ -> scalarProduct as' bs' `joinCell` product a b

    joinCell :: Cell c t -> Cell c t -> Cell c t
    joinCell = mergeWith (++) 

    terminalCell :: t -> Cell c t
    terminalCell term = ordMap [ (cat, treesFor cat) |
				 cat <- elems (terminal term) ]
      where treesFor cat = [cat:^[Leaf term]]

    product :: Cell c t -> Cell c t -> Cell c t
    product acell bcell = makeMapWith (++) 
			  [ (c, buildTrees c atrees btrees) |
			    (a, atrees) <- assocs acell,
			    (b, btrees) <- assocs bcell,
			    c <- grammarMap ?? (a,b) ]
      where buildTrees c atrees btrees = [ c:^[atree,btree] | 
					   atree <- atrees, 
					   btree <- btrees ]





