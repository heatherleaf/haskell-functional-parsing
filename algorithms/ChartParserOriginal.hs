

--------------------------------------------------
-- Chart parsing as described in sections 5.3-5.5


module ChartParserOriginal (parse) where

import Grammar
import OrdSet

err str = error ("ChartParserOriginal: " ++ str)


type Chart c = [State c]
type State c = Set (Edge c)
type Edge  c = (Int, c, [c])

type Passive c = (Int, Int, c)


parse :: Ord c => Grammar c t -> [t] -> [ParseTree c t]
parse (grammar@(_, start, terminal, productions) :: Grammar c t) input
    | cyclic grammar = err "input grammar is cyclic"
    | otherwise      = case lookup (0, length input, start) edgeTrees of
		         Just trees -> trees
			 Nothing    -> []
  where
    removeNullable :: [c] -> [[c]]
    removeNullable [] = []
    removeNullable cats@(cat:cats')
	| cat `elemSet` empties grammar = cats : removeNullable cats'
	| otherwise                     = [cats]

    finalChart :: Chart c
    finalChart = map buildState initialChart

    initialChart :: Chart c
    initialChart = emptySet : map initialState (zip [0..] input)
      where initialState (j, sym) = ordSet [ (j, cat, []) | 
					     cat <- elems (terminal sym) ]

    buildState :: State c -> State c
    buildState = limit more
      where more (j, a, [])   = makeSet [ (j, b, bs) |
					  (b, abs) <- elems productions,
					  (a':bs) <- removeNullable abs, a == a' ]
				<++>
				ordSet [ (i, b, bs) |
					 (i, b, a':bs) <- elems (finalChart !! j), a == a' ]
	    more (j, b, a:bs) = ordSet [ (j, b, bs) |
					 a `elemSet` empties grammar ]

    passiveEdges :: [ Passive c ]
    passiveEdges = [ (i, j, cat) |
		     (j, state) <- zip [0..] finalChart,
		     (i, cat, []) <- elems state ]
		   ++
		   [ (i, i, cat) |
		     i <- [0..length input],
		     cat <- elems (empties grammar) ]

    edgeTrees :: [ (Passive c, [ParseTree c t]) ]
    edgeTrees = [ (edge, treesFor edge) | edge <- passiveEdges ]

    treesFor :: Passive c -> [ParseTree c t]
    treesFor (i, j, cat) = [ cat :^ trees |
			     (cat', rhs) <- elems productions,
			     cat == cat',
			     trees <- children rhs i j ]
			   ++
			   [ cat :^ [Leaf sym] |
			     i == j-1,
			     let sym = input !! i,
			     cat `elemSet` terminal sym ]

    children :: [c] -> Int -> Int -> [[ParseTree c t]]
    children []     i k = [ [] | i == k ]
    children (c:cs) i k = [ tree : rest |
			    i <= k,
			    ((i',j,c'),trees) <- edgeTrees,
			    i == i', c == c',
			    rest <- children cs j k,
			    tree <- trees ]





