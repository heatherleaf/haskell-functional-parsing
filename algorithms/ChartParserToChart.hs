

--------------------------------------------------
-- Chart parsing to a chart of passive edges
-- as described in sections 5.3 and 5.5
-- with optimizations from section 5.6


module ChartParserToChart (process) where

import Grammar
import OrdSet
import OrdMap
import FinalChart (Passive)

err str = error ("ChartParserToChart: " ++ str)


type Chart c = [State c]
type State c = Set (Edge c)
type Edge  c = (Int, c, [c])


setMap :: (Ord s, Ord a) => [(s, a)] -> Map s (Set a)
setMap ass = makeMapWith (<++>) [ (s, unitSet a) | (s, a) <- ass ]


process :: Ord c => Grammar c t -> [t] -> [Passive c]
process (grammar@(_, start, terminal, productions) :: Grammar c t) 
    | cyclic grammar = err "input grammar is cyclic"
    | otherwise      = process'
  where
    emptyCats :: Set c
    emptyCats = empties grammar

    leftCornerMap :: Map c [Production c]
    leftCornerMap = mapMap elems (setMap [ (a, (b,bs)) | 
					   (b, abs) <- elems productions, 
					   (a : bs) <- removeNullable abs ])

    removeNullable :: [c] -> [[c]]
    removeNullable [] = []
    removeNullable cats@(cat:cats')
	| cat `elemSet` emptyCats = cats : removeNullable cats'
	| otherwise               = [cats]

    process' :: [t] -> [Passive c]
    process' input = passiveEdges
      where 
        finalChart :: Chart c
        finalChart = map buildState initialChart

        finalChartMap :: [Map c (Set (Edge c))]
	finalChartMap = map stateMap finalChart

        stateMap :: State c -> Map c (Set (Edge c))
	stateMap state = setMap [ (a, (i,b,bs)) |
				  (i, b, a:bs) <- elems state ]

        initialChart :: Chart c
        initialChart = emptySet : map initialState (zip [0..] input)
          where initialState (j, sym) = ordSet [ (j, cat, []) | 
						 cat <- elems (terminal sym) ]

        buildState :: State c -> State c
	buildState = limit more
	  where more (j, a, [])   = ordSet [ (j, b, bs) |
				             (b, bs) <- lookupWith [] leftCornerMap a ]
				    <++>
				    (lookupWith emptySet (finalChartMap !! j) a)
		more (j, b, a:bs) = ordSet [ (j, b, bs) |
					     a `elemSet` emptyCats ]

        passiveEdges :: [ Passive c ]
	passiveEdges = [ (i, j, cat) |
			 (j, state) <- zip [0..] finalChart,
			 (i, cat, []) <- elems state ]
		       ++
		       [ (i, i, cat) |
			 i <- [0 .. length input],
			 cat <- elems emptyCats ]





