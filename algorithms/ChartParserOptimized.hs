

--------------------------------------------------
-- Chart parsing with optimizations 
-- suggested in section 5.6


module ChartParserOptimized (parse) where

import Grammar
import OrdSet
import OrdMap

err str = error ("ChartParserOptimized: " ++ str)


type Chart   c = [State c]
type State   c = Set (Edge c)
type Edge    c = (Int, c, [c])
type Passive c = (Int, Int, c)


(??) :: (Ord s, Ord a) => Map s (Set a) -> s -> Set a
(??) = lookupWith emptySet

setMap :: (Ord s, Ord a) => [(s, a)] -> Map s (Set a)
setMap ass = makeMapWith (<++>) [ (s, unitSet a) | (s, a) <- ass ]


parse :: Ord c => Grammar c t -> [t] -> [ParseTree c t]
parse (grammar@(_, start, terminal, productions) :: Grammar c t) 
    | cyclic grammar = err "input grammar is cyclic"
    | otherwise      = parse'
  where
    emptyCats :: Set c
    emptyCats = empties grammar

    grammarMap :: Map c (Set [c])
    grammarMap = setMap (elems productions)

    leftCornerMap :: Map c (Set (Production c))
    leftCornerMap = setMap [ (a, (b,bs)) | 
			     (b, abs) <- elems productions, 
			     (a : bs) <- removeNullable abs ]

    removeNullable :: [c] -> [[c]]
    removeNullable [] = []
    removeNullable cats@(cat:cats')
	| cat `elemSet` emptyCats = cats : removeNullable cats'
	| otherwise               = [cats]

    parse' :: [t] -> [ParseTree c t]
    parse' input = case lookup (0, length input, start) edgeTrees of
		     Just trees -> trees
		     Nothing    -> []
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
				             (b, bs) <- elems (leftCornerMap ?? a) ]
				    <++>
				    ((finalChartMap !! j) ?? a)
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

        edgeTrees :: [ (Passive c, [ParseTree c t]) ]
	edgeTrees = [ (edge, treesFor edge) | edge <- passiveEdges ]

        edgeTreesMap :: Map (Int, c) [(Int, [ParseTree c t])]
	edgeTreesMap = makeMapWith (++) [ ((i,c), [(j,trees)]) |
					  ((i,j,c), trees) <- edgeTrees ]

        treesFor :: Passive c -> [ParseTree c t]
	treesFor (i, j, cat) = [ cat :^ trees |
				 rhs <- elems (grammarMap ?? cat),
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
				(j,trees) <- lookupWith [] edgeTreesMap (i,c),
				rest <- children cs j k,
				tree <- trees ]





