

--------------------------------------------------
-- module for working on charts of passive edges
-- e.g. building parse forests / parse trees

-- shared parse forests are not described in the thesis,
-- but are a compact way of storing a collection of
-- parse trees; the sharing of forests is implemented
-- by the sharing in Haskell


module FinalChart (Passive, 
		   ParseForest(..), 
		   forest2trees, 
		   buildForest, 
		   buildTrees) where

import Grammar
import OrdSet
import OrdMap
import List (transpose, intersperse)


--------------------------------------------------
-- the type of passive edges, given in section 5.4

type Passive c = (Int, Int, c)


--------------------------------------------------
-- parse forest

data ParseForest c t = LeaF t | c :^^ [[ParseForest c t]]
		       deriving (Eq, Ord)

instance (Show c, Show t) => Show (ParseForest c t) where
  show (LeaF t)    = show t
  show (c :^^ fss) = show c ++ "^{" ++ concat (intersperse "," (map show fss)) ++ "}"


--------------------------------------------------
-- converting a forest to a list of trees

forest2trees :: ParseForest c t -> [ParseTree c t]
forest2trees (LeaF t)     = [ Leaf t ]
forest2trees (c :^^ fss)  = [ c :^ rhs | rhs <- children fss ]
  where children []       = []
	children (fs:fss) = transpose (map forest2trees fs) ++ children fss


--------------------------------------------------
-- building parse forest

buildForest :: Ord c => Grammar c t -> [Passive c] -> [t] -> Maybe (ParseForest c t)
buildForest (grammar :: Grammar c t) = build
  where
    (_, start, terminal, productions) = grammar

    grammarMap :: Map c [[c]]
    grammarMap = makeMapWith (++) [ (cat,[rhs]) | (cat,rhs) <- elems productions ]

    build passiveEdges input = lookup (0, length input, start) edgeForest
      where
        edgeForest :: [ (Passive c, ParseForest c t) ]
	edgeForest = [ (edge, forestFor edge) | edge <- passiveEdges ]

        edgeForestMap :: Map (Int, c) [(Int, ParseForest c t)]
	edgeForestMap = makeMapWith (++) [ ((i,c), [(j,forest)]) |
					   ((i,j,c), forest) <- edgeForest ]

        forestFor :: Passive c -> ParseForest c t
	forestFor (i, j, cat) = cat :^^ ([ forests |
					   rhs <- lookupWith [] grammarMap cat,
					   forests <- children rhs i j ]
					 ++
					 [ [LeaF sym] |
					   i == j-1,
					   let sym = input !! i,
					   cat `elemSet` terminal sym ])

        children :: [c] -> Int -> Int -> [[ParseForest c t]]
	children []     i k = [ [] | i == k ]
	children (c:cs) i k = [ forests : rest |
				i <= k,
				(j,forests) <- lookupWith [] edgeForestMap (i,c),
				rest <- children cs j k ]


--------------------------------------------------
-- building parse trees

buildTrees :: Ord c => Grammar c t -> [Passive c] -> [t] -> [ParseTree c t]
buildTrees (grammar :: Grammar c t) = build
  where
    (_, start, terminal, productions) = grammar

    grammarMap :: Map c [[c]]
    grammarMap = makeMapWith (++) [ (cat,[rhs]) | (cat,rhs) <- elems productions ]

    build passiveEdges input = case lookup (0, length input, start) edgeTrees of
			         Just trees -> trees
				 Nothing    -> []
      where
        edgeTrees :: [ (Passive c, [ParseTree c t]) ]
	edgeTrees = [ (edge, treesFor edge) | edge <- passiveEdges ]

        edgeTreesMap :: Map (Int, c) [(Int, [ParseTree c t])]
	edgeTreesMap = makeMapWith (++) [ ((i,c), [(j,trees)]) |
					  ((i,j,c), trees) <- edgeTrees ]

        treesFor :: Passive c -> [ParseTree c t]
	treesFor (i, j, cat) = [ cat :^ trees |
				 rhs <- lookupWith [] grammarMap cat,
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

