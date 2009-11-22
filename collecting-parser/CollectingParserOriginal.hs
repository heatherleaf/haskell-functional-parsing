

--------------------------------------------------
-- Grammar-collecting parser combinators,
-- as described in section 8.2

-- there are some changes to the thesis;
-- e.g. the type of categories is slightly different


module CollectingParserOriginal (CollectingParser, 
				 grammarSize, 
				 grammarInfo, 
				 printGrammar) where

import Grammar
import Parser
import Ref
import OrdSet
import OrdMap
import List (nub, intersperse)

import qualified ChartParserOptimized as PRS 

err str = error ("CollectingParserOriginal: " ++ str)


--------------------------------------------------
-- the type of the collecting parser,
-- grammatical categories, and other helper types and functions

data CollectingParser s a  = P (Cat s) (Collect s) (Semantics s a)

data Cat s = ZeroCat | UnitCat | SymCat s |
	     ProdCat (Ref [Production (Cat s)]) 
	     deriving Eq

type Grammar'  s   = ([Cat s], Cat s, [(s,Cat s)], [Production (Cat s)])
type Collect   s   = Grammar' s -> Grammar' s
type Semantics s a = ParseTree (Cat s) s -> a

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' a abs = let Just b = lookup a abs in b


--------------------------------------------------
-- creating a category and extending the grammar

makeCat :: [Production (Cat s)] -> Cat s
makeCat prods = ProdCat (ref prods)

extendGrammar :: Ord s => Cat s -> [Production (Cat s)] -> [(s,Cat s)] -> Collect s -> Collect s
extendGrammar cat prods term collect = collect'
  where collect' grammar@(cats, start, term', prods')
	    | cat `elem` cats = grammar
	    | otherwise       = collect (cat:cats,
					 start,
					 term ++ term',
					 prods ++ prods')


--------------------------------------------------
-- the parser instance declarations
-- (Monoid, Monad, Functor, Sequence, Symbol and Parser)

instance Ord s => Monoid (CollectingParser s) where
  zero = P cat collect sem
    where collect = extendGrammar cat prods term id
	  cat     = ZeroCat
	  prods   = []
	  term    = []
	  sem _   = err ("the zero parser should never succeed. " ++
			 "Please contact the author of the library")

  ~(P pcat pcollect psem) <+> ~(P qcat qcollect qsem) = P cat collect sem
    where collect = extendGrammar cat prods term (pcollect . qcollect)
	  cat     = makeCat prods
	  prods   = [(cat, [pcat]), (cat, [qcat])]
	  term    = []
	  sem (_:^[tree@(cat':^_)]) 
	          = (if cat'==pcat then psem else qsem) tree
	  sem _   = err ("the (<+>) parser succeeded with the wrong kind of result. " ++
			 "Please contact the author of the library")


instance Ord s => Monad (CollectingParser s) where
  return a = P cat collect sem
    where collect = extendGrammar cat prods term id
	  cat     = UnitCat
	  prods   = [(cat, [])]
	  term    = []
	  sem _   = a

  (>>=) = err "(>>=) is not implemented"


instance Ord s => Functor (CollectingParser s) where
  fmap f ~(P cat collect sem) = P cat collect (f . sem)


instance Ord s => Sequence (CollectingParser s) where
  ~(P pcat pcollect psem) <*>  ~(P qcat qcollect qsem) = P cat collect sem
    where collect = extendGrammar cat prods term (pcollect . qcollect)
	  cat     = makeCat prods
	  prods   = [(cat, [pcat, qcat])]
	  term    = []
	  sem (_:^[ptree,qtree]) 
	          = (psem ptree) (qsem qtree)
	  sem _   = err ("the (<*>) parser succeeded with the wrong kind of result. " ++
			 "Please contact the author of the library")


instance InputSymbol s => Symbol (CollectingParser s) s where
  sym s = P cat collect sem
    where collect = extendGrammar cat prods term id
	  cat     = SymCat s
	  prods   = []
	  term    = [(s,cat)]
	  sem _   = s

  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (CollectingParser s) s where
  parse = err "parse is not implemented"

  parseFull (P start collect sem) = map sem' . PRS.parse grammar
    where grammar               = makeGrammar grammar'
	  grammar'@(cats,_,_,_) = collect emptyGrammar
	  emptyGrammar          = ([], start, [], [])
	  sem' tree             = sem (mapTree (cats!!) id tree)


--------------------------------------------------
-- building the grammar

makeGrammar :: Ord s => Grammar' s -> Grammar Int s
makeGrammar (cats, start, term, prods) = (cats', start', term', prods')
  where cats'  = makeSet [0 .. length cats-1]
	start' = ncat start
	term'  = lookupWith emptySet (mkMapSet [ (s,ncat cat) | (s,cat) <- term ])
	prods' = makeSet [ (ncat cat, map ncat rhs) | (cat, rhs) <- prods ]
	ncat c = lookup' c (zip cats [0..])

mkMapSet :: (Ord s, Ord a) => [(s, a)] -> Map s (Set a)
mkMapSet ass = makeMapWith (<++>) [ (s,unitSet a) | (s,a) <- ass ]


--------------------------------------------------
-- the size of the grammar
-- (number of categories, number of terminals, number of productions)

grammarSize :: Ord s => CollectingParser s a -> (Int, Int, Int)
grammarSize (P start collect _) = (length cats, length terminals, length productions)
  where (cats, _, termprods, productions) = collect emptyGrammar
	terminals    = nub (map fst termprods)
	emptyGrammar = ([], start, [], [])


--------------------------------------------------
-- printing information about the grammar

grammarInfo :: Ord s => CollectingParser s a -> IO ()
grammarInfo gr = do let (ncs, nts, nps) = grammarSize gr
		    putStrLn ("Nr. categories:  " ++ show ncs)
		    putStrLn ("Nr. terminals:   " ++ show nts)
		    putStrLn ("Nr. productions: " ++ show nps)


--------------------------------------------------
-- printing the grammar 

printGrammar :: (Ord s, Show s) => CollectingParser s a -> IO ()
printGrammar gr@(P start collect _)
    = do grammarInfo gr
	 putStrLn ""
	 putStrLn ("Categories:    " ++ concat (intersperse " " (map (showCat cats) cats)))
	 putStrLn ("Start cat:     " ++ showCat cats start)
	 putStrLn ("Terminals:     " ++ show terms)
	 putStrLn ("Productions:   " ++ concat (intersperse "\n               " (map (showProd cats) prods)))
  where (cats, _, term, prods) = collect emptyGrammar
	emptyGrammar           = ([], start, [], []) 
	terms                  = nub [ s | (s,_) <- term ]


--------------------------------------------------
-- showing a category and a production

showCat :: Show s => [Cat s] -> Cat s -> String
showCat cats = show'
  where show' ZeroCat     = "Zero"
	show' UnitCat     = "Unit"
	show' (SymCat s)  = show s
	show' (ProdCat c) = "<" ++ show (lookup' c (zip pcats [1..])) ++ ">"
	pcats             = [ c | ProdCat c <- cats ]

showProd :: Show s => [Cat s] -> Production (Cat s) -> String
showProd cats (cat, rhs) = show' cat ++ " -> " ++ concat (intersperse " " (map show' rhs))
  where show' = showCat cats




