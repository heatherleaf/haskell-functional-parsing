

--------------------------------------------------
-- Grammar-collecting parser combinators,
-- optimized version for efficiency and generality

-- the collected grammar will only consist of
-- unit and binary productions,
-- which means that LR parsing and 
-- (a variant of) CYK parsing are possible

-- the parser uses a chart of passive edges
-- when calculating parse results


module CollectingParserToChart (CollectingParser, 
				grammarSize, 
				grammarInfo, 
				printGrammar) where

import Grammar
import Parser
import Ref
import OrdSet 
import OrdMap
import FinalChart
import List (nub, intersperse)
import qualified Array

import qualified ChartParserToChart as PRS (process)
-- import qualified CYKParserUnitToChart as PRS (process)
-- import qualified LRParserToChart as PRS (processWith)

err str = error ("CollectingParserToChart: " ++ str)


--------------------------------------------------
-- when LR parsing, the grammar has to be augmented

augmentAndProcess :: Ord s => Grammar Int s -> [s] -> [Passive Int]
augmentAndProcess = PRS.process               -- when Chart and CYK parsing
-- augmentAndProcess = PRS.processWith (-1)      -- when LR parsing


--------------------------------------------------
-- the type of the collecting parser,
-- grammatical categories, and other helper types

data CollectingParser s a  = P (Cat s) [a] (Collect s) (Semantics s a)

data Cat s = ZeroCat | UnitCat | SymCat s |
             ProdCat (Ref [Production (Cat s)]) 
             deriving Eq

type Grammar'  s   = ([Cat s], [Production (Cat s)])
type Collect   s   = Grammar' s -> Grammar' s
type Semantics s a = ParseTree (Cat s) s -> [a]

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' a abs = let Just b = lookup a abs in b


--------------------------------------------------
-- creating a category and extending the grammar

makeCat :: [Production (Cat s)] -> Cat s
makeCat prods = ProdCat (ref prods)

extendGrammar :: Ord s => Cat s -> [Production (Cat s)] -> Collect s -> Collect s
extendGrammar cat prods collect = collect'
  where collect' grammar@(cats, prods')
            | cat `elem` cats = grammar
            | otherwise       = collect (cat:cats, prods++prods')


--------------------------------------------------
-- the parser instance declarations
-- (Monoid, Monad, Functor, Sequence, Symbol and Parser)

instance Ord s => Monoid (CollectingParser s) where
  zero = P cat empties collect sem
    where collect = extendGrammar cat prods id
          cat     = ZeroCat
          empties = []
          prods   = []
          sem _   = err ("the zero parser should never succeed. " ++
                         "Please contact the author of the library")

  ~(P pcat pempties pcollect psem) <+> ~(P qcat qempties qcollect qsem) = P cat empties collect sem
    where collect = extendGrammar cat prods (pcollect . qcollect)
          cat     = makeCat prods
          empties = pempties ++ qempties
          prods   = [(cat, [pcat]), (cat, [qcat])]
          sem (_:^[tree@(cat':^_)]) 
                  = (if cat'==pcat then psem else qsem) tree
          sem _   = err ("the (<+>) parser succeeded with the wrong kind of result. " ++
                         "Please contact the author of the library")

  anyof ps = P cat empties collect sem
    where collect = extendGrammar cat prods (foldr (.) id [ pcollect | P _ _ pcollect _ <- ps ])
          cat     = makeCat prods
          empties = concat [ pempties | P _ pempties _ _ <- ps ]
	  psems   = [ (pcat, psem) | P pcat _ _ psem <- ps ]
          prods   = [ (cat, [pcat]) | P pcat _ _ _ <- ps ]
          sem (_:^[tree@(cat':^_)]) 
                  = lookup' cat' psems tree
          sem _   = err ("the anyof parser succeeded with the wrong kind of result. " ++
                         "Please contact the author of the library")


instance Ord s => Monad (CollectingParser s) where
  return a = P cat empties collect sem
    where collect = extendGrammar cat prods id
          cat     = UnitCat
          empties = [a]
          prods   = []
          sem _   = err ("the return parser succeeded with the wrong kind of result. " ++
                         "Please contact the author of the library")

  (>>=) = err "(>>=) is not implemented"


instance Ord s => Functor (CollectingParser s) where
  fmap f ~(P cat empties collect sem) = P cat (map f empties) collect (map f . sem)


instance Ord s => Sequence (CollectingParser s) where
  ~(P pcat pempties pcollect psem) <*>  ~(P qcat qempties qcollect qsem) = P cat empties collect sem
    where collect = extendGrammar cat prods (pcollect . qcollect)
          cat     = makeCat prods
          empties = [ p q | p <- pempties, q <- qempties ]
          pempty  = not (null pempties)
          qempty  = not (null qempties)
          prods   = [(cat, [pcat, qcat])] ++
                    (if pempty then [(cat, [qcat])] else []) ++
                    (if qempty then [(cat, [pcat])] else [])

          sem     = case (pempty, qempty) of
                      (False, False) -> semFF
                      (False, True ) -> semFT
                      (True , False) -> semTF
                      (True , True ) -> semTT
          
          semFF (_:^[ptree,qtree]) 
                  = [ p q | p <- psem ptree, q <- qsem qtree ]
          semFF _ = err ("the (<*>) parser succeeded with the wrong kind of result. " ++
                         "Please contact the author of the library")

          semFT (_:^[ptree])
                  = [ p q | p <- psem ptree, q <- qempties ]
          semFT t = semFF t
          
          semTF (_:^[qtree])
                  = [ p q | p <- pempties, q <- qsem qtree ]
          semTF t = semFF t
          
          semTT (_:^[tree@(cat':^_)])
                  = if cat'==pcat then [ p q | p <- psem tree, q <- qempties ]
                                  else [ p q | p <- pempties, q <- qsem tree ]
          semTT t = semFF t


instance InputSymbol s => Symbol (CollectingParser s) s where
  sym s = P cat empties collect sem
    where collect = extendGrammar cat prods id
          cat     = SymCat s
          empties = []
          prods   = []
          sem _   = [s]

  sat p = anyof (map sym (filter p symbols))


instance Ord s => Parser (CollectingParser s) s where
  parse = err "parse is not implemented"

  parseFull (P start empties collect sem) = parseInput
    where grammar           = makeGrammar start grammar'
          grammar'@(cats,_) = collectGrammar collect
          sem'              = sem . mapTree (catsArray Array.!) id 
	  catsArray         = Array.listArray (0,length cats-1) cats
	  processPRS        = augmentAndProcess grammar
	  buildPRS          = buildTrees grammar
          parseInput []     = empties
	  parseInput input  = concatMap sem' (buildPRS (processPRS input) input)


--------------------------------------------------
-- collecting the grammar

collectGrammar :: Ord s => Collect s -> Grammar' s
collectGrammar collect = simplifyGrammar (collect ([], []))

simplifyGrammar :: Ord s => Grammar' s -> Grammar' s
simplifyGrammar (cats, prods) = (cats', prods')
  where prods' = [ prod | prod@(_,rhs) <- prods, all (`elem` cats') rhs ]
	cats'  = nub ([ c | (c,_) <- prods ] ++ [ c | (_,rhs) <- prods, c@(SymCat _) <- rhs ])


--------------------------------------------------
-- building the grammar

makeGrammar :: Ord s => Cat s -> Grammar' s -> Grammar Int s
makeGrammar start (cats, prods) = (cats', start', term', prods')
  where cats'  = makeSet [0..length cats-1]
        start' = ncat start
        term'  = lookupWith emptySet (mkMapSet [ (s,ncat (SymCat s)) | s <- terms ])
	terms  = nub [ s | (_,rhs) <- prods, SymCat s <- rhs ]
        prods' = makeSet [ (ncat cat, map ncat rhs) | (cat, rhs) <- prods ]
        ncat c = lookup' c (zip cats [0..])

mkMapSet :: (Ord s, Ord a) => [(s, a)] -> Map s (Set a)
mkMapSet ass = makeMapWith (<++>) [ (s,unitSet a) | (s,a) <- ass ]


--------------------------------------------------
-- the size of the grammar
-- (number of categories, number of terminals, number of productions, number of empty results)

grammarSize :: Ord s => CollectingParser s a -> (Int, Int, Int, Int)
grammarSize (P start empties collect _) = (length cats, length terms, length prods, length empties)
  where (cats, prods) = collectGrammar collect
        terms         = nub [ s | (_,rhs) <- prods, SymCat s <- rhs ]


--------------------------------------------------
-- printing information about the grammar

grammarInfo :: Ord s => CollectingParser s a -> IO ()
grammarInfo gr = do let (ncs, nts, nps, nes) = grammarSize gr
                    putStrLn ("Nr. categories:    " ++ show ncs)
                    putStrLn ("Nr. terminals:     " ++ show nts)
                    putStrLn ("Nr. productions:   " ++ show nps)
                    putStrLn ("Nr. empty results: " ++ show nes)


--------------------------------------------------
-- printing the grammar 

printGrammar :: (Ord s, Show s) => CollectingParser s a -> IO ()
printGrammar gr@(P start _ collect _)
    = do grammarInfo gr
	 putStrLn ""
	 putStrLn ("Categories:    " ++ concat (intersperse " " (map (showCat cats) cats)))
	 putStrLn ("Start cat:     " ++ showCat cats start)
	 putStrLn ("Terminals:     " ++ show terms)
	 putStrLn ("Productions:   " ++ concat (intersperse "\n               " (map (showProd cats) prods)))
  where (cats, prods) = collectGrammar collect
	terms         = nub [ s | (_,rhs) <- prods, SymCat s <- rhs ]


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




