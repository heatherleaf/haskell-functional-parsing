

--------------------------------------------------
-- The LR parsing table, optimized according to 
-- the first exercise in section 6.5


module LRTableOptimized where

import OrdSet
import OrdMap
import Grammar

import qualified LRTableOriginal as LROrig


--------------------------------------------------
-- the types of the original and the new LR states 

type IState  c = LROrig.LRState c
type LRState c = Int


--------------------------------------------------
-- the type of LR tables

type LR0Table c = (LRState c -> c -> [LRState c],
		   LRState c -> [(c, Int)],
		   LRState c -> Bool,
		   LRState c,
		   Set (LRState c))


--------------------------------------------------
-- creating an optimized LR table from the original

lr0table :: Ord c => Grammar c t -> LR0Table c
lr0table (grammar@(categories, _, _, _)::Grammar c t) 
    = (shift, reduce, accept, startState, allStates)
  where
    (shiftIS, reduceIS, acceptIS, startIS, allIS) = LROrig.lr0table grammar

    shift :: LRState c -> c -> [LRState c]
    shift state cat = case shiftTable ? (state,cat) of
		        Just state' -> [state']
			Nothing     -> []
    
    reduce :: LRState c -> [(c, Int)]
    reduce = lookupWith [] reduceTable 
    
    accept :: LRState c -> Bool
    accept = (`elemSet` acceptingStates)
    
    startState :: LRState c 
    startState = is2s startIS

    allStates :: Set (LRState c)
    allStates = makeSet (map is2s (elems allIS))
    
    acceptingStates :: Set (LRState c)
    acceptingStates = makeSet (map is2s (filter acceptIS (elems allIS)))

    shiftTable :: Map (LRState c, c) (LRState c)
    shiftTable = ordMap [ ((is2s is,c), is2s is') | 
			  is <- elems allIS, 
			  c <- elems categories, 
			  is' <- shiftIS is c ]

    reduceTable :: Map (LRState c) [(c, Int)]
    reduceTable = ordMap [ (is2s is, rs) | 
	 		   is <- elems allIS, 
			   let rs = reduceIS is, 
			   not (null rs) ]

    is2s :: IState c -> LRState c
    is2s = findIn (zip (elems allIS) [0..])
      where findIn abs a = let Just b = lookup a abs in b


