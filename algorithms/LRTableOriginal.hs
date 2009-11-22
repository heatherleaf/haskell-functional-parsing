

--------------------------------------------------
-- The LR parsing table, from section 6.5


module LRTableOriginal where

import OrdSet
import Grammar


--------------------------------------------------
-- the types of LR states and LR items

type LRState c = Set (Item c)
type Item    c = ([c], Production c)


--------------------------------------------------
-- the type of LR tables

type LR0Table c = (LRState c -> c -> [LRState c],
		   LRState c -> [(c, Int)],
		   LRState c -> Bool,
		   LRState c,
		   Set (LRState c))


--------------------------------------------------
-- creating the LR table

lr0table :: Ord c => Grammar c t -> LR0Table c
lr0table (grammar@(categories, start, _, productions)::Grammar c t) 
    = (shift, reduce, accept, startState, allStates)
  where
    shift :: LRState c -> c -> [LRState c]
    shift state cat = if isEmpty state' then [] else [state']
      where state' = nextState state cat
    
    reduce :: LRState c -> [(c, Int)]
    reduce state = [ (cat, length rhs) |
    		     ([], (cat, rhs)) <- elems state ]
    
    accept :: LRState c -> Bool
    accept state = or [ cat == start | 
			([], (cat, _)) <- elems state ]
    
    startState :: LRState c 
    startState = buildState (newItems start)
    
    allStates :: Set (LRState c)
    allStates = limit more (unitSet startState)
      where more state = makeSet (concatMap (shift state) (elems categories))

    buildState :: Set (Item c) -> LRState c
    buildState = limit more
      where more (cat:_, _) = newItems cat
    	    more _          = emptySet
    
    newItems :: c -> Set (Item c)
    newItems cat = makeSet [ (tofind, rule) |
    			     rule@(cat',tofind) <- elems productions,
    			     cat == cat' ]
    
    nextState :: LRState c -> c -> LRState c
    nextState state cat = buildState (makeSet [ (tofind, reduct) |
    						(cat':tofind, reduct) <- elems state,
    						cat == cat' ])
    

