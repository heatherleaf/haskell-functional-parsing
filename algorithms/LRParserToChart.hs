

--------------------------------------------------
-- Approximative Tomita LR parsing 
-- to a chart of passive edges
-- as described in an exercise in section 6.3.3

module LRParserToChart (process, processWith) where

import Grammar
import OrdSet
import OrdMap
import LRTableOptimized
import LRAugment
import FinalChart (Passive)

err str = error ("LRParserToChart: " ++ str)


--------------------------------------------------
-- augment the grammar with a new starting category
-- and then process the input

processWith :: Ord c => c -> Grammar c t -> [t] -> [Passive c]
processWith start = process . augmentGrammarWith start 


--------------------------------------------------
-- approximative Tomita LR parsing to a chart

type TStack'  c = TStack  (LRState c) (Passive c)
type TStacks' c = TStacks (LRState c) (Passive c)

process :: Ord c => Grammar c t -> [t] -> [Passive c]
process (grammar@(_, start, terminal, _) :: Grammar c t)
    | notAugmented grammar = err "the input grammar is not augmented"
    | cyclic grammar       = err "the input grammar is cyclic"
    | otherwise            = process'
  where 
    (shift, reduce, accept, startState, _) = lr0table grammar

    process' :: [t] -> [Passive c]
    process' input = concat (map elems (processT startStacks input 0))

    startStacks :: TStacks' c 
    startStacks = [unitT startState]

    processT :: TStacks' c -> [t] -> Int -> [Set (Passive c)]
    processT []     _     _ = []
    processT stacks input i = edges : shiftT stacks' input i 
      where stacks' = unionT [ reduceTS stack i | stack <- stacks ]
	    edges   = union [ topResulT stack | stack <- stacks' ]

    shiftT :: TStacks' c -> [t] -> Int -> [Set (Passive c)]
    shiftT stacks []          _ = []
    shiftT stacks (sym:input) i = processT stacks' input (i+1)
      where stacks' = unionT [ shiftTS stack sym i | stack <- stacks ]
    
    reduceTS :: TStack' c -> Int -> TStacks' c
    reduceTS stack i = [stack] `plusT`
                       unionT [ reduceTS (pushT state edge stack') i |
                                (cat, n) <- reduce (topT stack),
                                (popped, stack') <- popT n stack,
                                let edge = (startNode popped, i, cat),
                                state <- shift (topT stack') cat ]
      where startNode []          = i
            startNode ((j,_,_):_) = j

    shiftTS :: TStack' c -> t -> Int -> TStacks' c
    shiftTS stack sym i = joinT [ pushT state edge stack |
                                  cat <- elems (terminal sym),
                                  let edge = (i, i+1, cat),
                                  state <- shift (topT stack) cat ]


--------------------------------------------------
-- Tomita stacks, version from section 6.3.4

data TStack  st res = st ::: Map res (TStacks st res)
type TStacks st res = [TStack st res]

popT :: Ord res => Int -> TStack st res -> [([res], TStack st res)]
popT n = popT' n []
  where popT' 0 popped stack      = [(popped, stack)]
        popT' n popped (_:::smap) = concat [ popT' (n-1) (res:popped) stack |
					     (res, stacks) <- assocs smap,
                                             stack <- stacks ]

pushT :: Ord res => st -> res -> TStack st res -> TStack st res
pushT state res stack = state ::: (res |-> [stack])

topT :: TStack st res -> st
topT (state ::: _) = state

topResulT :: Ord res => TStack st res -> Set res
topResulT (_ ::: smap) = ordSet (map fst (assocs smap))

unitT :: Ord res => st -> TStack st res
unitT state = state ::: emptyMap

joinT :: (Ord st, Ord res) => [TStack st res] -> TStacks st res
joinT xs = unionT [ [x] | x <- xs ]

unionT :: (Ord st, Ord res) => [TStacks st res] -> TStacks st res
unionT []  = []
unionT [x] = x
unionT xys = let (xs,ys) = split xys in unionT xs `plusT` unionT ys
  where split []        = ([], [])
        split [x]       = ([x], [])
        split (x:y:xys) = let (xs, ys) = split xys in (x:xs, y:ys)

plusT :: (Ord st, Ord res) => TStacks st res -> TStacks st res -> TStacks st res
plusT [] ys = ys
plusT xs [] = xs
plusT xs@(x@(a:::as):xs') ys@(y@(b:::bs):ys')
    = case compare a b of
        LT -> x : plusT xs' ys
        GT -> y : plusT xs  ys'
        EQ -> (a ::: mergeWith plusT as bs) : plusT xs' ys'


