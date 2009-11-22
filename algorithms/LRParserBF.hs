

--------------------------------------------------
-- Breadth-first LR parsing 
-- as described in sections 6.2.3 and 6.3.2


module LRParserBF (parse, parseWith) where

import Grammar
import OrdSet
import LRTableOptimized
import LRAugment

err str = error ("LRParserBF: " ++ str)


--------------------------------------------------
-- augment the grammar with a new starting category
-- and then parse the input

parseWith :: (Ord c, Ord t) => c -> Grammar c t -> [t] -> [ParseTree c t]
parseWith start = parse . augmentGrammarWith start 


--------------------------------------------------
-- breadth-first LR parsing 

type Stack' c t = Stack (LRState c) (ParseTree c t)

parse :: Ord c => Grammar c t -> [t] -> [ParseTree c t]
parse (grammar@(_, start, terminal, _) :: Grammar c t)
    | notAugmented grammar = err "the input grammar is not augmented"
    | cyclic grammar       = err "the input grammar is cyclic"
    | otherwise            = parse'
  where 
    (shift, reduce, accept, startState, _) = lr0table grammar

    parse' :: [t] -> [ParseTree c t]
    parse' input = [ tree |
		     stack <- processBF startStacks input,
		     let ([tree], _) = pop 1 stack ]

    startStacks :: [Stack' c t]
    startStacks = [(startState, [])]

    processBF :: [Stack' c t] -> [t] -> [Stack' c t]
    processBF []     _     = []
    processBF stacks input = shiftBF stacks' input 
      where stacks' = concat [ reduceS stack | stack <- stacks ]

    shiftBF :: [Stack' c t] -> [t] -> [Stack' c t]
    shiftBF stacks []          = [ stack | stack <- stacks, accept (top stack) ]
    shiftBF stacks (sym:input) = processBF stacks' input
      where stacks' = concat [ shiftS stack sym | stack <- stacks ]
    
    reduceS :: Stack' c t -> [Stack' c t]
    reduceS stack = stack : concat [ reduceS (push state tree stack') |
				     (cat, n) <- reduce (top stack),
				     let (popped, stack') = pop n stack,
				     let tree = cat :^ popped,
				     state <- shift (top stack') cat ]

    shiftS :: Stack' c t -> t -> [Stack' c t]
    shiftS stack sym = [ push state tree stack |
			 cat <- elems (terminal sym),
			 let tree = cat :^ [Leaf sym],
			 state <- shift (top stack) cat ]


--------------------------------------------------
-- parse stacks, for depth- and breadth-first parsing

type Stack st res = (st, [(res, st)])

pop :: Int -> Stack st res -> ([res], Stack st res)
pop n = pop' n [] 
  where pop' 0 popped stack = (popped, stack)
        pop' n popped (_, (res,state):stack) = pop' (n-1) (res:popped) (state,stack)

push :: st -> res -> Stack st res -> Stack st res
push state res (state', stack) = (state, (res,state'):stack)

top :: Stack st res -> st
top (state, _) = state


