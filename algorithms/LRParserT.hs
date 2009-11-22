

--------------------------------------------------
-- Approximative Tomita LR parsing 
-- as described in sections 6.2.4, 6.2.5, 6.3.3 and 6.3.4


module LRParserT (parse, parseWith) where

import Grammar
import OrdSet
import LRTableOptimized
import LRAugment

err str = error ("LRParserT: " ++ str)


--------------------------------------------------
-- augment the grammar with a new starting category
-- and then parse the input

parseWith :: (Ord c, Ord t) => c -> Grammar c t -> [t] -> [ParseTree c t]
parseWith start = parse . augmentGrammarWith start 


--------------------------------------------------
-- approximative Tomita LR parsing to parse trees

type TStack'  c t = TStack  (LRState c) (ParseTree c t)
type TStacks' c t = TStacks (LRState c) (ParseTree c t)

parse :: Ord c => Grammar c t -> [t] -> [ParseTree c t]
parse (grammar@(_, start, terminal, _) :: Grammar c t)
    | notAugmented grammar = err "the input grammar is not augmented"
    | cyclic grammar       = err "the input grammar is cyclic"
    | otherwise            = parse'
  where 
    (shift, reduce, accept, startState, _) = lr0table grammar

    parse' :: [t] -> [ParseTree c t]
    parse' input = [ tree |
		     stack <- processT startStacks input,
		     ([tree], _) <- popT 1 stack ]

    startStacks :: TStacks' c t
    startStacks = [unitT startState]

    processT :: TStacks' c t -> [t] -> TStacks' c t
    processT []     _     = []
    processT stacks input = shiftT stacks' input 
      where stacks' = unionT [ reduceTS stack | stack <- stacks ]

    shiftT :: TStacks' c t -> [t] -> TStacks' c t
    shiftT stacks []          = [ stack | stack <- stacks, accept (topT stack) ]
    shiftT stacks (sym:input) = processT stacks' input
      where stacks' = unionT [ shiftTS stack sym | stack <- stacks ]
    
    reduceTS :: TStack' c t -> TStacks' c t
    reduceTS stack = [stack] `plusT`
		     unionT [ reduceTS (pushT state tree stack') |
			      (cat, n) <- reduce (topT stack),
			      (popped, stack') <- popT n stack,
			      let tree = cat :^ popped,
			      state <- shift (topT stack') cat ]

    shiftTS :: TStack' c t -> t -> TStacks' c t
    shiftTS stack sym = joinT [ pushT state tree stack |
				cat <- elems (terminal sym),
				let tree = cat :^ [Leaf sym],
				state <- shift (topT stack) cat ]



--------------------------------------------------
-- Tomita stacks, simplified version

-- this is not the version described in section 6.3.4,
-- but a simplified variant which is more efficient in most cases

data TStack  st res = st ::: [(res, TStack st res)]
type TStacks st res = [TStack st res]

popT :: Int -> TStack st res -> [([res], TStack st res)]
popT n = popT' n []
  where popT' 0 popped stack        = [(popped, stack)]
        popT' n popped (_:::stacks) = concat [ popT' (n-1) (res:popped) stack |
					       (res, stack) <- stacks ]

pushT :: st -> res -> TStack st res -> TStack st res
pushT state res stack = state ::: [(res, stack)]

topT :: TStack st res -> st
topT (state ::: _) = state

unitT :: st -> TStack st res
unitT state = state ::: []

joinT :: Ord st => [TStack st res] -> TStacks st res
joinT xs = unionT [ [x] | x <- xs ]

unionT :: Ord st => [TStacks st res] -> TStacks st res
unionT []  = []
unionT [x] = x
unionT xys = let (xs,ys) = split xys in unionT xs `plusT` unionT ys
  where split []        = ([], [])
        split [x]       = ([x], [])
        split (x:y:xys) = let (xs, ys) = split xys in (x:xs, y:ys)

plusT :: Ord st => TStacks st res -> TStacks st res -> TStacks st res
plusT [] ys = ys
plusT xs [] = xs
plusT xs@(x@(a:::as):xs') ys@(y@(b:::bs):ys')
    = case compare a b of
        LT -> x : plusT xs' ys
        GT -> y : plusT xs  ys'
        EQ -> (a ::: (as ++ bs)) : plusT xs' ys'


