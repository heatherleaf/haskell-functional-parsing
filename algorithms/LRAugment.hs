

--------------------------------------------------
-- augmentation of grammars for LR parsing
-- as explained in the beginning of section 6.5


module LRAugment (notAugmented, augmentGrammarWith) where

import Grammar
import OrdSet

err str = error ("LRAugment: " ++ str)


--------------------------------------------------
-- does the grammar have to be augmented?

notAugmented :: Ord c => Grammar c t -> Bool
notAugmented (_, start, _, productions)
    = case [ rhs | (start', rhs) <- elems productions, start==start' ] of
	[[_]] -> False
        _     -> True


--------------------------------------------------
-- augment the grammar with a new starting category

augmentGrammarWith :: Ord c => c -> Grammar c t -> Grammar c t
augmentGrammarWith start' ((categories, start, terminal, productions)::Grammar c t)
    | start' `elem` usedCats = err "the grammar could not be augmented"
    | otherwise              = (categories', start', terminal, productions')
  where categories'  = unitSet start' <++> categories
        productions' = unitSet (start',[start]) <++> productions
	usedCats     = concat [ cat:rhs | (cat, rhs) <- elems productions ]



