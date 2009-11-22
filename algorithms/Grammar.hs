

--------------------------------------------------
-- Basic definitions, functions and predicates on 
-- context-free grammars, from section 2.3


module Grammar (Grammar, 
		Production, 
		ParseTree(..),
		empties, 
		chomskyNormalForm,
		cyclic,
		leftRecursive) where

import OrdSet


--------------------------------------------------
-- context-free grammars and productions

type Grammar c t = (Set c, c, t -> Set c, Set (Production c))

type Production c = (c, [c])


--------------------------------------------------
-- parse trees

data ParseTree c t = Leaf t | c :^ [ParseTree c t] 
		     deriving (Eq, Ord)

instance (Show c, Show t) => Show (ParseTree c t) where
  show (Leaf t)     = show t
  show (c :^ trees) = show c ++ "^" ++ show trees


--------------------------------------------------
-- the set of empty categories

empties :: Ord c => Grammar c t -> Set c
empties (_, _, _, productions) = empties' emptySet
  where empties' cats | cats==cats' = cats
		      | otherwise   = empties' cats'
	  where cats' = makeSet [ cat | (cat, rhs) <- elems productions,
				        all (`elemSet` cats) rhs ]


--------------------------------------------------
-- Chomsky normal form

chomskyNormalForm :: Ord c => Grammar c t -> Bool
chomskyNormalForm (_, _, _, productions) 
    = null (filter (2 /=) (map (length.snd) (elems productions)))


--------------------------------------------------
-- cyclic grammar

cyclic :: Ord c => Grammar c t -> Bool
cyclic (grammar@(categories, _, _, productions)::Grammar c t) 
    = any isCyclic (elems categories)
  where isCyclic :: c -> Bool
	isCyclic c = c `elemSet` limit more (more c)

	more :: c -> Set c
        more c = makeSet [ d | (c', ds) <- elems productions, c==c',
			       d <- selectUnits ds ]

        emptyCat  = (`elemSet` empties grammar)

        selectUnits []     = []
	selectUnits (x:xs) = case (emptyCat x, all emptyCat xs) of
			       (True,  True)  -> x : selectUnits xs
			       (True,  False) ->     selectUnits xs
			       (False, True)  -> [x]
			       (False, False) -> []


--------------------------------------------------
-- left-recursion

leftRecursive :: Ord c => Grammar c t -> Bool
leftRecursive (grammar@(categories, _, _, productions)::Grammar c t) 
    = any isLrec (elems categories)
  where isLrec :: c -> Bool
	isLrec c = c `elemSet` limit more (more c)

	more :: c -> Set c
        more c = makeSet [ d | (c', ds) <- elems productions, c==c',
			       d <- selectLeftcorner ds ]

        emptyCat  = (`elemSet` empties grammar)

        selectLeftcorner []     = []
	selectLeftcorner (x:xs) 
	    | emptyCat x = x : selectLeftcorner xs
	    | otherwise  = [x]



