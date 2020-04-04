module Assignment01 where

data WFF = T | F | Neg WFF
         | Conj WFF WFF | Disj WFF WFF deriving Show

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

denotation :: WFF -> Bool
denotation f = case f of 
	T -> True 
	F -> False
	Neg x -> denotation (not x)
--	Conj x y -> (denotation T) (denotation F)
--	Disj x y -> Disj (denotation T) (denotation F)


removeNegs :: WFF -> WFF
removeNegs f = case f of
	T -> T
	F -> F
	Neg x -> removeNegs x
	Conj x y -> Conj (removeNegs x) (removeNegs y)
	Disj x y -> Disj (removeNegs x) (removeNegs y)