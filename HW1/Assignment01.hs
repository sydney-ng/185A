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
	Neg x -> denotation (T)
--	Conj x y -> (denotation T) (denotation F)
--	Disj x y -> Disj (denotation T) (denotation F)