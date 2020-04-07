module Assignment01 where

data WFF = T | F | Neg WFF
         | Conj WFF WFF | Disj WFF WFF deriving Show

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

denotation :: WFF -> Bool
denotation f = case f of {T -> True; F -> False; Neg x -> not (denotation (x)); Conj x y -> (denotation x) && (denotation y); Disj x y -> (denotation x) || (denotation y)} 

-- notes:
-- T should return True
-- F should return False
-- Neg should return the opposite value (T-> False)  
-- Conj should return x AND y boolean
-- Disj should return x OR y boolean 

-- test cases: 
-- *Assignment01> denotation (Conj T F)
-- False
-- *Assignment01> denotation (Conj F F)
-- False
-- *Assignment01> denotation (Disj F F)
-- False
-- *Assignment01> denotation (Disj T F)
-- True
-- *Assignment01> denotation (Disj F T)
-- True
-- *Assignment01> denotation (T)
-- True
-- *Assignment01> denotation (F)
-- False
-- *Assignment01> denotation (Neg T)
-- False
-- *Assignment01> denotation (Neg F)
-- True

