module Assignment02 where

-- Imports just a few things that we have seen from the standard Prelude
-- module. (If there is no explicit 'import Prelude' line, then the entire
-- Prelude module is imported.)
import Prelude((+), (-), (*), (<), (>), (++), not, Bool(..), Char, undefined)

import Recursion

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

times :: Numb -> Numb -> Numb
times a b = case a of E -> E 
                      one -> case b of E -> E
                                       S b' -> S (times a b')
                      S a' -> S (times a' b)

-- times m n = case m of E -> case n of E -> E ss
--                                     S n'' -> S (times E n'')
--                      S m' -> case n of E -> E  
--
--                                       S n' -> S (times m n')
-- pseudocode:
-- if m = 0 -> decrement n until 0
--    else -> keep m the same, decrement n til zero s

equal :: Numb -> Numb -> Bool 
equal n m = case m of E -> case n of E -> True
                                     S n' -> False 
                      S m' -> case n of E -> False 
                                        S n'' -> equal n'' m'
