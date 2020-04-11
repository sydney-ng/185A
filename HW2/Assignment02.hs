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
times m n = case m of E -> S E  
                      S n' -> case n' of E -> S E 
                                         S m' -> times m' n' 

equal :: Numb -> Numb -> Bool 
equal n m = case m of E -> case n of E -> True
                                     S n' -> False 
                      S m' -> case n of E -> False 
                                        S n'' -> equal n'' m'

bigger :: Numb -> Numb -> Numb
bigger = undefined

count :: (a -> Bool) -> [a] -> Numb
count = undefined

remove :: (a -> Bool) -> [a] -> [a]
remove a b = undefined
-- 

prefix :: Numb -> [a] -> [a]
prefix = undefined
-- 

depth :: WFF -> Numb
depth = undefined
