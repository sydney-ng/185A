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
                      S E -> case b of E -> E
                                       S b' -> add a (times a b')
                      S a' -> case b of E -> E  
                                        S b' -> add b (times a' b)

equal :: Numb -> Numb -> Bool 
equal n m = case m of E -> case n of E -> True
                                     S n' -> False 
                      S m' -> case n of E -> False 
                                        S n'' -> equal n'' m'

count :: (a -> Bool) -> [a] -> Numb
count fx list = case list of [] -> E 
                             x: xs -> case fx x of True -> add (S E) (count fx xs)
                                                   False -> count fx xs

remove :: (a -> Bool) -> [a] -> [a]
remove fx list = case list of [] -> []
                              x: xs -> case fx x of True -> remove fx xs
                                                    False -> x : (remove fx xs)
prefix :: Numb -> [a] -> [a]
prefix number list = case number of E -> [] 
                                    S number' -> case list of [] -> [] 
                                                              x: xs -> x : (prefix number' xs)