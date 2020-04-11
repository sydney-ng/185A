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
--  computes the product of two numbers using the existing add function.
times a b = case a of E -> E -- if empty, empty 
                      S E -> case b of E -> E -- zero x zero = zero 
                                       S b' -> add a (times a b') -- decrement b 
                      S a' -> case b of E -> E  -- capture zero case 
                                        S b' -> add b (times a' b) -- decrement a 

equal :: Numb -> Numb -> Bool 
-- returns True if the two numbers given are equal and False otherwise.
equal n m = case m of E -> case n of E -> True -- equal numbers, 
                                     S n' -> False -- not equal 
                      S m' -> case n of E -> False -- not equal 
                                        S n'' -> equal n'' m' -- keep decrementing both by one to match
bigger :: Numb -> Numb -> Numb
-- takes two numbers and returns the larger one.
bigger a b = case a of E -> case b of E -> a -- they're equal in value 
                                      S b' -> b -- b is bigger 
                       S a' -> case b of E -> a -- a is bigger 
                                         S b' -> S (bigger a' b') -- decrement both until one hits 0 

count :: (a -> Bool) -> [a] -> Numb
-- returns the number of el- ements in the given list for which the given function returns True.
count fx list = case list of [] -> E -- empty list, return zero 
                             x: xs -> case fx x of True -> add (S E) (count fx xs) -- add one to count
                                                   False -> count fx xs -- didn't return true, call on rest, don't count

remove :: (a -> Bool) -> [a] -> [a]
-- returns a list which is just like l but with those elements removed for which f returns True.
remove fx list = case list of [] -> [] -- empty list 
                              x: xs -> case fx x of True -> remove fx xs -- satisfy req, remove + check rest 
                                                    False -> x : (remove fx xs) -- keep + check rest 
prefix :: Numb -> [a] -> [a]
-- returns the list containing the  rst n elements of list; or if n is greater than the length of list, 
-- returns list as is. (Hint: The function will need to work recursively on both arguments.)
prefix number list = case number of E -> [] -- finished counting, add nothing 
                                    S number' -> case list of [] -> [] -- list is empty 
                                                              x: xs -> x : (prefix number' xs) -- decrement counter, append first element, continue

depth :: WFF -> Numb
-- returns the length of the longest root-to-leaf sequence of nodes in the tree for the given WFF
depth wff = case wff of T -> one -- leaf node, return one 
                        F -> one -- leaf node, return one 
                        Neg phrase1 -> add one (depth phrase1) -- calculate depth of Neg Phrase 
                        Conj phrase1 phrase2-> add one (bigger (depth phrase1) (depth phrase2)) -- add 1, choose larger of 2 phrases
                        Disj phrase1 phrase2 -> add one (bigger (depth phrase1) (depth phrase2)) -- add 1, choose larger of 2 phrases






