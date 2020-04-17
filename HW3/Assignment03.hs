module Assignment03 where

import RegEx
import SLG

import Data.List (nub)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

bigrams :: [a] -> [(a, a)]
bigrams str = case str of (x:[]) -> [] -- if it's the last letter, return nothing, end recursion 
                          (x:xs) -> (x, (head xs)) : bigrams (xs) -- create a tuple with the first two letters, recurse through rest 
                          
--pretty :: (Eq a) => [(a, a)] -> a
--pretty [] = [] -- empty string return empty  
--pretty :: (a,a) -> a 
--pretty str = case fst str of [] -> []
--                  isChained 
--isChained str of True -> head (fst str) : pretty (snd str)
--                 False -> []
-- if isChained is true, return only the first element 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows = undefined

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes = undefined

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]

forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward = undefined

-- MORE EXAMPLE USAGE:
-- backward g2 1 "cat"
-- => [["cat"],["the","cat"],["fat","cat"]]
-- backward g2 2 "very"
-- => [["very"],["the","very","very"],["very","very","very"],["the","very"],
--    ["very","very"]]

backward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
backward = undefined

generates :: (Eq sy) => SLG sy -> Int -> [[sy]]
generates = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

occurrences :: Int -> (RegEx a) -> (RegEx a)
occurrences = undefined

optional :: (RegEx a) -> (RegEx a)
optional = undefined
