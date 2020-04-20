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
                          
pretty :: (Eq a) => [(a, a)] -> [a]   
pretty str = case isChained str of True -> case str of ((t_head, t_tail): []) -> [t_head, t_tail] -- for last element 
                                                       ((t_head, t_tail): rest) -> t_head : pretty rest
                                                       [] -> []
                                   False -> [] 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- (Eq sy) is for equality 
--follows :: (Eq sy) => SLG sy -> sy -> [sy]
--follows ([start]:[end]:[]) target  = []
--follows ([start]:[end]:([x, xs]: [])) target = case target of xs -> [x]
--follows ([start]: [end] : ([x, xs]: rest)) target = case target of xs -> x : follows ([start]:[end]: [rest]) target
-- case transition of ((head, tail): []) -> [] 

--                                                -> (tail xs)
precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes grammar target = case grammar of (start , end , []) -> [] 
                                          (start , end , (x, xs)  : rest) -> case (xs == target) of True -> x: precedes (start , end , rest) target 
                                                                                                    False -> precedes (start , end , rest) target

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows grammar target = case grammar of (start , end , []) -> [] 
                                         (start , end , (x, xs) : rest) -> case (x == target) of True -> xs: follows (start , end , rest) target 
                                                                                                 False -> follows (start , end , rest) target                                                                                                   
-- break grammar down into each element 
-- check if it has at least 2 items
-- if snd item == target, append the fst item 
-- else return []

--follows :: (Eq sy) => SLG sy -> sy -> [sy]
--follows (s, e, transitions) target = filter (\x -> (x 'elem' [target])) [transitions]

--follows2 :: (Eq sy) => SLG sy -> sy -> [sy]
--follows2 grammar target = map (filter (\n -> snd n == target)) grammar

--precedes (start, final, transition) target = filter (\x -> (`elem` target x)) transition

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
occurrences 0 rest = One -- one is empty Set  
occurrences n r = case n of 0 -> One 
                            n -> Concat r (occurrences (n-1) r)

optional :: (RegEx a) -> (RegEx a)
optional r = Alt r One -- one is empty set 
