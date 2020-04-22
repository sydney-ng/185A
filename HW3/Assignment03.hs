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
bigrams [] = []
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

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows grammar target = case grammar of (start , end , []) -> [] 
                                         (start , end , (x, xs) : rest) -> case (x == target) of True -> xs: follows (start , end , rest) target 
                                                                                                 False -> follows (start , end , rest) target   

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes grammar target = case grammar of (start , end , []) -> [] 
                                          (start , end , (x, xs)  : rest) -> case (xs == target) of True -> x: precedes (start , end , rest) target 
                                                                                                    False -> precedes (start , end , rest) target                                                                                                                                                                                                 
-- IDEA:
-- break grammar down into each element 
-- check if it has at least 2 items
-- if snd item == target, append the fst item 
-- else return []

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]

--forward' :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
--forward' gram 0 symbol = [symbol] : []
--sforward' gram n symbol = map (\x -> forward gram (subtract 1 n) x) (follows gram symbol)

--forward3:: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward gram 0 symbol = [[symbol]]
forward gram 1 symbol = concat [[[symbol]], (map (\x -> concat [[symbol], [x]]) (follows gram symbol))] 
forward gram steps symbol = [[symbol]] ++ (concat (map (\s -> map (\t -> concat [tail s, t]) (forward gram (subtract 1 steps) (head s))) (map(\m -> [m] ++[symbol]) (follows gram symbol)))) 


-- all the leements that follow from very 
-- helper fx: \x -> map (\y -> x ++ [y]) (follows gram (last x))
-- gives you everything after "very"
-- figure out how to \z -> map (\x -> map (\y -> x ++ [y]) (follows gram (last x))) z???
-- figure out how to get from the -> the cat, the very, the fat 
--                                  -> cat is a dead end, fat is a dead end, very can keep going 
--                                  -> concatenate all the possible options 




--forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
--forward gram 0 symbol = []
--forward gram steps symbol = map (\x -> forward' steps symbol) gram
-- this will give you all the children 
-- goal: determine which transitions can be expanded n-1 times

-- forward prime will generate everything on that level for forwards 
--forward' :: (Eq sy) => SLG sy -> Int -> sy -> [sy]
--forward' gram 0 symbol = []
--forward' gram steps symbol = map (\x -> follows gram x) [] : forward' gram (subtract 1 steps) symbol

 -- follows gram symbol : forward gram (subtract 1 steps) symbol 

--forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
--forward gram steps symbol = case steps of 0 -> [symbol] : [] -- append itself to the list
--                                          _ -> follows gram symbol : forward gram (subtract 1 steps) symbol

-- try: wrapper function pattern 
-- should generalize up 
-- if you have list with one item, you can still do head/last 
-- 2 map functions 
-- use where clause to define pieces 
-- forward accumulates different input, what you pass back in you need to also save 
-- no steps -> very 
-- 1 steps -> just very 
-- 2 steps -> based off of n-1 but keep the output @ this step 

-- attempt at recursion using:
-- --                                          1 -> map (\x -> (follows gram x)) (follows gram symbol) : forward gram (subtract 1 steps) symbol 


-- for each transition 
   -- for each result in follows gram symbol, need to go one level deeper 
   --                                                                

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
