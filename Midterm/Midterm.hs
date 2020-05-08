module Midterm where

import ProbSLG
import Helpers

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Problem 1:
-------------------------------------------------------------------------------

follows :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
follows (ProbSLG grammar) target = case grammar of (s, sy, []) -> [] 
                                                   (start , end , (x, xs, val) : rest) -> case (x == target) of True -> (xs, val): follows (ProbSLG (start , end , rest)) target 
                                                                                                                False -> follows (ProbSLG (start , end , rest)) target   



precedes :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
precedes (ProbSLG grammar) target = case grammar of (start , end , []) -> [] 
                                                    (start , end , (x, xs, val)  : rest) -> case (xs == target) of True -> (x, val): precedes (ProbSLG (start , end , rest)) target
                                                                                                                   False -> precedes (ProbSLG (start , end , rest)) target                                                                                                                                                                                                 
valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP = undefined

-- ex: valP g1 ["the", "cat"] --> words 
-- take first word, make sure that it is the start 
-- take last word make sure it is the end 
-- run P' on it 

valP' :: Ord sy => ProbSLG sy -> [sy] -> Double
valP' g l = case l of (first_word : xs_head : []) -> helper (follows g first_word) xs_head -- return 1 beause we are done  
                      (first_word : xs_head : xs_tail ) -> (helper (follows g first_word) xs_head) * (valP' g (xs_head : xs_tail))

helper :: Ord sy => [(sy, Double)] -> sy -> Double 
helper [] _ = 1.0
helper ((answer, chance) : []) _ = chance
helper ((answer, chance): rest) target = case (answer == target) of True -> chance 
                                                                    False -> helper rest target 
-- ex: valP' g1 ["very", "fat", "cat"]  --> words_list 
-- each time take 1 word off of words_list 
-- check for what follows head of words_list
-- case of true, take it and run again w/ the next (Compound with probability) 
-- false (you can't have anything following words & return 0) 

-------------------------------------------------------------------------------
-- Problem 2:
-------------------------------------------------------------------------------

buildProbSLG :: Ord a => Corpus a -> ProbSLG a
buildProbSLG = undefined

-- A potentially helpful starting point:
-- buildProbSLG corpus = ProbSLG ([], [], [])

-------------------------------------------------------------------------------
-- Problem 3:
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.
sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = []

posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG = undefined

tag :: ProbSLG String -> String -> [(Sentence TaggedWord, Double)]
tag = undefined

tagBest :: ProbSLG String -> String -> Sentence TaggedWord
tagBest = undefined
