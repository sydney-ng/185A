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

valP' :: Ord sy => ProbSLG sy -> [sy] -> Double
valP' = undefined

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
