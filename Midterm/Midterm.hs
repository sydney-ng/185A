module Midterm where

import ProbSLG
import Helpers
import Data.List (nub)


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

-- ex: valP g1 ["the", "cat"] --> words 
-- take first word, make sure that it is the start 
-- take last word make sure it is the end 
-- run P' on it 

valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP g l = case l of (x : xs : []) -> (initProb g x) * (finalProb g xs) * (valP' g l) 

initProb :: Ord sy => ProbSLG sy -> sy -> Double
initProb (ProbSLG (starts, finals, trans)) q = sum (map (\(q',p) -> p) (filter (\(q',p) -> q == q') starts))

finalProb :: Ord sy => ProbSLG sy -> sy -> Double
finalProb (ProbSLG (starts, finals, trans)) q = sum (map (\(q',p) -> p) (filter (\(q',p) -> q == q') finals))


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
buildProbSLG corpus = ProbSLG ((calculate_start_probability corpus), (calculate_end_probability corpus), (calculate_trans_probability corpus))

calculate_start_probability :: Ord a => Corpus a -> [(a, Double)] 
calculate_start_probability corpus = let frequency_pairs = frequencies (extract_starts corpus) 
                                         total = total_number_of_items corpus 
                                     in 
                                         map (\(symbol, probability) -> (symbol, divide probability total) ) frequency_pairs

calculate_end_probability :: Ord a => Corpus a -> [(a, Double)] 
calculate_end_probability corpus = let frequency_pairs = frequencies (extract_end corpus) 
                                       total = total_number_of_items corpus 
                                     in 
                                         map (\(symbol, probability) -> (symbol, divide probability total) ) frequency_pairs

calculate_trans_probability :: Ord a => Corpus a -> [(a, a, Double)]
calculate_trans_probability corpus = let transition_body = get_trans corpus
                                         poss_transitions = nub (get_trans corpus)
                                     in map (\x-> buildProbSLG_trans_helper2 transition_body x) poss_transitions

buildProbSLG_trans_helper2 :: Ord a => [(a,a)] -> (a, a) -> (a, a, Double)
buildProbSLG_trans_helper2 t_body (s1, s2) = let numerator = buildProbSLG_count_numerator t_body s1 s2
                                                 denominator = buildProbSLG_count_base t_body s1 
                                             in 
                                             (s1, s2, (divide numerator denominator))


buildProbSLG_count_numerator :: Ord a => [(a, a)] -> a -> a -> Int 
buildProbSLG_count_numerator all_trans target1 target2 = length (filter (\(x, y) -> ((x == target1 ) && (y == target2))) all_trans)


buildProbSLG_count_base :: Ord a => [(a, a)] -> a -> Int 
buildProbSLG_count_base all_trans target = length (filter (\(x, y) -> (x == target)) all_trans)
 

total_number_of_items :: Ord a => Corpus a -> Int 
total_number_of_items corpus = length corpus

extract_starts :: Ord a => Corpus a -> [a]
extract_starts corpus = (map (\x -> head x ) corpus) 

extract_end :: Ord a => Corpus a -> [a]
extract_end corpus = (map (\x -> last x ) corpus)

get_trans :: Ord a => Corpus a -> [(a,a)]
get_trans [] = []
get_trans (x: rest) = bigrams x ++ get_trans rest

trans_base :: Ord a => [((a, a), Int)] -> a -> Int 
trans_base all_trans target = sum (map (\((q',p'), z') -> z') (filter (\((x, y), num_occ) -> (y == target)) all_trans))
                            
allStates :: Ord a => Corpus a -> [a]
allStates [] = []
allStates ((x_head: []) : rest) = nub (x_head : allStates rest)  
allStates ((x_head: x_tail) : rest) = nub (x_head : (allStates (x_tail : rest)))  


-------------------------------------------------------------------------------
-- Problem 3: posProbSLG
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.
sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = []

posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG corpus = ProbSLG ((posProbSLG_starts corpus), (posProbSLG_finish corpus), (get_possProbSLG_trans corpus))

posProbSLG_starts :: Corpus TaggedWord ->  [(String, Double)] 
posProbSLG_starts corpus =  map (\(TaggedWord (sym1, sym2), chance) -> (sym2, chance)) (calculate_start_probability corpus) 

posProbSLG_finish :: Corpus TaggedWord ->  [(String, Double)] 
posProbSLG_finish corpus =  map (\(TaggedWord (sym1, sym2), chance) -> (sym2, chance)) (calculate_end_probability corpus) 

get_possProbSLG_trans :: Corpus TaggedWord -> [(String, String, Double)]
get_possProbSLG_trans corpus = let transition_body = all_trans_possProbSLG corpus
                                   poss_transitions = nub_all_trans_possProbSLG corpus 
                               in map (\x-> get_possProbSLG_trans_helper2 transition_body x) poss_transitions

get_possProbSLG_trans_helper2 :: [(String,String)] -> (String, String) -> (String, String, Double)
get_possProbSLG_trans_helper2 t_body (s1, s2) = let numerator = posProbSLG_count_numerator t_body s1 s2
                                                    denominator = posProbSLG_count_base t_body s1 
                                         in 
                                         (s1, s2, (divide numerator denominator))

posProbSLG_allStates_helper :: Corpus TaggedWord -> [(TaggedWord)]
posProbSLG_allStates_helper [] = []
posProbSLG_allStates_helper ((x_head: []) : rest) = nub (x_head : allStates rest)  
posProbSLG_allStates_helper ((x_head: x_tail) : rest) = nub (x_head : (allStates (x_tail : rest)))  

posProbSLG_allStates :: Corpus TaggedWord -> [String]
posProbSLG_allStates l =  map (\(TaggedWord (sym1, sym2)) -> sym2) (posProbSLG_allStates_helper l)


posProbSLG_count_base :: [(String, String)] -> String -> Int 
posProbSLG_count_base all_trans target = length (filter (\(x, y) -> (x == target)) all_trans)

posProbSLG_count_numerator :: [(String, String)] -> String -> String -> Int 
posProbSLG_count_numerator all_trans target1 target2 = length (filter (\(x, y) -> ((x == target1 ) && (y == target2))) all_trans)

get_trans_possProbSLG_helper :: Corpus TaggedWord -> [(TaggedWord,TaggedWord)]
get_trans_possProbSLG_helper [] = []
get_trans_possProbSLG_helper (x: rest) = bigrams x ++ get_trans rest

all_trans_possProbSLG :: Corpus TaggedWord -> [(String, String)]
all_trans_possProbSLG corpus =  map (\(TaggedWord (sym1, sym2), TaggedWord (sym3, sym4)) -> (sym2, sym4)) (get_trans_possProbSLG_helper corpus)

nub_all_trans_possProbSLG :: Corpus TaggedWord -> [(String, String)]
nub_all_trans_possProbSLG corpus =  nub $ map (\(TaggedWord (sym1, sym2), TaggedWord (sym3, sym4)) -> (sym2, sym4)) (get_trans_possProbSLG_helper corpus)


-------------------------------------------------------------------------------
-- Problem 3: tag
-------------------------------------------------------------------------------
-- idea 
--  1. split the string into a list get the length 
--  2. find all transitions in corpus with at least that length 
--      - extract the first n transitions 
--      - assign each word to the first part of tagged word 
--      - do calculation for final output 

tag :: Corpus TaggedWord -> String -> [(Sentence TaggedWord, Double)]
tag = undefined 

split_string:: String -> [(String)]
split_string str = words str

string_length:: [(String)] -> Int 
string_length str_list = length str_list

find_trans_with_min_len :: Corpus TaggedWord -> Int -> [[(TaggedWord)]]
find_trans_with_min_len [] str_len = []
find_trans_with_min_len (x: xs) str_len  =  case ((length x) < str_len) of True -> find_trans_with_min_len xs str_len
                                                                           False -> x: find_trans_with_min_len xs str_len
tagBest :: Corpus TaggedWord -> String -> Sentence TaggedWord
tagBest = undefined

-- tag takes corpus + string and determine the prob of generating that string 
-- 
