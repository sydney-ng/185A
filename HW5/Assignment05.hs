module Assignment05 where

import FSA

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--fwdProb :: (Ord st, Ord sy) => ProbFSA st sy -> [sy] -> st -> Double
--fwdProb pfsa (l_head : l_tail) ending_sym = let end_chance = finProb pfsa2 ending_sym
--                                                start_chance = initProb pfsa2 l_head
--                                            in start_chance * end_chance * (fwdProb_pre_formatter pfsa (l_head : l_tail) ending_sym)  

--fwdProb_pre_formatter :: (Ord st, Ord sy) => ProbFSA st sy -> [sy] -> st -> Double
--fwdProb_pre_formatter (starts : ends : trans) (l_head: l_next: l_tail) ending_sym = let curr_trans = l_head 
--                                                                                    curr_state = l_next 
--                                                                                    in trProb pfsa 



-- pass in entire pfsa, list of words left to parse, current symbol we are on 
--calc_transitional_prob :: ProbFSA st sy -> sy-> sy -> st
--calc_transitional_prob pfsa_trans curr_sym next_sym = 


-- input : 
-- calculate for each of the transitions that are possible 
find_next_transitions :: (Ord st, Ord sy) => [(st, sy, Double, st)] -> sy -> st -> [(st, sy, Double, st)] 
find_next_transitions trans_list transition next_st =  (filter (\(_, str, _ ,_) -> (str == transition)) trans_list)
-- find_next_transitions trans_list transition next_st = find_next_transition_with_correct_endstate (filter (\(_, str, _ ,_) -> (str == transition)) trans_list) next_st


-- input : 
find_next_transition_with_correct_endstate :: (Ord st, Ord sy) => [(st, sy, Double, st)] -> st -> [(st, sy, Double, st)]
find_next_transition_with_correct_endstate trans_list next_st = filter (\(st1, _ , _ ,_) -> (st1 == next_st)) trans_list

find_next_word_in_list :: [sy] -> sy 
find_next_word_in_list (x: xs) = x 

extract_probability :: (Ord st, Ord sy) => (st, sy, Double, st) -> Double
extract_probability transition = case transition of (_, _ , chance ,_) -> chance 

pfsa_transitions :: (Ord st, Ord sy) => ProbFSA st sy -> [(st, sy, Double, st)]
pfsa_transitions (ProbFSA (starts, ends, trans)) = trans
                                                        
-- PseudoCode: 
-- for each start, check for all next transitions 
-- append the probability to running counter 
-- stop when it is a final state 
-- Then go to next 
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

unionFSAs :: (Ord sy)
          => EpsAutomaton Int sy
          -> EpsAutomaton Int sy
          -> EpsAutomaton Int sy
unionFSAs = undefined

concatFSAs :: (Ord sy)
            => EpsAutomaton Int sy
            -> EpsAutomaton Int sy
            -> EpsAutomaton Int sy
concatFSAs = undefined

starFSA :: (Ord sy)
        => EpsAutomaton Int sy
        -> EpsAutomaton Int sy
starFSA = undefined

reToFSA :: (Ord sy)
        => RegEx sy
        -> EpsAutomaton Int sy
reToFSA (Lit a) = undefined
reToFSA (Alt r1 r2) = undefined
reToFSA (Concat r1 r2) = undefined
reToFSA (Star r) = undefined
reToFSA Zero = undefined
reToFSA One = undefined
