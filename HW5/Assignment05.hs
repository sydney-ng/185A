module Assignment05 where

import FSA


fwdProb :: (Ord st, Ord sy)
        => ProbFSA st sy
        -> [sy]
        -> st
        -> Double
fwdProb = undefined

--fwdProb :: (Ord st, Ord sy) => ProbFSA st sy -> [sy] -> st -> Double
--fwdProb pfsa (l_head : l_tail) ending_sym = let end_chance = finProb pfsa2 ending_sym
--                                                start_chance = initProb pfsa2 l_head
--                                                pfsa_trans = pfsa_transitions pfsa2 
--                                            in start_chance * end_chance * (fwdProb_pre_formatter pfsa_trans (l_head : l_tail) ending_sym)  

-- fwdProb_pre_formatter (pfsa_transitions pfsa2) ["the", "deer", "must"] (Init) 
--fwdProb_pre_formatter :: (Ord st, Ord sy) => [(st, sy, Double, st)] -> [sy] -> st -> Double
--fwdProb_pre_formatter pfsa_transitions (l_head : l_tail) ending_sym = let curr_word = l_head
--                                                                          next_trans = find_next_transitions pfsa_transitions curr_trans
--                                                                      in 


-- input: parse (pfsa_transitions pfsa2) (Init) "the" ["deer", "must"] "must" 
-- output: 
--parse :: (Ord st, Ord sy) => [(st, sy, Double, st)] -> st -> sy -> [sy] -> st -> [Double]
--parse all_transitions curr_state curr_word unread_words ending_sym = case curr_word of ending_sym -> 1 -- this is the ending symbol 
--                                                                                        _ -> let next_trans_list = find_next_transitions all_transitions curr_word -- next transitions 
--                                                                                             in 
--                                                                                             -- get number of curr and multiply with rest 
--                                                                                             extract_probability (curr_state) * (map (\child -> fwdProb_pre_formatter child unread_words ending_sym) next_trans_list) 

-- input: parse (pfsa_transitions pfsa2) (Init) "the" ["deer", "must"] "must" 
-- output: 
--parse :: [(st, sy, Double, st)] -> (st, sy, Double, st) -> [Char] -> [[Char]] -> st -> [(st, sy, Double, st)]
--parse all_transitions curr_state (st1, sym, chance, st2) unread_words ending_st = case (st1 == ending_st) of True  -> [] -- this is the ending symbol 
 --                                                                                                            False -> find_next_transitions all_transitions st2 (find_next_word_in_list unread_words) -- next transitions 

-- pass in entire pfsa, list of words left to parse, current symbol we are on 
--calc_transitional_prob :: ProbFSA st sy -> sy-> sy -> st
--calc_transitional_prob pfsa_trans curr_sym next_sym = 

-- purpose: calculate for each of the transitions that are possible 
-- input : find_next_transitions (pfsa_transitions pfsa2) "the" 
-- output: [(Init,"the",0.5,Sg 1),(Init,"the",0.5,Pl 1)]
--find_next_transitions :: (Ord st, Ord sy) => [(st, sy, Double, st)] -> sy -> [(st, sy, Double, st)] 
--find_next_transitions trans_list target_state trans_symbol =  (filter (\(st1, str, _ ,_) -> (str == transition) && (st1 == trans_symbol)) trans_list)

-- purpose: gets next word in the input list to parse 
-- input: find_next_word_in_list ["the", "cat"]
-- output: "the"
--find_next_word_in_list :: [sy] -> sy 
--find_next_word_in_list (x: xs) = x 

-- purpose: get probability from 1 pair 
--input: extract_probability (Init,"the",0.5,Sg 1)
--output: 0.5
extract_probability :: (Ord st, Ord sy) => (st, sy, Double, st) -> Double
extract_probability transition = case transition of (_, _ , chance ,_) -> chance 

multiply :: [Double] -> Double
multiply [] = 1 
multiply (x: xs) = x * (multiply xs)

pfsa_transitions :: (Ord st, Ord sy) => ProbFSA st sy -> [(st, sy, Double, st)]
pfsa_transitions (ProbFSA (starts, ends, trans)) = trans
                                                        

-- PseudoCode: 
-- for each start, check for all next transitions 
-- append the probability to running counter 
-- stop when it is a final state 
-- Then go to next 

------------------------------------------------------------------------------------------------------------------
-- UNION FSA 
------------------------------------------------------------------------------------------------------------------

unionFSAs :: (Ord sy) => 
              EpsAutomaton Int sy -> 
              EpsAutomaton Int sy -> 
              EpsAutomaton Int sy

unionFSAs automaton fsaB = 
    let startA = get_starting_symbols automaton
        finalA = get_ending_symbols automaton
        transitionA = get_transitions automaton
        newB_FSA = ensureUnused (allStatesEFSA automaton) fsaB in
        union_preparser newB_FSA startA finalA transitionA

union_preparser :: (Ord sy) => 
                   EpsAutomaton Int sy -> --newB_FSA
                   Int -> -- startA
                   [Int] -> -- finalA
                   [(Int, Maybe sy, Int)] -> -- transitionA 
                   EpsAutomaton Int sy -- result 
union_preparser new_fsaB startA finalA transitionA = 
    let startB = get_starting_symbols new_fsaB
        finalB = get_ending_symbols new_fsaB
        transitionB = get_transitions new_fsaB in
        union_format_trans new_fsaB startA finalA startB finalB transitionA transitionB

union_format_trans ::  (Ord sy) => 
                       EpsAutomaton Int sy -> --newB_FSA
                       Int -> -- startA
                       [Int] -> -- finalA
                       Int -> -- startB
                       [Int] -> -- finalB
                       [(Int, Maybe sy, Int)] -> -- transitionA 
                       [(Int, Maybe sy, Int)] -> -- transitionB
                       EpsAutomaton Int sy -- result 
union_format_trans new_fsaB startA finalA startB finalB transitionA transitionB = 
     let all_trans = format_transitions (format_transitions transitionA transitionB) [(startA, Nothing, startB)] 
         all_finals = finalA ++ finalB in
     unionFSAs_HELPER startA all_finals all_trans


get_starting_symbols :: (Ord sy) => EpsAutomaton Int sy -> Int
get_starting_symbols (EpsAutomaton (start, ends, delta)) = start

get_ending_symbols :: (Ord sy) => EpsAutomaton Int sy -> [Int]
get_ending_symbols (EpsAutomaton (start, ends, delta)) = ends

get_transitions :: (Ord sy) => EpsAutomaton Int sy -> [(Int, Maybe sy, Int)]
get_transitions (EpsAutomaton (start, ends, delta)) = delta 

format_transitions :: [(Int, Maybe sy, Int)] -> [(Int, Maybe sy, Int)] ->[(Int, Maybe sy, Int)]  
format_transitions t1 t2 = t1 ++ t2 

unionFSAs_HELPER :: (Ord sy) => 
                    Int -> -- startA
                    [Int] -> -- finalA / finalB
                    [(Int, Maybe sy, Int)] -> -- transitions
                    EpsAutomaton Int sy -- answer 
unionFSAs_HELPER startA finals transitions = (EpsAutomaton (startA, finals, transitions))


------------------------------------------------------------------------------------------------------------------
-- CONCAT FSA 
------------------------------------------------------------------------------------------------------------------
concatFSAs :: (Ord sy) => 
              EpsAutomaton Int sy -> 
              EpsAutomaton Int sy ->
              EpsAutomaton Int sy
concatFSAs automaton fsaB = 
     let startA = get_starting_symbols automaton
         finalA = get_ending_symbols automaton
         transitionA = get_transitions automaton
         newB_FSA = (ensureUnused (allStatesEFSA automaton) fsaB) in
         concatFSAs_preparser newB_FSA startA finalA transitionA

concatFSAs_preparser:: (Ord sy) => 
                       EpsAutomaton Int sy -> --newB_FSA
                       Int -> -- startA
                       [Int] -> -- finalA
                       [(Int, Maybe sy, Int)] -> -- transitionA 
                       EpsAutomaton Int sy -- result 
concatFSAs_preparser new_fsaB startA finalA transitionA = 
    let startB = get_starting_symbols new_fsaB
        finalB = get_ending_symbols new_fsaB
        transitionB = get_transitions new_fsaB in
        concat_format_trans new_fsaB startA finalA startB finalB transitionA transitionB

concat_trans :: [Int] -> Int -> [(Int, Maybe sy, Int)]
concat_trans trans start = case trans of (x:xs) -> ((x, Nothing, start):(concat_trans xs start))
                                         _ -> []
concat_format_trans ::  (Ord sy) => 
                       EpsAutomaton Int sy -> --newB_FSA
                       Int -> -- startA
                       [Int] -> -- finalA
                       Int -> -- startB
                       [Int] -> -- finalB
                       [(Int, Maybe sy, Int)] -> -- transitionA 
                       [(Int, Maybe sy, Int)] -> -- transitionB
                       EpsAutomaton Int sy -- result 
concat_format_trans new_fsaB startA finalA startB finalB transitionA transitionB = 
     let all_trans = format_transitions (format_transitions transitionB (concat_trans finalA startB)) transitionA
     in
     (EpsAutomaton (startA, finalB, all_trans))

------------------------------------------------------------------------------------------------------------------
-- STAR FSA 
------------------------------------------------------------------------------------------------------------------

starFSA :: (Ord sy) => 
           EpsAutomaton Int sy -> 
           EpsAutomaton Int sy
starFSA fsa =
    let new_fsa = ensureUnused [0] fsa in 
    starFSA_variable_finder new_fsa 

starFSA_variable_finder :: (Ord sy) => 
                           EpsAutomaton Int sy -> 
                           EpsAutomaton Int sy 
starFSA_variable_finder new_fsa =     
    let starts = get_starting_symbols new_fsa
        finals = get_ending_symbols new_fsa
        transitions = get_transitions new_fsa in
        starFSA_combiner starts finals transitions 

starFSA_combiner ::  (Ord sy) => 
                       Int -> -- start
                       [Int] -> -- finals
                       [(Int, Maybe sy, Int)] -> -- transitions 
                       EpsAutomaton Int sy -- result 
starFSA_combiner starts finals transitions = 
     let e_transition_to_beg = concat_trans finals starts
         start_trans = [(0, Nothing, starts)]
     in
     (EpsAutomaton (0, ([0]++finals), (format_transitions (format_transitions transitions e_transition_to_beg) start_trans) ))
------------------------------------------------------------------------------------------------------------------
-- reToFSA
------------------------------------------------------------------------------------------------------------------
reToFSA :: (Ord sy) => 
           RegEx sy -> 
           EpsAutomaton Int sy

reToFSA input = case input of (Lit x) -> (EpsAutomaton (0, [1], [(0, Just x, 1)]))
                              (Alt r1 r2) -> unionFSAs (reToFSA r1) (reToFSA r2) 
                              (Concat r1 r2) -> concatFSAs (reToFSA r1) (reToFSA r2) 
                              (Star r)-> starFSA (reToFSA r) 
                              Zero -> (EpsAutomaton (0, [], []))
                              One -> (EpsAutomaton (0, [0], []))
