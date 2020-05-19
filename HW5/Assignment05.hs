module Assignment05 where

import FSA

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
