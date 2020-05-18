module Assignment05 where

import FSA

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

--        unionFSAs_HELPER startA finalA startB finalB transitionA transitionB

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







