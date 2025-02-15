module Assignment04 where

import FSA
import SLG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fsa2 :: Automaton Int SegmentCV
fsa2 = ( 1, 
         [3],
         [(1, V, 1),
         (1, C, 2),
         (2, V, 2),
         (2, C, 3),
         (3, C, 3)])

fsa3 :: Automaton Int SegmentCV
fsa3 =  ( 1, 
         [4],
         [(1, V, 2),
         (1, C, 4),
         (2, V, 1),
         (2, C, 3),
         (3, C, 2),
         (3, V, 4),
         (4, C, 1),
         (4, V, 3)])

fsa4 :: Automaton Int SegmentCV
fsa4 = (1,
       [4],
       [(1, C, 1),
       (1, V, 1),
       (1, C, 2),
       (2, C, 3),
       (2, V, 3),
       (3, C, 4),
       (3, V, 4)])

fsa5 :: Automaton Int SegmentPKIU
fsa5 = (1,
       [1, 2, 3],
       [(1, I, 2),
       (1, P, 1),
       (1, K, 1),
       (1, U, 3),
       (1, WB, 1),
       (2, I, 2),
       (2, P, 2),
       (2, K, 2),
       (2, WB, 1),
       (3, U, 3),
       (3, P, 3),
       (3, K, 3),
       (3, WB, 1)])

fsa6 :: Automaton Int SegmentPKIU
fsa6 = (1,
       [1, 2, 3],
       [(1, P, 1),
        (1, P, 2),
        (1, I, 1),
        (1, K, 1),
        (2, P, 2),
        (2, K, 2),
        (2, I, 2),
        (2, U, 3),
        (3, P, 3),
        (3, K, 3),
        (3, I, 3),
        (3, U, 3)])

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA slg =
  let (starting_symbol, ending_symbol, transitions) = slg in
  let terminate = [StateForSymbol c | c <- ending_symbol] in
  let trans = [(ExtraState, c, StateForSymbol c) | c <- starting_symbol] ++
              [(StateForSymbol x, y, StateForSymbol y) | (x, y) <- transitions]
  in
  (ExtraState, terminate, trans)
