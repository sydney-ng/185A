module FSA where

import Data.List (nub)

-------------------------------------------------------------------------------
-- Types for FSAs and regular expressions.
-------------------------------------------------------------------------------

-- Ordinary FSAs.
newtype Automaton st sy = Automaton ( st              -- Initial state.
                                    , [st]            -- Final states.
                                    , [(st, sy, st)]  -- Transitions.
                                    )
                        deriving Show

-- FSAs with e-transition.
newtype EpsAutomaton st sy = EpsAutomaton ( st
                                          , [st]
                                          , [(st, Maybe sy, st)]
                                          )
                           deriving Show

-- Probabilistic FSAs.
newtype ProbFSA st sy = ProbFSA ( [(st, Double)]
                                , [(st, Double)]
                                , [(st, sy, Double, st)]
                                )
                      deriving Show

-- Regular expressions.
data RegEx sy = Lit sy
              | Alt (RegEx sy) (RegEx sy)
              | Concat (RegEx sy) (RegEx sy)
              | Star (RegEx sy)
              | Zero
              | One
              deriving Show

-------------------------------------------------------------------------------
-- Toy examples.
-------------------------------------------------------------------------------

data WordSegState = Edge
                  | Internal
                  deriving (Show, Eq, Ord)

-- Corresponds to (4) on the PFSA handout.
pfsa1 :: ProbFSA WordSegState Char
pfsa1 = ProbFSA
    ( [ (Edge, 1.0) ]
    , [ (Edge, 0.5) ]
    , [ (Edge, 'a', 0.015, Edge) ,     (Internal, 'a', 0.042, Edge)
      , (Edge, 'i', 0.015, Edge) ,     (Internal, 'e', 0.056, Edge)
      ,                                (Internal, 'i', 0.014, Edge)
      ,                                (Internal, 'n', 0.098, Edge)
      ,                                (Internal, 't', 0.084, Edge)
      ,                                (Internal, 's', 0.154, Edge)
      , (Edge, 'a', 0.103, Internal) , (Internal, 'a', 0.085, Internal)
      , (Edge, 'e', 0.029, Internal) , (Internal, 'e', 0.149, Internal)
      , (Edge, 'i', 0.088, Internal) , (Internal, 'i', 0.149, Internal)
      , (Edge, 'n', 0.029, Internal) , (Internal, 'n', 0.085, Internal)
      , (Edge, 't', 0.103, Internal) , (Internal, 't', 0.021, Internal)
      , (Edge, 's', 0.118, Internal) , (Internal, 's', 0.064, Internal)
      ]
     )

data SgPlState = Init
               | Sg Int
               | Pl Int
               | M Int
               deriving (Show, Eq, Ord)

-- Corresponds to ProbFSA from whiteboard notes on 5 May.
pfsa2 :: ProbFSA SgPlState String
pfsa2 = ProbFSA
    ( [ (Init, 1.0) ]
    , [ (Sg 3, 1.0), (M 2, 1.0), (Pl 3, 1.0) ]
    , [ (Init, "the", 0.5, Sg 1)
      , (Init, "the", 0.5, Pl 1)
      -- From Sg1
      , (Sg 1, "cat", 0.5, Sg 2)
      , (Sg 1, "deer", 0.25, Sg 2)
      , (Sg 1, "dog", 0.25, Sg 2)
      -- From Sg2
      , (Sg 2, "jumps", 0.2, Sg 3)
      , (Sg 2, "sleeps", 0.3, Sg 3)
      , (Sg 2, "must", 0.5, M 1)
      -- From Pl1
      , (Pl 1, "cats", 0.25, Pl 2)
      , (Pl 1, "deer", 0.5, Pl 2)
      , (Pl 1, "dogs", 0.25, Pl 2)
      -- From Pl2
      , (Pl 2, "jump", 0.5, Pl 3)
      , (Pl 2, "sleep", 0.2, Pl 3)
      , (Pl 2, "must", 0.3, M 1)
      -- From M1
      , (M 1, "jump", 0.6, M 2)
      , (M 1, "sleep", 0.4, M 2)
      ]
    )

-- Corresponds to (3) on the eFSA handout.
efsa1 :: EpsAutomaton Int Char
efsa1 = EpsAutomaton
    ( 0
    , [2]
    , [ (0, Just 'a', 0)
      , (0, Nothing, 1)
      , (1, Just 'b', 1)
      , (1, Nothing, 2)
      , (2, Just 'c', 2)
      ]
     )

re1 :: RegEx Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegEx Char
re2 = Star re1

re3 :: RegEx Int
re3 = Star (Concat Zero (Lit 3))

re4 :: RegEx Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

-------------------------------------------------------------------------------
-- Functions for working with PFSAs.
-------------------------------------------------------------------------------

-- Returns all the states mentioned anywhere in a ProbFSA.
allStatesPFSA :: (Ord st, Ord sy) => ProbFSA st sy -> [st]
allStatesPFSA (ProbFSA (starts, ends, trans)) =
    nub $ concat [ map (\(q, _) -> q) starts
                 , map (\(q, _) -> q) ends
                 , map (\(q, _, _, _) -> q) trans
                 , map (\(_, _, _, q) -> q) trans
                 ]

-- Finds starting probability of a state.
initProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Double
initProb (ProbFSA (starts, ends, trans)) q =
    sum $ map (\(_ , p) -> p) $ filter (\(q', _) -> q' == q) starts

-- Finds ending probability of a state.
finProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Double
finProb (ProbFSA (starts, ends, trans)) q =
    sum $ map (\(_, p) -> p) $ filter (\(q', _) -> q' == q) ends

-- Finds probability for particular state-symbol-state transition.
trProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> sy -> st -> Double
trProb (ProbFSA (starts, ends, trans)) q1 x q2 =
    sum $ map (\(_, _, p, _) -> p)
        $ filter (\(q, y, _, q') -> (q, y, q') == (q1, x, q2)) trans

-------------------------------------------------------------------------------
-- Functions for working with ordinary FSAs.
-------------------------------------------------------------------------------

targets :: (Ord s, Ord a) => [(s, a, s)] -> s -> a -> [s]
targets [] q x = []
targets (head:tail) q x =
    if   q1 == q && y == x
    then q2 : (targets tail q x) -- Save and recurse.
    else targets tail q x -- Recurse, but don't save.
  where
    (q1, y, q2) = head -- Unpack the first tuple.

hat :: (Ord s, Ord a) => [(s, a, s)] -> s -> [a] -> [s]
hat delta q [] = [q]
hat delta q (x:u) =
    nub $ concat $ -- Big union and remove duplicates.
        map (\q' -> hat delta q' u) -- Recurse on target states.
            (targets delta q x) -- Get possible target states.

recognize :: (Ord s, Ord a) => Automaton s a -> [a] -> Bool
recognize (Automaton (start, ends, delta)) u =
    or $ -- Big disjunction: something in list must be True
        map (\q -> elem q ends) -- Check if q is an end state.
            (hat delta start u) -- Get all states that would emit u.

-------------------------------------------------------------------------------
-- Functions for working with eFSAs.
-------------------------------------------------------------------------------

-- Corresponds to (4) on the eFSA handout.
-- NOTE: 'until p f x' applies f to x repeatedly until p x is true.
epsilonClosure :: (Ord st, Ord sy) => [(st, Maybe sy, st)] -> st -> [st]
epsilonClosure delta q =
    until (\qs -> update qs == qs) update [q]
  where
    update qs = qs ++ [q2 | q1 <- qs
                          , q2 <- targets delta q1 Nothing
                          , not (elem q2 qs)] -- Avoids infinite loop.

-- Helper function getting all the states of an eFFSA.
allStatesEFSA :: (Ord st) => EpsAutomaton st sy -> [st]
allStatesEFSA (EpsAutomaton (start, ends, delta)) =
    nub (start : ends ++ concat [[q1,q2] | (q1,x,q2) <- delta])

-- Helper function getting all the symbols of an eFSA.
allSymbolsEFSA :: (Ord sy) => EpsAutomaton st sy -> [Maybe sy]
allSymbolsEFSA (EpsAutomaton (start, ends, delta)) =
    nub [x | (q1, x, q2) <- delta]

-- Corresponds to (5) on the eFSA handout.
removeEpsilons :: (Ord st, Ord sy) => EpsAutomaton st sy -> Automaton st sy
removeEpsilons efsa =
    Automaton ( start
                -- New end states.
              , filter canReachEndState states
                -- NOTE: Apply newTransitions to all the state-symbol
                -- combinations, which will either return [] or a list of
                -- transitions; concat then combines these all into a single
                -- list.
              , concat [newTransitions q x | q <- states
                                           , x <- symbols]
              )
    where
        EpsAutomaton (start, ends, delta) = efsa
        states = allStatesEFSA efsa
        symbols = allSymbolsEFSA efsa
        -- Corresponds to (5b) on eFSA handout.
        newTransitions q1 x = 
            case x of
                Nothing -> []
                Just s -> [(q1, s, q3) | q2 <- epsilonClosure delta q1
                                       , q3 <- targets delta q2 (Just s)]
        canReachEndState q =
            or -- Big disjunction.
                $ map (\q' -> elem q' ends)     -- Is q' an end state?
                      (epsilonClosure delta q)  -- Epsilon closure of q.

-------------------------------------------------------------------------------
-- Helper functions for combining eFSAs.
-------------------------------------------------------------------------------

-- Produces a new version of an eFSA with the guarantee that certain integers
-- are not used as state labels.
ensureUnused :: [Int] -> EpsAutomaton Int sy -> EpsAutomaton Int sy
ensureUnused reserved fsa =
    if   reserved == []
    then fsa
    else -- nonneg maps integers to nonnegative integers, preserving all
         -- distinctions.
         let nonneg x = if x < 0 then 2 * (-x) - 1 else 2 * x in
         -- Create a version of fsa where all state numbers are nonnegative.
         let fsanonneg = mapStates nonneg fsa in
         -- Add enough to all state numbers to make sure they do not clash with
         -- the reserved list.
         mapStates (\x -> x + 1 + maximum reserved) fsanonneg

-- Maps a function onto all the state labels.
mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f (EpsAutomaton (start, ends, delta)) =
    EpsAutomaton ( f start
                 , map f ends
                   -- Apply f to source + target states.
                 , map (\(q1, x, q2) -> (f q1, x, f q2)) delta
                 )
