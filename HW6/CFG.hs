module CFG where

-------------------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------------------

type CFG nt t = ( nt                  -- Starting nonterminal.
                , [RewriteRule nt t]  -- List of rules.
                )

-- Rules that enforce Chomsky Normal Form.
data RewriteRule nt t = NonterminalRule nt (nt, nt)
                      | TerminalRule nt t
                      deriving (Show, Eq)

-- Type that allows terminal and nonterminal symbols to be different types.
-- Examples: NT 3 :: Symbol Int Char
--           T 'a' :: Symbol Int Char
data Symbol nt t = NT nt
                 | T t
                 deriving (Show, Eq)

-- Type for tree structures.
-- Example: NonLeaf "S" (NonLeaf "NP" (Leaf "D" "the") (Leaf "N" "cat"))
--              (NonLeaf "VP" (Leaf "V" "saw") (Leaf "NP" "Mary"))
--          Above, the type of the expression is Tree String String.
data Tree nt t = NonLeaf nt (Tree nt t) (Tree nt t)
               | Leaf nt t
               deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Sample CFG.
-------------------------------------------------------------------------------

cfg1 :: CFG String String
cfg1 = ( "S"
       , [ NonterminalRule "S" ("NP", "VP")
         , NonterminalRule "NP" ("D", "N")
         , NonterminalRule "VP" ("V", "NP")
         , TerminalRule "NP" "John"
         , TerminalRule "NP" "Mary"
         , TerminalRule "D" "the"
         , TerminalRule "D" "a"
         , TerminalRule "N" "cat"
         , TerminalRule "N" "dog"
         , TerminalRule "V" "saw"
         , TerminalRule "V" "likes"
         ]
       )

-------------------------------------------------------------------------------

-- This function splits at the leftmost occurrence of the given nonterminal,
-- which is not necessarily the same as splitting at the leftmost nonterminal.
splitAtNT :: (Eq nt, Eq t)
          => nt
          -> [Symbol nt t]
          -> Maybe ([Symbol nt t], [Symbol nt t])
splitAtNT _ [] = Nothing
splitAtNT key (x:xs) =
    if   x == NT key
    then Just ([], xs)
    else case splitAtNT key xs of
            Nothing       -> Nothing
            Just (ys, zs) -> Just (x:ys, zs)

-- Example usage (pseudo-code):
-- rewrite (A -> beta) (alpha A gamma) ==> alpha beta gamma
-- rewrite (NP -> D N) [S, Mary, NP, the] ==> Just [S, Mary, D, N, the]
-- rewrite (NP -> D N) [S, Mary, VP, the] ==> Nothing
rewrite :: (Eq nt, Eq t)
        => RewriteRule nt t
        -> [Symbol nt t]
        -> Maybe [Symbol nt t]
rewrite r xs =
    case splitAtNT a xs of
        Nothing             -> Nothing
        Just (alpha, gamma) -> Just (concat [alpha, beta, gamma])
  where
    (a, beta) = case r of NonterminalRule x (y, z) -> (x, [NT y, NT z])
                          TerminalRule x y         -> (x, [T y])

-------------------------------------------------------------------------------
-- Helper functions.
-------------------------------------------------------------------------------

-- Returns root node of a tree.
root :: Tree nt t -> nt
root (Leaf x _)      = x
root (NonLeaf x _ _) = x

-- Returns the left-hand side of a rule.
lhs :: RewriteRule nt t -> nt
lhs (NonterminalRule x _) = x
lhs (TerminalRule x _)    = x

-- Returns the right-hand side of a rule.
rhs :: RewriteRule nt t -> [Symbol nt t]
rhs (NonterminalRule _ (y, z)) = [NT y, NT z]
rhs (TerminalRule _ y)         = [T y]
