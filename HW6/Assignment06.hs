module Assignment06 where

import CFG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly input = case (nt_checker input) of True -> Nothing
                                                 False -> Just (map (\(T x) -> x) input)

nt_checker :: [Symbol nt t] -> Bool
nt_checker list = case list of [] -> False 
                               ((NT _):_) -> True 
                               ((T _):xs) -> nt_checker xs

-------------------------------------------------------------------------------

leaves :: Tree nt t -> [t]
leaves (Leaf nt t) = t : []
leaves (NonLeaf nt t1 t2) = concat [(leaves t1), (leaves t2)]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList tree = case tree of (Leaf nt t) -> (TerminalRule nt t) : []
                                   (NonLeaf nt t1 t2) -> treeToRuleList_HELPER tree

treeToRuleList_HELPER :: Tree nt t -> [RewriteRule nt t]
treeToRuleList_HELPER (NonLeaf nt t1 t2) = let first_part = (NonterminalRule nt (root t1, root t2)):(treeToRuleList t1) -- does concat 
                                               second_part = treeToRuleList t2 -- recursive step 
                                           in first_part ++ second_part -- put steps together 

-------------------------------------------------------------------------------

ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree input = case input of [] -> Nothing
                                     _ -> ruleListToTree_HELPER input

ruleListToTree_HELPER :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree_HELPER input = ruleListToTree_HELPER_2 (formatTree input (findRootNode input))

ruleListToTree_HELPER_2 ::  Maybe ((Tree nt t), [RewriteRule nt t]) ->  Maybe (Tree nt t)                               
ruleListToTree_HELPER_2 (Just (tree, [])) = Just tree
ruleListToTree_HELPER_2 _ = Nothing

formatTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> nt -> Maybe ((Tree nt t), [RewriteRule nt t])
formatTree rules symbol = case rules of [] -> Nothing
                                        ((TerminalRule nt t): rest) -> parse_terminals rules symbol
                                        ((NonterminalRule nt (t1, t2)): rest) -> parse_nonterminals rules symbol 
                                                                              

parse_terminals :: (Eq nt, Eq t) => [RewriteRule nt t] -> nt -> Maybe ((Tree nt t), [RewriteRule nt t])
parse_terminals ((TerminalRule nt t) : rest) symbol = case (symbol == nt) of True -> Just ((Leaf nt t), rest)
                                                                             False -> Nothing

parse_nonterminals :: (Eq nt, Eq t) => [RewriteRule nt t] -> nt -> Maybe ((Tree nt t), [RewriteRule nt t])
parse_nonterminals ((NonterminalRule nt (t1, t2)): rest) symbol = case (symbol == nt) of True -> parse_nonterminals_HELPER (formatTree rest t1) nt rest t2 
                                                                                         False -> Nothing 

parse_nonterminals_HELPER :: (Eq nt, Eq t) => 
                             Maybe ((Tree nt t), [RewriteRule nt t]) --formatTree_output
                             -> nt -- nt 
                             -> [RewriteRule nt t] -- rest 
                             -> nt -- t2 
                             -> Maybe ((Tree nt t), [RewriteRule nt t])

parse_nonterminals_HELPER formatLtree_output nt rest t2 = case formatLtree_output of Just (left_x, left_xs) -> parse_nonterminals_HELPER_2 formatLtree_output (formatTree left_xs t2) nt rest left_x
                                                                                     Nothing -> Nothing 

parse_nonterminals_HELPER_2 :: (Eq nt, Eq t) => 
                             Maybe ((Tree nt t), [RewriteRule nt t]) --formatLtree_output
                             -> Maybe ((Tree nt t), [RewriteRule nt t]) --formatRtree_output
                             -> nt -- nt 
                             ->  [RewriteRule nt t] -- rest 
                             -> (Tree nt t) -- left_x
                             -> Maybe ((Tree nt t), [RewriteRule nt t])
parse_nonterminals_HELPER_2 formatLtree_output formatRtree_output nt rest left_x = case formatRtree_output of Just (right_x, right_xs) -> Just (NonLeaf nt left_x right_x, right_xs)  
                                                                                                              Nothing -> Nothing 
findRootNode :: [RewriteRule nt t] -> nt
findRootNode (x:xs) = case x of (NonterminalRule nt _) -> nt
                                (TerminalRule nt _) -> nt                                           

-------------------------------------------------------------------------------

treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = case tree of (Leaf nt t) -> [[NT nt], [T t]]
                                     (NonLeaf nt t1 t2) -> treeToDerivation_HELPER tree tree 

treeToDerivation_HELPER :: Tree nt t -> Tree nt t  -> [[Symbol nt t]]
treeToDerivation_HELPER tree (NonLeaf nt t1 t2) =  let new_left = treeToDerivation t1 
                                                       new_right = treeToDerivation t2
                                                   in treeToDerivation_HELPER_2 new_left new_right tree 

treeToDerivation_HELPER_2 :: [[Symbol nt t]] -> [[Symbol nt t]] -> Tree nt t -> [[Symbol nt t]] 
treeToDerivation_HELPER_2 new_left new_right (NonLeaf nt t1 t2) = 
    let processed_t2 = map (\x -> x ++ [NT (root t2)]) new_left 
        processed_new_left = map (\x -> (last new_left) ++ x) new_right
    in ([NT nt]:(processed_t2++(tail processed_new_left)))  

-------------------------------------------------------------------------------

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost phrase = case phrase of [] -> Nothing
                                        ((NT x):xs) -> Just ([], x, xs)
                                        _ -> splitAtLeftmost_HELPER phrase

splitAtLeftmost_HELPER :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost_HELPER ((T x'):xs') = case splitAtLeftmost xs' of Nothing -> Nothing
                                                                  Just (x, nt, xs) -> Just ((T x'):x, nt, xs)

-------------------------------------------------------------------------------
rewriteLeftmost :: (Eq nt, Eq t)
                => [RewriteRule nt t]
                -> [Symbol nt t]
                -> [[Symbol nt t]]
rewriteLeftmost rules input = undefined
-------------------------------------------------------------------------------
derivableFrom :: (Eq nt, Eq t)
              => [Symbol nt t]
              -> [RewriteRule nt t]
              -> Int
              -> [[t]]
derivableFrom = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

derivable :: (Eq nt, Eq t) => CFG nt t -> Int -> [[t]]
derivable (start , rules) n = derivableFrom [NT start] rules n
