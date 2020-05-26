module Assignment06 where

import CFG

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly string = case (nt_checker string) of True -> Nothing
                                                   False -> Just (map (\(T str) -> str) string)

nt_checker :: [Symbol nt t] -> Bool
nt_checker list = case list of [] -> False 
                               ((NT s):xs) -> True 
                               ((T s):xs) -> nt_checker xs

-------------------------------------------------------------------------------

leaves :: Tree nt t -> [t]
leaves (Leaf nt t) = t : []
leaves (NonLeaf nt t1 t2) = concat [(leaves t1), (leaves t2)]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList tree = case tree of (Leaf nt t) -> [TerminalRule nt t]
                                   (NonLeaf nt t1 t2) -> (NonterminalRule nt (root t1, root t2)):(treeToRuleList t1)++(treeToRuleList t2)

treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = case tree of (Leaf nt t) -> [[NT nt], [T t]]
                                     (NonLeaf nt t1 t2) -> treeToDerivation_HELPER tree

treeToDerivation_HELPER :: Tree nt t -> [[Symbol nt t]]
treeToDerivation_HELPER (NonLeaf nt t1 t2) = 
    let derivedLeft = treeToDerivation t1 in
    let attachedRight = do_rhs derivedLeft t2 in
    let derivedRight = treeToDerivation t2 in
    let attachedLeft = do_lhs (last derivedLeft) derivedRight in
    ([NT nt]:(attachedRight++(tail attachedLeft)))

do_rhs :: [[Symbol nt t]] -> Tree nt t -> [[Symbol nt t]]
do_rhs phrase tree = case phrase of [] -> []
                                    (x:xs) -> (x++[NT (root tree)]):(do_rhs xs tree)

do_lhs :: [Symbol nt t] -> [[Symbol nt t]] -> [[Symbol nt t]]
do_lhs phrase tree = case tree of [] -> []
                                  (x:xs) -> (phrase++x):(do_lhs phrase xs)    

-------------------------------------------------------------------------------

ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree ruleList = case ruleList of [] -> Nothing
                                           _ -> ruleListToTree_HELPER ruleList

ruleListToTree_HELPER :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree_HELPER ruleList = ruleListToTree_HELPER_2 (createTree ruleList (findRootNode ruleList))

ruleListToTree_HELPER_2 ::  Maybe ((Tree nt t), [RewriteRule nt t]) ->  Maybe (Tree nt t)                               
ruleListToTree_HELPER_2 (Just (tree, [])) = Just tree
ruleListToTree_HELPER_2 _ = Nothing


createTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> nt -> Maybe ((Tree nt t), [RewriteRule nt t])
createTree rules symbol = case rules of [] -> Nothing
                                        ((TerminalRule nt t):rs) -> if (symbol == nt) then Just ((Leaf nt t), rs)
                                                           else Nothing
                                        ((NonterminalRule nt (t1, t2)):rs) -> if (symbol == nt) then let createLeft = createTree rs t1 in
                                                                               case createLeft of
                                                                                   Nothing -> Nothing
                                                                                   Just (leftTree, rs) ->
                                                                                       let createRight = createTree rs t2 in
                                                                                       case createRight of Nothing -> Nothing
                                                                                                           Just (rightTree, rs) -> Just (NonLeaf nt leftTree rightTree, rs)
                                                                              else Nothing

findRootNode :: [RewriteRule nt t] -> nt
findRootNode (x:xs) = case x of (NonterminalRule nt _) -> nt
                                (TerminalRule nt _) -> nt                                           

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

splitAtLeftmost :: (Eq nt, Eq t)
                => [Symbol nt t]
                -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost phrase = case phrase of [] -> Nothing
                                        ((NT s):rs) -> Just ([], s, rs)
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
rewriteLeftmost rules input = case (splitAtLeftmost input) of Nothing -> input : []
                                                              _  ->rewriteLeftmost_HELPER rules (splitAtLeftmost input)
       
rewriteLeftmost_HELPER :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe ([Symbol nt t], nt, [Symbol nt t]) -> [[Symbol nt t]]
rewriteLeftmost_HELPER rules (Just (nt_before, nt, nt_after)) = rule_replacement nt_before (matching_rules nt rules) nt_after


matching_rules :: (Eq nt) => nt -> [RewriteRule nt t] -> [RewriteRule nt t]
matching_rules target rules = case rules of [] -> []
                                            ((NonterminalRule nt (a, b)):xs) -> handle_nt target rules 
                                            ((TerminalRule nt t):xs) -> handle_t target rules 

handle_nt :: (Eq nt) => nt -> [RewriteRule nt t] -> [RewriteRule nt t]
handle_nt target ((NonterminalRule nt (a, b)):xs) = case (target == nt) of True -> (NonterminalRule nt (a, b)):(matching_rules target xs)
                                                                           False -> matching_rules target xs


handle_t :: (Eq nt) => nt -> [RewriteRule nt t] -> [RewriteRule nt t]
handle_t target ((TerminalRule nt t):xs) = case (target == nt) of True -> (TerminalRule nt t):(matching_rules target xs)
                                                                  False -> matching_rules target xs

rule_replacement :: [Symbol nt t] -> [RewriteRule nt t] -> [Symbol nt t] -> [[Symbol nt t]]
rule_replacement nt_before rules nt_after= case rules of [] -> []
                                                         ((NonterminalRule nt (t1, t2)):xs) -> (nt_before++[NT t1, NT t2]++nt_after) : (rule_replacement nt_before xs nt_after)
                                                         ((TerminalRule nt t):xs) -> (nt_before++[T t]++nt_after):(rule_replacement nt_before xs nt_after)

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
