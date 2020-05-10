module Helpers where

-- For efficient list parsing.
import Data.List (group, sort, sortBy, groupBy)
import Data.Function (on)
import Data.Ord (comparing)

-- For defining easy-to-work-with examples.
import ProbSLG

-------------------------------------------------------------------------------
-- Helper functions.
-------------------------------------------------------------------------------

-- Converts a list into a list of its bigrams.
bigrams :: [a] -> [(a, a)]
bigrams xs = zip xs (tail xs)

-- Computes the frequencies of elements in a list.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = map (\y -> (head y, length y)) . group . sort $ xs

-- Divides integers without rounding.
divide :: (Integral a, Integral b, Fractional c) => a -> b -> c
divide x y = (fromIntegral x) / (fromIntegral y)

-- Gets just the tag out of a tagged-word pair.
getTag :: TaggedWord -> String
getTag (TaggedWord (_, tag)) = tag

-- Gets just the word out of a tagged-word pair.
getWord :: TaggedWord -> String
getWord (TaggedWord (word, _)) = word

-- Update the tag of a tagged-word pair.
changeTag :: TaggedWord -> String -> TaggedWord
changeTag (TaggedWord (word, _)) tag = TaggedWord (word, tag)

-------------------------------------------------------------------------------
-- Simple frequency grammar.
--
-- Usage with "lookup":
-- case lookup "the" (mostFrequent corpus3) of
--     Just tag -> tag
--     Nothing  -> ""
-------------------------------------------------------------------------------

mostFrequent :: Corpus TaggedWord -> [(String, String)]
mostFrequent corpus =
    map go chunked
  where
    -- Gather all words and convert to plain tuples.
    allWords = map (\(TaggedWord p) -> p) $ concat corpus
    -- Group words and tags.
    chunked = groupBy ((==) `on` fst) $ sortBy (comparing fst) allWords
    -- Calculate most frequent tag.
    go xs =
        ( fst $ head xs
        , fst $ foldl1 (\x y -> if snd x >= snd y then x else y)
              $ frequencies
              $ map snd xs
        )

-------------------------------------------------------------------------------
-- Easy-to-work-with corpora.
-------------------------------------------------------------------------------

corpus1 :: Corpus Bool
corpus1 =
    [ [True, True, True]
    , [True, False]
    , [True, False]
    , [False, True]
    , [False, False, True]
    ]

corpus2 :: Corpus String
corpus2 =
    [ ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "N"]
    , ["D", "Adj", "N"]
    , ["D", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adj", "N"]
    , ["D", "Adv", "Adv", "Adj", "N"]
    ]

corpus3 :: Corpus TaggedWord
corpus3 =
    [ [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fat", "Adj")
      ,TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("fat", "Adj")
      ,TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv")
      ,TaggedWord ("fat", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv")
      ,TaggedWord ("fat", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv")
      ,TaggedWord ("fat", "Adj"), TaggedWord ("cat", "N")]
    , [TaggedWord ("the", "D"), TaggedWord ("very", "Adv")
      ,TaggedWord ("very", "Adv"), TaggedWord ("fat", "Adj")
      ,TaggedWord ("cat", "N")]
    ]

corpus4 :: Corpus TaggedWord
corpus4 =
    [ [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    ]

corpus5 :: Corpus TaggedWord
corpus5 =
    [ [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("the", "A"), TaggedWord ("the", "A")]
    , [TaggedWord ("cat", "A"), TaggedWord ("cat", "A")]
    , [TaggedWord ("cat", "A"), TaggedWord ("cat", "A")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("the", "B"), TaggedWord ("the", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    , [TaggedWord ("cat", "B"), TaggedWord ("cat", "B")]
    ]

-------------------------------------------------------------------------------
-- Easy-to-work-with PSLG.
-------------------------------------------------------------------------------

g1 :: ProbSLG String
g1 = ProbSLG
    ( [ ("the", 1.0) ]
    , [ ("cat", 0.5) ]
    , [ ("the", "cat", 0.5), ("the", "very", 0.2), ("the", "fat", 0.3)
      , ("very", "very", 0.3), ("very", "fat", 0.7), ("fat", "cat", 0.5) ]
    )
