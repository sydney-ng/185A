module ProbSLG ( ProbSLG(..)
               , Sentence
               , Corpus
               , TaggedWord(..)
               ) where

-- For pretty-printing PSLGs.
import Data.List (transpose)
import PrettyPrint.Boxes

-------------------------------------------------------------------------------
-- Probabilistic SLGs.
-------------------------------------------------------------------------------

newtype ProbSLG sy = ProbSLG ( [(sy, Double)]      -- Starting symbols.
                             , [(sy, Double)]      -- Final symbols.
                             , [(sy, sy, Double)]  -- Transitions.
                             )

-- Pretty-printing function.
instance Show a => Show (ProbSLG a) where
    show (ProbSLG (starts, finals, trans)) =
           "===== Starting symbols: ===\n"
        ++ makeTable (map (\(x, y) -> [show x, show y]) starts)
        ++ "\n===== Final symbols: ======\n"
        ++ makeTable (map (\(x, y) -> [show x, show y]) finals)
        ++ "\n===== Transitions: ========\n"
        ++ makeTable (map (\(x, y, z) -> [show x, show y, show z]) trans)

-------------------------------------------------------------------------------
-- Corpora.
-------------------------------------------------------------------------------

-- Sentences are lists of symbols.
type Sentence a = [a]

-- Corpora are lists of sentences (i.e. lists of lists of symbols).
type Corpus a = [Sentence a]

-- Tagged words are pairs: (word, part-of-speech tag)
newtype TaggedWord = TaggedWord (String, String)
                   deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Helpers.
-------------------------------------------------------------------------------

-- Turns a list of lists of strings into a pretty table.
makeTable :: [[String]] -> String
makeTable rows =
    render $ hsep 2 left (map (vcat left . map text) (transpose rows))
