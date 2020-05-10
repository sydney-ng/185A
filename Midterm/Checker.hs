module Checker ( checkPOS
               , checkTag
               , checkTagBest
               , checkAccuracy
               , accuracy
               )
               where

-- For loading the corpus files.
import System.Directory (listDirectory)
import qualified Data.Text as T (strip, unpack)
import qualified Data.Text.IO as TIO (readFile)

-- For parsing the file into pieces.
import qualified Data.List.Split as Split (endBy)

-- For list processing.
import Data.List (foldl', foldl1')

-- For dealing with weird Brown non-tags.
import qualified Data.Maybe as Maybe (mapMaybe)

-- For the midterm.
import ProbSLG
import Helpers
import Midterm ( buildProbSLG
               , sanitize
               , posProbSLG
               , tag
               , tagBest
               )

-------------------------------------------------------------------------------
-- File paths for Brown corpus.
-------------------------------------------------------------------------------

brownSmall :: FilePath
brownSmall = "brown-small/"

-------------------------------------------------------------------------------
-- Run posProbSLG function against corpora.
-------------------------------------------------------------------------------

checkPOS = checkPOS' brownSmall

checkPOS' :: FilePath -> IO (ProbSLG String)
checkPOS' path =
    posProbSLG <$> loadParsedCorpus path

-------------------------------------------------------------------------------
-- Run tag function against corpora.
-------------------------------------------------------------------------------

checkTag = checkTag' brownSmall

checkTag' :: FilePath -> String -> IO [(Sentence TaggedWord, Double)]
checkTag' path sentence =
    (tag <$> loadParsedCorpus path) <*> pure sentence

checkTagBest = checkTagBest' brownSmall

checkTagBest' :: FilePath -> String -> IO (Sentence TaggedWord)
checkTagBest' path sentence =
    (tagBest <$> loadParsedCorpus path) <*> pure sentence

-------------------------------------------------------------------------------
-- Compute overall accuracy against corpora.
-------------------------------------------------------------------------------

checkAccuracy = checkAccuracy' brownSmall

checkAccuracy' :: FilePath -> IO Double
checkAccuracy' path =
    accuracy <$> loadParsedCorpus path

accuracy :: Corpus TaggedWord -> Double
accuracy unsanitizedCorpus =
    let (correct, total) = foldl' checkSentence (0, 0) corpus
    in  divide correct total
  where
    -- Shorten corpus sentences (for faster performance).
    shorter = map (take 5) unsanitizedCorpus
    -- Run corpus sentences through sanitization.
    corpus = foldl' (\a b -> map b a) shorter sanitize
    -- Faster tagging.
    tB str = fst $ foldl1' (\x y -> if snd x >= snd y then x else y)
                           (tag corpus str)
    -- Check single sentence.
    checkSentence (correct, total) original =
        foldl' checkPair (correct, total) (zip original tagged)
      where
        tagged = tB . unwords $ map getWord original
    -- Check single pair.
    checkPair (correct, total) (parsed, unparsed) =
        if   parsed == unparsed
        then (correct+1, total+1)
        else (correct, total+1)

-------------------------------------------------------------------------------
-- Load corpus files.
-------------------------------------------------------------------------------

-- Loads the corpus and parses it into tagged words.
loadParsedCorpus :: FilePath -> IO (Corpus TaggedWord)
loadParsedCorpus path =
    filter (not . null) <$> fmap parseSentence <$> loadCorpus path

-- Loads all the corpus files, unparsed.
loadCorpus :: FilePath -> IO [String]
loadCorpus path =
    concat <$> (listDirectory path >>= mapM (loadFile . absPath))
  where
    absPath = (++) path

-- Loads an individual file as a string list.
loadFile :: FilePath -> IO [String]
loadFile path =
    (Split.endBy "./.") <$> readFileStrict path

-- Reads a file strict IO, avoiding resource exhaustion.
readFileStrict :: FilePath -> IO String
readFileStrict path =
    T.unpack <$> T.strip <$> (TIO.readFile path)

-------------------------------------------------------------------------------
-- Parse corpus files into tag pairs.
-------------------------------------------------------------------------------

-- Parses a sentence into tagged words.
parseSentence :: String -> (Sentence TaggedWord)
parseSentence str =
    Maybe.mapMaybe parseWord (words str)

-- Separates word and tag into a pair, chucking what does not work into the
-- ether for simplicity.
parseWord :: String -> Maybe TaggedWord
parseWord str
    | length parts == 2 = Just $ makeWord parts
    | otherwise         = Nothing  -- Aufwiedersehen...
  where
    parts = Split.endBy "/" str
    makeWord (x:y:[]) = TaggedWord (x, y)
