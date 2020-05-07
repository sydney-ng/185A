module Checker ( checkPOS
               , checkPOSFull
               , checkTag
               , checkTagFull
               , checkTagBest
               , checkTagBestFull
               , checkAccuracy
               , checkAccuracyFull
               , accuracy
               )
               where

-- For loading the corpus files.
import System.Directory (listDirectory)
import qualified Data.Text as T (strip, unpack)
import qualified Data.Text.IO as TIO (readFile)

-- For parsing the file into pieces.
import qualified Data.List.Split as Split (endBy)

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

brownFull :: FilePath
brownFull = "brown/"

brownSmall :: FilePath
brownSmall = "brown-small/"

-------------------------------------------------------------------------------
-- Run posProbSLG function against corpora.
-------------------------------------------------------------------------------

checkPOS = checkPOS' brownSmall
checkPOSFull = checkPOS' brownFull

checkPOS' :: FilePath -> IO (ProbSLG String)
checkPOS' path =
    posProbSLG <$> loadParsedCorpus path

-------------------------------------------------------------------------------
-- Run tag function against corpora.
-------------------------------------------------------------------------------

checkTag = checkTag' brownSmall
checkTagFull = checkTag' brownFull

checkTag' :: FilePath -> String -> IO [(Sentence TaggedWord, Double)]
checkTag' path sentence =
    (tag <$> posProbSLG <$> loadParsedCorpus path) <*> pure sentence

checkTagBest = checkTagBest' brownSmall
checkTagBestFull = checkTagBest' brownFull

checkTagBest' :: FilePath -> String -> IO (Sentence TaggedWord)
checkTagBest' path sentence =
    (tagBest <$> posProbSLG <$> loadParsedCorpus path) <*> pure sentence

-------------------------------------------------------------------------------
-- Compute overall accuracy against corpora.
-------------------------------------------------------------------------------

checkAccuracy = checkAccuracy' brownSmall
checkAccuracyFull = checkAccuracy' brownFull

checkAccuracy' :: FilePath -> IO Double
checkAccuracy' path =
    accuracy <$> loadParsedCorpus path

accuracy :: Corpus TaggedWord -> Double
accuracy unsanitizedCorpus =
    divide (length $ filter check pairs) -- Number of correct sentences.
           (length $ concat corpus)      -- Number of total sentences.
  where
    -- Run corpus sentences through sanitization.
    corpus = foldl (\a b -> map b a) unsanitizedCorpus sanitize
    -- Generate PSLG.
    g = posProbSLG corpus
    -- Strip the corpus of all tags.
    unparsedCorpus = map getWord <$> corpus
    -- Run the corpus through the tagger.
    taggedCorpus = map (tagBest g . unwords) unparsedCorpus
    -- Pair up parsed and unparsed sentences.
    pairs = zip (concat corpus) (concat taggedCorpus)
    -- Function for checking how the PSLG fares for a sentence.
    check (parsed, unparsed) = parsed == unparsed
    --check (parsed, unparsed) = parsed == (tagBest g (unwords unparsed))

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
