module Main where

import System.Environment
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.Posix.Internals (fileType)
import Debug.Trace
import Control.Parallel.Strategies
import Control.Parallel

newtype MustContain = MustContain [Char] deriving (Eq, Show)
newtype CannotContain = CannotContain [Char] deriving (Eq, Show)
newtype Positions = Positions [(Char,Int)] deriving (Eq, Show)
newtype NotPositions = NotPositions [(Char,Int)] deriving (Eq, Show)

data Restrictions = Restrictions MustContain CannotContain Positions NotPositions deriving (Eq, Show)

-- a Restrictions with no restrictions
emptyRestrictions = Restrictions (MustContain []) (CannotContain []) (Positions []) (NotPositions [])

-- Returns true if a word conforms to all the restrictions
wordAllowed :: Restrictions -> String -> Bool
wordAllowed (Restrictions (MustContain mustContain) (CannotContain cannotContain)
            (Positions positions) (NotPositions notPositions)) w =
  -- each letter in mustContain must be in the word
  length mustContain == length (mustContain `intersect` nub w) &&
  -- none of the letters in cannotContain can be in the word
  null (cannotContain `intersect` w) &&
  -- any letter-at-position restrictions must match
  all (posOK w) positions &&
  -- any letter-not-at-position restrictions must match
  all (notPosOK w) notPositions
  where
    posOK w (ch,pos) = ch == (w !! pos)
    notPosOK w (ch,pos) = ch /= (w !! pos)

-- Loads a file of words with each word on a separate line
-- and filters it for words of a specific length
loadDict :: String -> Int -> IO [String]
loadDict file wordLength = do
  -- load the dictionary of common words
  contents <- readFile file
  -- only use the ones with the correct length
  return $ filter checkLen (lines contents)
  where
    checkLen x = length x == wordLength

-- Loads a file of words and frequences, with word & its frequency on a separate line
-- and filters it for words of a specific length
loadFreqDict :: String -> Int -> IO (Map.Map String Int)
loadFreqDict file wordLength = do
  contents <- readFile file
  -- turn each line into a (string,int) pair, filter them for length
  -- and create a map from the resulting list
  return $ Map.fromList $ filter lenOK $ map (makePair . words) $ lines contents
  where
    lenOK (w,f) = length w == wordLength
    makePair ws = (head ws, read $ ws !! 1)

-- Computes a list of letters to add to MustContain from a word result
-- where the lower case letters in the word result indicate a letter
-- that is correct but in the wrong position
getMustContain :: String -> [Char]
getMustContain wordResult =
  -- any lower case letters in wordResult are additions to mustContain
  map toUpper $ filter isLower wordResult

-- Computes a list of letters to add to CannotContain from the last word
-- tried and the word result, where each . in the word result means that
-- the corresponding letter in the last word should be added to CannotContain
getCannotContain :: String -> String -> [Char]
getCannotContain [] _ = []
getCannotContain (lw:lws) (wr:wrs) =
  -- any . in wr means that lw is a letter that should be added to cannotContain
  if wr == '.' then
    lw : getCannotContain lws wrs
  else
    getCannotContain lws wrs

-- Computes a list of Letter-Position pairs for letters that must be in a
-- particular position from the word result. An upper-case letter in the
-- word result indicates that the letter must be in that position
getPositions :: String -> [Int] -> [(Char,Int)]
getPositions [] _ = []
getPositions (wr:wrs) (n:ns) =
  -- if wr is an upper-case letter, the letter and its position represent a
  -- letter-at-position restriction
  if isUpper wr then
    (wr,n) : getPositions wrs ns
  else
    getPositions wrs ns

-- Computes a list of Letter-Position pairs for letters that cannot be in a
-- particular position from the word result. A lower-case letter in the
-- word result indicates that the letter cannot be in that position
getNotPositions :: String -> [Int] -> [(Char,Int)]
getNotPositions [] _ = []
getNotPositions (wr:wrs) (n:ns) =
  -- if wr is a lower-case letter, the letter and its position represent a
  -- letter-not-at-position restriction
  if isLower wr then
    (toUpper wr,n) : getNotPositions wrs ns
  else
    getNotPositions wrs ns

-- Updates the restrictions with the mustContain, cannotContain, positions and notPositions
-- derived from the most recent lastWord and wordResult
updateRestrictions :: Restrictions -> String -> String -> Restrictions
updateRestrictions (Restrictions (MustContain mustContain) (CannotContain cannotContain)
            (Positions positions) (NotPositions notPositions)) lastWord wordResult =
  Restrictions (MustContain (mustContain ++ getMustContain wordResult))
    (CannotContain (cannotContain ++ (lastWord \\ getCannotContain lastWord wordResult)))
    (Positions (positions ++ getPositions wordResult [0..]))
    (NotPositions (notPositions ++ getNotPositions wordResult [0..]))

-- Updated the letter counts with the letters from the word
updateLetterCounts :: Map.Map Char Int -> String -> Map.Map Char Int
updateLetterCounts counts word =
  foldl' addLetter counts word
  where
    addLetter counts ch = Map.insertWith (+) ch 1 counts

-- Count all the letters in each word in the current dict
countLetters :: [String] -> Map.Map Char Int
countLetters dict =
  foldl' updateLetterCounts (Map.empty) dict

-- Counts the number of letters this word has in common with every word in the dict
evalCandidate :: Map.Map Char Int -> String -> (String, Int)
evalCandidate counts word =
  -- sum the letter counts for each unique letter in word
  (word, sum $ map ((Map.!) counts) (nub word))

computePositions pos [] _ = []
computePositions pos (w:ws) (g:gs) =
  if w == g then
    (w,pos) : computePositions (pos+1) ws gs
  else
    computePositions (pos+1) ws gs

checkNotPos :: String -> String -> [(Char,Int)] -> Int -> [(Char,Int)]
checkNotPos w guess l n =
  if wn /= gn && elem gn w then
    (gn,n):l
  else
    l
  where
    wn = w !! n
    gn = guess !! n


computeNotPositions :: String -> String -> [(Char, Int)]
computeNotPositions w guess =
  foldl' (checkNotPos w guess) [] [0..length w - 1]

generateRestrictions w guess =
  Restrictions (MustContain mustContain) (CannotContain cannotContain)
    (Positions positions) (NotPositions notPositions)
  where
    mustContain = nub w
    cannotContain = nub guess \\ nub w
    positions = computePositions 0 w guess
    notPositions = computeNotPositions w guess

addGuess :: [String] -> String -> Int -> Int -> String -> Int
addGuess dict guess depth acc w =
--  trace ("Trying guess "++guess++" against word "++w) acc+newDepth
  acc+newDepth
  where
    (_, newDepth) = bestCandidate' newDict (depth+1) w
    newRestrictions = generateRestrictions w guess
    newDict = filter (wordAllowed newRestrictions) dict

bestCandidate' :: [String] -> Int -> String -> (String,Int)
bestCandidate' [] _ w = error ("Dictionary empty for word "++w)
--bestCandidate' [final] depth w = trace ("Got to "++w++" at depth "++(show depth)) (w,depth)
bestCandidate' [final] depth w = (w,depth)
bestCandidate' dict depth w =
--  trace ("Trying "++w++" at depth "++(show depth)++" with dict "++(show  dict))
--  (w, foldl' (addGuess dict w depth) 0 dict)
-- if depth == 0 then
--   let res = (w, foldl' (addGuess dict w depth) 0 dict) in
--   trace (show res) res
-- else
  (w, foldl' (addGuess dict w depth) 0 dict)

bestCandidate dict depth =
  minimumBy compareCandidates allCandidates
  where
    allCandidates = parMap rpar (bestCandidate' dict depth) dict
    compareCandidates (_,n1) (_,n2) = compare n1 n2

-- Print the next word to try and wait for the user to enter the response
-- then repeat
doRound :: Bool -> Restrictions -> [String] -> String -> IO String
doRound debugFlag restrictions  dict lastWord = do
  if debugFlag then do
    putStr "Dictionary now has "
    putStr $ show $ length dict
    putStrLn " entries"
    print dict
  else do
    return ()
  putStrLn lastWord
  wordResult <- getLine
  -- update the restrictions based on the result
  let rests = updateRestrictions restrictions lastWord wordResult

  -- filter the dictionary based on the updated restrictions
  let newDict = filter (wordAllowed rests) dict

  -- choose a new candidate word
  let nextCandidate = fst $ bestCandidate newDict 0

  if length newDict > 1 then
    doRound debugFlag rests newDict nextCandidate
  else
    return nextCandidate

-- Looks for "--debug" in command-line arguments
getDebugFlag :: [String] -> Bool
getDebugFlag [] = False
getDebugFlag (f:fs) = (f == "--debug") || getDebugFlag fs

-- Looks for "--length n" in command-line arguments
getWordLength :: [String] -> Int
getWordLength [] = 5
getWordLength [_] = 5
getWordLength (f:fs) =
  if f == "--length" then
    read (head fs)
  else
    getWordLength fs

main :: IO ()
main = do
  args <- getArgs
  let wordLength = getWordLength args
  let debugFlag = getDebugFlag args
  -- dict <- loadDict "common.txt" wordLength
  dict <- loadDict "common.txt" 5
  --let startWord = fst $ bestCandidate dict 0
  let startWord = "SALES"
  finalWord <- doRound debugFlag emptyRestrictions dict startWord
  putStr "Final word is "
  putStrLn finalWord
