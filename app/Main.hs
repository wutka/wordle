module Main where

import System.Environment
import Data.List
import Data.Char

newtype MustContain = MustContain [Char] deriving (Eq, Show)
newtype CannotContain = CannotContain [Char] deriving (Eq, Show)
newtype Positions = Positions [(Char,Int)] deriving (Eq, Show)
newtype NotPositions = NotPositions [(Char,Int)] deriving (Eq, Show)

data Restrictions = Restrictions MustContain CannotContain Positions NotPositions deriving (Eq, Show)

emptyRestrictions = Restrictions (MustContain []) (CannotContain []) (Positions []) (NotPositions [])

wordAllowed :: Restrictions -> String -> Bool
wordAllowed (Restrictions (MustContain mustContain) (CannotContain cannotContain)
            (Positions positions) (NotPositions notPositions)) w =
  -- each letter in mustContain must be in the word
  length mustContain == length (mustContain `intersect` nub w) &&
  -- none of the letters in cannotContain can be in the word
  null (cannotContain `intersect` w) &&
  -- any letter-at-position restrictions must match
  all (posOK w) positions &&
  all (notPosOK w) notPositions
  where
    posOK w (ch,pos) = ch == (w !! pos)
    notPosOK w (ch,pos) = ch /= (w !! pos)

loadDict :: String -> Int -> IO [String]
loadDict file wordLength = do
  -- load the dictionary of common words
  contents <- readFile file
  -- only use the ones with the correct length
  return $ filter checkLen (lines contents)
  where
    checkLen x = length x == wordLength

getMustContain :: String -> [Char]
getMustContain wordResult =
  -- any lower case letters in wordResult are additions to mustContain
  map toUpper $ filter isLower wordResult

getCannotContain :: String -> String -> [Char]
getCannotContain [] _ = []
getCannotContain (lw:lws) (wr:wrs) =
  -- any . in wr means that lw is a letter that should be added to cannotContain
  if wr == '.' then
    lw : getCannotContain lws wrs
  else
    getCannotContain lws wrs

getPositions :: String -> [Int] -> [(Char,Int)]
getPositions [] _ = []
getPositions (wr:wrs) (n:ns) =
  -- if wr is an upper-case letter, the letter and its position represent a
  -- letter-at-position restriction
  if isUpper wr then
    (wr,n) : getPositions wrs ns
  else
    getPositions wrs ns

getNotPositions :: String -> [Int] -> [(Char,Int)]
getNotPositions [] _ = []
getNotPositions (wr:wrs) (n:ns) =
  -- if wr is a lower-case letter, the letter and its position represent a
  -- letter-not-at-position restriction
  if isLower wr then
    (toUpper wr,n) : getNotPositions wrs ns
  else
    getNotPositions wrs ns

updateRestrictions :: Restrictions -> String -> String -> Restrictions
updateRestrictions (Restrictions (MustContain mustContain) (CannotContain cannotContain)
            (Positions positions) (NotPositions notPositions)) lastWord wordResult =
  -- update the restrictions with the mustContain, cannotContain, and positions
  -- derived from the most recent lastWord and wordResult
  Restrictions (MustContain (mustContain ++ getMustContain wordResult))
    (CannotContain (cannotContain ++ getCannotContain lastWord wordResult))
    (Positions (positions ++ getPositions wordResult [0..]))
    (NotPositions (notPositions ++ getNotPositions wordResult [0..]))

numCommonLetters :: String -> String -> Int
numCommonLetters s1 s2 =
  -- return the number of letters that two words in common
  length $ nub s1 `intersect` nub s2

evalCandidate :: [String] -> String -> (String, Int)
evalCandidate dict word =
  -- count the number of letters this word has in common with every word in the dict
  (word, sum $ map (numCommonLetters word) dict)

bestCandidate :: [String] -> String
bestCandidate dict =
  -- the best candidate is the one that has the most letters in common
  -- with the entire dictionary
  fst $ maximumBy compareCandidates $ map (evalCandidate dict) dict
  where
    compareCandidates (_, c1s) (_, c2s) = compare c1s c2s

doRound :: Bool -> Restrictions -> [String] -> String -> IO ()
doRound debugFlag restrictions dict lastWord = do
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
  let nextCandidate = bestCandidate newDict
  doRound debugFlag rests newDict nextCandidate

getDebugFlag :: [String] -> Bool
getDebugFlag [] = False
getDebugFlag (f:fs) = (f == "--debug") || getDebugFlag fs

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
  dict <- loadDict "common.txt" wordLength
  let startWord = if wordLength == 5 then "AROSE" else bestCandidate dict
  -- bestCandidate takes a while to run on the whole dictionary, but since the
  -- initial candidate is always the same at the beginning, hard-code it
  doRound debugFlag emptyRestrictions dict startWord
