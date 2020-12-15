{- Impressed, the Elves issue you a challenge: determine the 30000000th number
 - spoken. For example, given the same starting numbers as above:
 -
 - Given 0,3,6, the 30000000th number spoken is 175594.
 - Given 1,3,2, the 30000000th number spoken is 2578.
 - Given 2,1,3, the 30000000th number spoken is 3544142.
 - Given 1,2,3, the 30000000th number spoken is 261214.
 - Given 2,3,1, the 30000000th number spoken is 6895259.
 - Given 3,2,1, the 30000000th number spoken is 18.
 - Given 3,1,2, the 30000000th number spoken is 362.
 -
 - Given your starting numbers, what will be the 30000000th number spoken?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Int] -> IO ()
handleInput = mapM_ print . take 1 . drop (30000000 - 1) . playGame

playGame :: [Int] -> [Int]
playGame start = start ++ L.unfoldr go initialState
  where
    initialSeen = Map.fromList $ zip (init start) [0..]
    initialState = (initialSeen, length start - 1, last start)

    go (lastSeen, pos, lastN) = case Map.lookup lastN lastSeen of
        Just i -> Just (pos - i, (Map.insert lastN pos lastSeen, pos + 1, pos - i))
        Nothing -> Just (0, (Map.insert lastN pos lastSeen, pos + 1, 0))

parseInput :: T.Text -> Either P.ParseError [Int]
parseInput = P.parse (parseInts <* P.eof) ""

parseInts :: P.Parsec T.Text () [Int]
parseInts = P.int `P.sepBy` P.char ',' <* P.newline
