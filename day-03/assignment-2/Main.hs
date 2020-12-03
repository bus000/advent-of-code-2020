{- Time to check the rest of the slopes - you need to minimize the probability
 - of a sudden arboreal stop, after all.
 -
 - Determine the number of trees you would encounter if, for each of the
 - following slopes, you start at the top-left corner and traverse the map all
 - the way to the bottom:
 -
 - * Right 1, down 1.
 - * Right 3, down 1. (This is the slope you already checked.)
 - * Right 5, down 1.
 - * Right 7, down 1.
 - * Right 1, down 2.
 -
 - In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
 - respectively; multiplied together, these produce the answer 336.
 -
 - What do you get if you multiply together the number of trees encountered on
 - each of the listed slopes?
 -}
module Main where

import AdventOfCode
import qualified Data.CircularList as C
import qualified Data.Text as T
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data Tile = Tree | Empty deriving (Show, Eq, Ord)

data Plan = Plan { _down :: !Int, _right :: !Int } deriving (Show, Eq, Ord)

type Forest = [C.CircularList Tile]

type Path = [Tile]

handleInput :: Forest -> IO ()
handleInput forest = print . product $ trees
  where
    forest' = tail forest
    trees = map countTrees [Plan 1 1, Plan 1 3, Plan 1 5, Plan 1 7, Plan 2 1]
    countTrees plan = length . filter (== Tree) $ findPath plan forest'

findPath :: Plan -> Forest -> Path
findPath (Plan down right) forest = map C.unsafeCurrent forest''
  where
    forest' = everyNth down forest
    forest'' = zipWith C.moveRightN forest' [right, right+right..]

everyNth :: Int -> [a] -> [a]
everyNth n xs = map fst . filter (\(_, i) -> i `mod` n == 0) $ zip xs [1..]

parseInput :: T.Text -> Either P.ParseError Forest
parseInput = P.parse (parseForest <* P.eof) ""

parseForest :: P.Parsec T.Text () Forest
parseForest = P.many parseTreeLine

parseTreeLine :: P.Parsec T.Text () (C.CircularList Tile)
parseTreeLine = C.fromList <$> P.many parseTile <* P.char '\n'

parseTile :: P.Parsec T.Text () Tile
parseTile = P.choice [P.char '#' *> pure Tree, P.char '.' *> pure Empty]
