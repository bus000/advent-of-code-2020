module Main where

import AdventOfCode
import qualified Data.CircularList as C
import qualified Data.Text as T
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data Tile = Tree | Empty deriving (Show, Eq, Ord)

type Forest = [C.CircularList Tile]

type Path = [Tile]

handleInput :: Forest -> IO ()
handleInput = print . length . filter (== Tree) . findPath 3 . tail

findPath :: Int -> Forest -> Path
findPath n forest = map C.unsafeCurrent forest'
  where
    forest' = zipWith C.moveRightN forest [n, n+n..]

parseInput :: T.Text -> Either P.ParseError Forest
parseInput = P.parse (parseForest <* P.eof) ""

parseForest :: P.Parsec T.Text () Forest
parseForest = P.many parseTreeLine

parseTreeLine :: P.Parsec T.Text () (C.CircularList Tile)
parseTreeLine = C.fromList <$> P.many parseTile <* P.char '\n'

parseTile :: P.Parsec T.Text () Tile
parseTile = P.choice [P.char '#' *> pure Tree, P.char '.' *> pure Empty]
