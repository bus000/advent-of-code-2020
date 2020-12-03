{- --- Day 3: Toboggan Trajectory ---
 -
 - With the toboggan login problems resolved, you set off toward the airport.
 - While travel by toboggan might be easy, it's certainly not safe: there's very
 - minimal steering and the area is covered in trees. You'll need to see which
 - angles will take you near the fewest trees.
 -
 - Due to the local geology, trees in this area only grow on exact integer
 - coordinates in a grid. You make a map (your puzzle input) of the open squares
 - (.) and trees (#) you can see. For example:
 -
 - ..##.......
 - #...#...#..
 - .#....#..#.
 - ..#.#...#.#
 - .#...##..#.
 - ..#.##.....
 - .#.#.#....#
 - .#........#
 - #.##...#...
 - #...##....#
 - .#..#...#.#
 -
 - These aren't the only trees, though; due to something you read about once
 - involving arboreal genetics and biome stability, the same pattern repeats to
 - the right many times:
 -
 - ..##.........##.........##.........##.........##.........##.......  --->
 - #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
 - .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
 - ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
 - .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
 - ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
 - .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
 - .#........#.#........#.#........#.#........#.#........#.#........#
 - #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
 - #...##....##...##....##...##....##...##....##...##....##...##....#
 - .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
 -
 - You start on the open square (.) in the top-left corner and need to reach the
 - bottom (below the bottom-most row on your map).
 -
 - The toboggan can only follow a few specific slopes (you opted for a cheaper
 - model that prefers rational numbers); start by counting all the trees you
 - would encounter for the slope right 3, down 1:
 -
 - From your starting position at the top-left, check the position that is right
 - 3 and down 1. Then, check the position that is right 3 and down 1 from there,
 - and so on until you go past the bottom of the map.
 -
 - The locations you'd check in the above example are marked here with O where
 - there was an open square and X where there was a tree:
 -
 - ..##.........##.........##.........##.........##.........##.......  --->
 - #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
 - .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
 - ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
 - .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
 - ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
 - .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
 - .#........#.#........X.#........#.#........#.#........#.#........#
 - #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
 - #...##....##...##....##...#X....##...##....##...##....##...##....#
 - .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
 -
 - In this example, traversing the map using this slope would cause you to
 - encounter 7 trees.
 -
 - Starting at the top-left corner of your map and following a slope of right 3
 - and down 1, how many trees would you encounter?
 -}
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
