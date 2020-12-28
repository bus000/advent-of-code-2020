{- The tile floor in the lobby is meant to be a living art exhibit. Every day,
 - the tiles are all flipped according to the following rules:
 -
 -  * Any black tile with zero or more than 2 black tiles immediately adjacent
 -    to it is flipped to white.
 -  * Any white tile with exactly 2 black tiles immediately adjacent to it is
 -    flipped to black.
 -
 - Here, tiles immediately adjacent means the six tiles directly touching the
 - tile in question.
 -
 - The rules are applied simultaneously to every tile; put another way, it is
 - first determined which tiles need to be flipped, then they are all flipped at
 - the same time.
 -
 - In the above example, the number of black tiles that are facing up after the
 - given number of days has passed is as follows:
 -
 -    Day 1: 15
 -    Day 2: 12
 -    Day 3: 25
 -    Day 4: 14
 -    Day 5: 23
 -    Day 6: 28
 -    Day 7: 41
 -    Day 8: 37
 -    Day 9: 49
 -    Day 10: 37
 -
 -    Day 20: 132
 -    Day 30: 259
 -    Day 40: 406
 -    Day 50: 566
 -    Day 60: 788
 -    Day 70: 1106
 -    Day 80: 1373
 -    Day 90: 1844
 -    Day 100: 2208
 -
 - After executing this process a total of 100 times, there would be 2208 black
 - tiles facing up.
 -
 - How many tiles will be black after 100 days?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
    deriving (Show, Eq, Ord)

type Path = [Direction]

handleInput :: [Path] -> IO ()
handleInput
    = print
    . Set.size
    . (!! 100)
    . iterate evolve
    . foldr flipTile Set.empty

evolve :: Set.Set (Int, Int) -> Set.Set (Int, Int)
evolve blacks = Set.union newBlacks oldBlacks
  where
    whites
        = Set.fromList
        . filter (\x -> not $ x `Set.member` blacks)
        . concatMap neighbours $ blacks
    newBlacks = Set.filter wFilter whites
    oldBlacks = Set.filter bFilter blacks

    bFilter position =
        blackNeighbourCount blacks position == 1 ||
        blackNeighbourCount blacks position == 2
    wFilter position = blackNeighbourCount blacks position == 2

blackNeighbourCount :: Set.Set (Int, Int) -> (Int, Int) -> Int
blackNeighbourCount tiles = length . filter (`Set.member` tiles) . neighbours

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours position =
    [ applyDirection East position
    , applyDirection SouthEast position
    , applyDirection SouthWest position
    , applyDirection West position
    , applyDirection NorthWest position
    , applyDirection NorthEast position
    ]

flipTile :: Path -> Set.Set (Int, Int) -> Set.Set (Int, Int)
flipTile path tiles
    | location `Set.member` tiles = Set.delete location tiles
    | otherwise = Set.insert location tiles
  where
    location = foldr applyDirection (0, 0) path

applyDirection :: Direction -> (Int, Int) -> (Int, Int)
applyDirection East (x, y) = (x + 1, y)
applyDirection SouthEast (x, y) = (x, y + 1)
applyDirection SouthWest (x, y) = (x - 1, y + 1)
applyDirection West (x, y) = (x - 1, y)
applyDirection NorthWest (x, y) = (x, y - 1)
applyDirection NorthEast (x, y) = (x + 1, y - 1)

parseInput :: T.Text -> Either P.ParseError [Path]
parseInput = P.parse (parsePaths <* P.eof) ""

parsePaths :: P.Parsec T.Text () [Path]
parsePaths = parsePath `P.endBy` P.newline

parsePath :: P.Parsec T.Text () Path
parsePath = P.many parseDirection

parseDirection :: P.Parsec T.Text () Direction
parseDirection = P.choice $ map P.try
    [ P.string "se" *> pure SouthEast
    , P.string "sw" *> pure SouthWest
    , P.string "ne" *> pure NorthEast
    , P.string "nw" *> pure NorthWest
    , P.string "e" *> pure East
    , P.string "w" *> pure West
    ]
