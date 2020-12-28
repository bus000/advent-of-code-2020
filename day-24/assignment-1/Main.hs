{- --- Day 24: Lobby Layout ---
 -
 - Your raft makes it to the tropical island; it turns out that the small crab
 - was an excellent navigator. You make your way to the resort.
 -
 - As you enter the lobby, you discover a small problem: the floor is being
 - renovated. You can't even reach the check-in desk until they've finished
 - installing the new tile floor.
 -
 - The tiles are all hexagonal; they need to be arranged in a hex grid with a
 - very specific color pattern. Not in the mood to wait, you offer to help
 - figure out the pattern.
 -
 - The tiles are all white on one side and black on the other. They start with
 - the white side facing up. The lobby is large enough to fit whatever pattern
 - might need to appear there.
 -
 - A member of the renovation crew gives you a list of the tiles that need to be
 - flipped over (your puzzle input). Each line in the list identifies a single
 - tile that needs to be flipped by giving a series of steps starting from a
 - reference tile in the very center of the room. (Every line starts from the
 - same reference tile.)
 -
 - Because the tiles are hexagonal, every tile has six neighbors: east,
 - southeast, southwest, west, northwest, and northeast. These directions are
 - given in your list, respectively, as e, se, sw, w, nw, and ne. A tile is
 - identified by a series of these directions with no delimiters; for example,
 - esenee identifies the tile you land on if you start at the reference tile and
 - then move one tile east, one tile southeast, one tile northeast, and one tile
 - east.
 -
 - Each time a tile is identified, it flips from white to black or from black to
 - white. Tiles might be flipped more than once. For example, a line like esew
 - flips a tile immediately adjacent to the reference tile, and a line like
 - nwwswee flips the reference tile itself.
 -
 - Here is a larger example:
 -
 -    sesenwnenenewseeswwswswwnenewsewsw
 -    neeenesenwnwwswnenewnwwsewnenwseswesw
 -    seswneswswsenwwnwse
 -    nwnwneseeswswnenewneswwnewseswneseene
 -    swweswneswnenwsewnwneneseenw
 -    eesenwseswswnenwswnwnwsewwnwsene
 -    sewnenenenesenwsewnenwwwse
 -    wenwwweseeeweswwwnwwe
 -    wsweesenenewnwwnwsenewsenwwsesesenwne
 -    neeswseenwwswnwswswnw
 -    nenwswwsewswnenenewsenwsenwnesesenew
 -    enewnwewneswsewnwswenweswnenwsenwsw
 -    sweneswneswneneenwnewenewwneswswnese
 -    swwesenesewenwneswnwwneseswwne
 -    enesenwswwswneneswsenwnewswseenwsese
 -    wnwnesenesenenwwnenwsewesewsesesew
 -    nenewswnwewswnenesenwnesewesw
 -    eneswnwswnwsenenwnwnwwseeswneewsenese
 -    neswnwewnwnwseenwseesewsenwsweewe
 -    wseweeenwnesenwwwswnew
 -
 - In the above example, 10 tiles are flipped once (to black), and 5 more are
 - flipped twice (to black, then back to white). After all of these instructions
 - have been followed, a total of 10 tiles are black.
 -
 - Go through the renovation crew's list and determine which tiles they need to
 - flip. After all of the instructions have been followed, how many tiles are
 - left with the black side up?
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
handleInput = print . Set.size . foldr flipTile Set.empty

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
