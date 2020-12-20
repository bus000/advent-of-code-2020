{- --- Day 20: Jurassic Jigsaw ---
 -
 - The high-speed train leaves the forest and quickly carries you south. You can
 - even see a desert in the distance! Since you have some spare time, you might
 - as well see if there was anything interesting in the image the Mythical
 - Information Bureau satellite captured.
 -
 - After decoding the satellite messages, you discover that the data actually
 - contains many small images created by the satellite's camera array. The
 - camera array consists of many cameras; rather than produce a single square
 - image, they produce many smaller square image tiles that need to be
 - reassembled back into a single image.
 -
 - Each camera in the camera array returns a single monochrome image tile with a
 - random unique ID number. The tiles (your puzzle input) arrived in a random
 - order.
 -
 - Worse yet, the camera array appears to be malfunctioning: each image tile has
 - been rotated and flipped to a random orientation. Your first task is to
 - reassemble the original image by orienting the tiles so they fit together.
 -
 - To show how the tiles should be reassembled, each tile's image data includes
 - a border that should line up exactly with its adjacent tiles. All tiles have
 - this border, and the border lines up exactly when the tiles are both oriented
 - correctly. Tiles at the edge of the image also have this border, but the
 - outermost edges won't line up with any other tiles.
 -
 - For example, suppose you have the following nine tiles:
 -
 -    Tile 2311:
 -    ..##.#..#.
 -    ##..#.....
 -    #...##..#.
 -    ####.#...#
 -    ##.##.###.
 -    ##...#.###
 -    .#.#.#..##
 -    ..#....#..
 -    ###...#.#.
 -    ..###..###
 -
 -    Tile 1951:
 -    #.##...##.
 -    #.####...#
 -    .....#..##
 -    #...######
 -    .##.#....#
 -    .###.#####
 -    ###.##.##.
 -    .###....#.
 -    ..#.#..#.#
 -    #...##.#..
 -
 -    Tile 1171:
 -    ####...##.
 -    #..##.#..#
 -    ##.#..#.#.
 -    .###.####.
 -    ..###.####
 -    .##....##.
 -    .#...####.
 -    #.##.####.
 -    ####..#...
 -    .....##...
 -
 -    Tile 1427:
 -    ###.##.#..
 -    .#..#.##..
 -    .#.##.#..#
 -    #.#.#.##.#
 -    ....#...##
 -    ...##..##.
 -    ...#.#####
 -    .#.####.#.
 -    ..#..###.#
 -    ..##.#..#.
 -
 -    Tile 1489:
 -    ##.#.#....
 -    ..##...#..
 -    .##..##...
 -    ..#...#...
 -    #####...#.
 -    #..#.#.#.#
 -    ...#.#.#..
 -    ##.#...##.
 -    ..##.##.##
 -    ###.##.#..
 -
 -    Tile 2473:
 -    #....####.
 -    #..#.##...
 -    #.##..#...
 -    ######.#.#
 -    .#...#.#.#
 -    .#########
 -    .###.#..#.
 -    ########.#
 -    ##...##.#.
 -    ..###.#.#.
 -
 -    Tile 2971:
 -    ..#.#....#
 -    #...###...
 -    #.#.###...
 -    ##.##..#..
 -    .#####..##
 -    .#..####.#
 -    #..#.#..#.
 -    ..####.###
 -    ..#.#.###.
 -    ...#.#.#.#
 -
 -    Tile 2729:
 -    ...#.#.#.#
 -    ####.#....
 -    ..#.#.....
 -    ....#..#.#
 -    .##..##.#.
 -    .#.####...
 -    ####.#.#..
 -    ##.####...
 -    ##..#.##..
 -    #.##...##.
 -
 -    Tile 3079:
 -    #.#.#####.
 -    .#..######
 -    ..#.......
 -    ######....
 -    ####.#..#.
 -    .#...#.##.
 -    #.#####.##
 -    ..#.###...
 -    ..#.......
 -    ..#.###...
 -
 - By rotating, flipping, and rearranging them, you can find a square
 - arrangement that causes all adjacent borders to line up:
 -
 -    #...##.#.. ..###..### #.#.#####.
 -    ..#.#..#.# ###...#.#. .#..######
 -    .###....#. ..#....#.. ..#.......
 -    ###.##.##. .#.#.#..## ######....
 -    .###.##### ##...#.### ####.#..#.
 -    .##.#....# ##.##.###. .#...#.##.
 -    #...###### ####.#...# #.#####.##
 -    .....#..## #...##..#. ..#.###...
 -    #.####...# ##..#..... ..#.......
 -    #.##...##. ..##.#..#. ..#.###...
 -
 -    #.##...##. ..##.#..#. ..#.###...
 -    ##..#.##.. ..#..###.# ##.##....#
 -    ##.####... .#.####.#. ..#.###..#
 -    ####.#.#.. ...#.##### ###.#..###
 -    .#.####... ...##..##. .######.##
 -    .##..##.#. ....#...## #.#.#.#...
 -    ....#..#.# #.#.#.##.# #.###.###.
 -    ..#.#..... .#.##.#..# #.###.##..
 -    ####.#.... .#..#.##.. .######...
 -    ...#.#.#.# ###.##.#.. .##...####
 -
 -    ...#.#.#.# ###.##.#.. .##...####
 -    ..#.#.###. ..##.##.## #..#.##..#
 -    ..####.### ##.#...##. .#.#..#.##
 -    #..#.#..#. ...#.#.#.. .####.###.
 -    .#..####.# #..#.#.#.# ####.###..
 -    .#####..## #####...#. .##....##.
 -    ##.##..#.. ..#...#... .####...#.
 -    #.#.###... .##..##... .####.##.#
 -    #...###... ..##...#.. ...#..####
 -    ..#.#....# ##.#.#.... ...##.....
 -
 - For reference, the IDs of the above tiles are:
 -
 -    1951    2311    3079
 -    2729    1427    2473
 -    2971    1489    1171
 -
 - To check that you've assembled the image correctly, multiply the IDs of the
 - four corner tiles together. If you do this with the assembled tiles from the
 - example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.
 -
 - Assemble the tiles into an image. What do you get if you multiply together
 - the IDs of the four corner tiles?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad as M

main :: IO ()
main = defaultMain parseInput handleInput

data Tile = Tile
    { tileId    :: !Int
    , tileUp    :: !String
    , tileRight :: !String
    , tileDown  :: !String
    , tileLeft  :: !String
    }
  deriving (Show, Eq, Ord)

handleInput :: [Tile] -> IO ()
handleInput = print . product . map tileId . findCorners

findCorners :: [Tile] -> [Tile]
findCorners tiles = filter (isCorner tiles) tiles

isCorner :: [Tile] -> Tile -> Bool
isCorner tiles tile = matchingEdges == 2
  where
    tiles' = filter (/= tile) tiles
    matchingEdges = length . filter (hasMatching tile) $ tiles'

hasMatching :: Tile -> Tile -> Bool
hasMatching tile1 tile2 = not . null $ do
    t1 <- legalTiles tile1
    t2 <- legalTiles tile2
    M.guard $ anyBorderMatch t1 t2

anyBorderMatch :: Tile -> Tile -> Bool
anyBorderMatch (Tile _ up1 right1 down1 left1) (Tile _ up2 right2 down2 left2) =
    up1 == down2 || right1 == left2 || down1 == up2 || left1 == right2

legalTiles :: Tile -> [Tile]
legalTiles tile = [tile, r1, r2, r3, f1, f2, f3, f4]
  where
    r1 = rotateRight tile
    r2 = rotateRight r1
    r3 = rotateRight r2
    f1 = flipVertically tile
    f2 = flipVertically r1
    f3 = flipVertically r2
    f4 = flipVertically r3

rotateRight :: Tile -> Tile
rotateRight tile = tile
    { tileUp = reverse $ tileLeft tile
    , tileRight = tileUp tile
    , tileDown = reverse $ tileRight tile
    , tileLeft = tileDown tile
    }

flipVertically :: Tile -> Tile
flipVertically tile = tile
    { tileUp = tileDown tile
    , tileDown = tileUp tile
    }

parseInput :: T.Text -> Either P.ParseError [Tile]
parseInput = P.parse (parseTiles <* P.eof) ""

parseTiles :: P.Parsec T.Text () [Tile]
parseTiles = parseTile `P.endBy` P.newline

parseTile :: P.Parsec T.Text () Tile
parseTile = do
    ident <- P.string "Tile " *> P.int <* P.string ":\n"
    tileLines <- parseLine `P.endBy1` P.newline
    let up = head tileLines
        right = map last tileLines
        down = last tileLines
        left = map head tileLines
    return $! Tile
        { tileId = ident
        , tileUp = up
        , tileRight = right
        , tileDown = down
        , tileLeft = left
        }
  where
    parseLine = P.many1 (P.oneOf ".#")
