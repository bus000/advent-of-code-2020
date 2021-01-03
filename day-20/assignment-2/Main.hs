{- Now, you're ready to check the image for sea monsters.
 -
 - The borders of each tile are not part of the actual image; start by removing
 - them.
 -
 - In the example above, the tiles become:
 -
 -    .#.#..#. ##...#.# #..#####
 -    ###....# .#....#. .#......
 -    ##.##.## #.#.#..# #####...
 -    ###.#### #...#.## ###.#..#
 -    ##.#.... #.##.### #...#.##
 -    ...##### ###.#... .#####.#
 -    ....#..# ...##..# .#.###..
 -    .####... #..#.... .#......
 -
 -    #..#.##. .#..###. #.##....
 -    #.####.. #.####.# .#.###..
 -    ###.#.#. ..#.#### ##.#..##
 -    #.####.. ..##..## ######.#
 -    ##..##.# ...#...# .#.#.#..
 -    ...#..#. .#.#.##. .###.###
 -    .#.#.... #.##.#.. .###.##.
 -    ###.#... #..#.##. ######..
 -
 -    .#.#.### .##.##.# ..#.##..
 -    .####.## #.#...## #.#..#.#
 -    ..#.#..# ..#.#.#. ####.###
 -    #..####. ..#.#.#. ###.###.
 -    #####..# ####...# ##....##
 -    #.##..#. .#...#.. ####...#
 -    .#.###.. ##..##.. ####.##.
 -    ...###.. .##...#. ..#..###
 -
 - Remove the gaps to form the actual image:
 -
 -    .#.#..#.##...#.##..#####
 -    ###....#.#....#..#......
 -    ##.##.###.#.#..######...
 -    ###.#####...#.#####.#..#
 -    ##.#....#.##.####...#.##
 -    ...########.#....#####.#
 -    ....#..#...##..#.#.###..
 -    .####...#..#.....#......
 -    #..#.##..#..###.#.##....
 -    #.####..#.####.#.#.###..
 -    ###.#.#...#.######.#..##
 -    #.####....##..########.#
 -    ##..##.#...#...#.#.#.#..
 -    ...#..#..#.#.##..###.###
 -    .#.#....#.##.#...###.##.
 -    ###.#...#..#.##.######..
 -    .#.#.###.##.##.#..#.##..
 -    .####.###.#...###.#..#.#
 -    ..#.#..#..#.#.#.####.###
 -    #..####...#.#.#.###.###.
 -    #####..#####...###....##
 -    #.##..#..#...#..####...#
 -    .#.###..##..##..####.##.
 -    ...###...##...#...#..###
 -
 - Now, you're ready to search for sea monsters! Because your image is
 - monochrome, a sea monster will look like this:
 -
 -                #
 -    #    ##    ##    ###
 -     #  #  #  #  #  #
 -
 - When looking for this pattern in the image, the spaces can be anything; only
 - the # need to match. Also, you might need to rotate or flip your image before
 - it's oriented correctly to find sea monsters. In the above image, after
 - flipping and rotating it to the appropriate orientation, there are two sea
 - monsters (marked with O):
 -
 -    .####...#####..#...###..
 -    #####..#..#.#.####..#.#.
 -    .#.#...#.###...#.##.O#..
 -    #.O.##.OO#.#.OO.##.OOO##
 -    ..#O.#O#.O##O..O.#O##.##
 -    ...#.#..##.##...#..#..##
 -    #.##.#..#.#..#..##.#.#..
 -    .###.##.....#...###.#...
 -    #.####.#.#....##.#..#.#.
 -    ##...#..#....#..#...####
 -    ..#.##...###..#.#####..#
 -    ....#.##.#.#####....#...
 -    ..##.##.###.....#.##..#.
 -    #...#...###..####....##.
 -    .#.##...#.##.#.#.###...#
 -    #.###.#..####...##..#...
 -    #.###...#.##...#.##O###.
 -    .O##.#OO.###OO##..OOO##.
 -    ..O#.O..O..O.#O##O##.###
 -    #.#..##.########..#..##.
 -    #.#####..#.#...##..#....
 -    #....##..#.#########..##
 -    #...#.....#..##...###.##
 -    #..###....##.#...##.##.#
 -
 - Determine how rough the waters are in the sea monsters' habitat by counting
 - the number of # that are not part of a sea monster. In the above example, the
 - habitat's water roughness is 273.
 -
 - How many # are not part of a sea monster?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Array as A

main :: IO ()
main = defaultMain parseInput handleInput

data Tile = Tile
    { tileId    :: !Int
    , tileArray :: ![[Char]]
    , tileUp    :: !String
    , tileRight :: !String
    , tileDown  :: !String
    , tileLeft  :: !String
    }
  deriving (Show, Eq, Ord)

type Arrangement = [((Int, Int), Tile)]
type Image = A.Array (Int, Int) Char

handleInput :: [Tile] -> IO ()
handleInput = print . minimum . map (roughness . toImage) . findArrangements

roughness :: Image -> Int
roughness image = hashes - 15*monsters
  where
    hashes = length . filter (== '#') . A.elems $ image
    monsters = countMonsters image

countMonsters :: Image -> Int
countMonsters image = length . filter p $ positions
  where
    (_, (xb, yb)) = A.bounds image
    positions = (,) <$> [0..xb] <*> [0..yb]

    p (x, y) =
        isHash (x+0, y+1) &&
        isHash (x+1, y+2) &&
        isHash (x+4, y+2) &&
        isHash (x+5, y+1) &&
        isHash (x+6, y+1) &&
        isHash (x+7, y+2) &&
        isHash (x+10, y+2) &&
        isHash (x+11, y+1) &&
        isHash (x+12, y+1) &&
        isHash (x+13, y+2) &&
        isHash (x+16, y+2) &&
        isHash (x+17, y+1) &&
        isHash (x+18, y+1) &&
        isHash (x+18, y+0) &&
        isHash (x+19, y+1)

    isHash ix = safeLook ix == Just '#'
    safeLook (x, y)
        | x >= 0 && x <= xb && y >= 0 && y <= yb = Just $ image A.! (x, y)
        | otherwise = Nothing

toImage :: Arrangement -> Image
toImage arrangement =
    foldl1 stichDown
        [foldl1 stichRight
            [Maybe.fromJust (lookup (x, y) images) | x <- [0..xb]] | y <- [0..yb]]
  where
    (xb, yb) = maximum . map fst $ arrangement
    images = map (fmap tileToImage) arrangement

tileToImage :: Tile -> Image
tileToImage (Tile _ array _ _ _ _) = A.array bounds (map f coords)
  where
    sideLen = length array
    bounds = ((0, 0), (sideLen-1, sideLen-1))
    coords = (,) <$> [0..sideLen-1] <*> [0..sideLen-1]
    f (x, y) = ((x,y), array !! y !! x)

stichRight :: Image -> Image -> Image
stichRight i1 i2
    | y1 /= y2 = error "Cannot stich due to dimensions not matching."
    | otherwise = A.array bounds (map f coords)
  where
    (_, (x1, y1)) = A.bounds i1
    (_, (x2, y2)) = A.bounds i2
    bounds = ((0, 0), (x1+x2+1, y1))
    coords = (,) <$> [0..x1+x2+1] <*> [0..y1]
    f (x, y) = ((x, y), if x <= x1 then i1 A.! (x, y) else i2 A.! (x-(x1+1), y))

stichDown :: Image -> Image -> Image
stichDown i1 i2
    | x1 /= x2 = error "Cannot stich due to dimensions not matching."
    | otherwise = A.array bounds (map f coords)
  where
    (_, (x1, y1)) = A.bounds i1
    (_, (x2, y2)) = A.bounds i2
    bounds = ((0, 0), (x1, y1+y2+1))
    coords = (,) <$> [0..x1] <*> [0..y1+y2+1]
    f (x, y) = ((x, y), if y <= y1 then i1 A.! (x, y) else i2 A.! (x, y-(y1+1)))

showImage :: Image -> String
showImage image =
    unlines [concat [[image A.! (x, y)] | x <- [0..xb]] | y <- [0..yb]]
  where
    (_, (xb, yb)) = A.bounds image

findArrangements :: [Tile] -> [Arrangement]
findArrangements tiles = do
    let legal = concatMap legalTiles tiles
        bound = floor . (sqrt :: Double -> Double) . fromIntegral . length $ tiles
        coords = (,) <$> [0..bound-1] <*> [0..bound-1]
    (arrangement, rest) <- M.foldM matchTile ([], legal) coords
    M.guard $ null rest
    return arrangement

matchTile :: (Arrangement, [Tile]) -> (Int, Int) -> [(Arrangement, [Tile])]
matchTile (arrangement, tiles) (x, y) = do
    tile <- tiles

    M.guard $ matchAbove tile
    M.guard $ matchLeft tile

    let rest = filter ((tileId tile /=) . tileId) tiles

    return (((x, y), tile):arrangement, rest)
  where
    matchAbove t1 = case lookup (x, y-1) arrangement of
        Just t2 -> tileUp t1 == tileDown t2
        Nothing -> True
    matchLeft t1 = case lookup (x-1, y) arrangement of
        Just t2 -> tileLeft t1 == tileRight t2
        Nothing -> True

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
    { tileArray = L.transpose . reverse $ tileArray tile
    , tileUp = reverse $ tileLeft tile
    , tileRight = tileUp tile
    , tileDown = reverse $ tileRight tile
    , tileLeft = tileDown tile
    }

flipVertically :: Tile -> Tile
flipVertically tile = tile
    { tileArray = reverse $ tileArray tile
    , tileUp = tileDown tile
    , tileDown = tileUp tile
    , tileLeft = reverse . tileLeft $ tile
    , tileRight = reverse . tileRight $ tile
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
        array = map (tail . init) . tail . init $ tileLines

    return $! Tile
        { tileId = ident
        , tileArray = array
        , tileUp = up
        , tileRight = right
        , tileDown = down
        , tileLeft = left
        }
  where
    parseLine = P.many1 (P.oneOf ".#")
