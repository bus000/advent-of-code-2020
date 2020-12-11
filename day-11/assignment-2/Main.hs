{- As soon as people start to arrive, you realize your mistake. People don't
 - just care about adjacent seats - they care about the first seat they can see
 - in each of those eight directions!
 -
 - Now, instead of considering just the eight immediately adjacent seats,
 - consider the first seat in each of those eight directions. For example, the
 - empty seat below would see eight occupied seats:
 -
 -    .......#.
 -    ...#.....
 -    .#.......
 -    .........
 -    ..#L....#
 -    ....#....
 -    .........
 -    #........
 -    ...#.....
 -
 - The leftmost empty seat below would only see one empty seat, but cannot see
 - any of the occupied ones:
 -
 -    .............
 -    .L.L.#.#.#.#.
 -    .............
 -
 - The empty seat below would see no occupied seats:
 -
 -    .##.##.
 -    #.#.#.#
 -    ##...##
 -    ...L...
 -    ##...##
 -    #.#.#.#
 -    .##.##.
 -
 - Also, people seem to be more tolerant than you expected: it now takes five or
 - more visible occupied seats for an occupied seat to become empty (rather than
 - four or more from the previous rules). The other rules still apply: empty
 - seats that see no occupied seats become occupied, seats matching no rule
 - don't change, and floor never changes.
 -
 - Given the same starting layout as above, these new rules cause the seating
 - area to shift around as follows:
 -
 -    L.LL.LL.LL
 -    LLLLLLL.LL
 -    L.L.L..L..
 -    LLLL.LL.LL
 -    L.LL.LL.LL
 -    L.LLLLL.LL
 -    ..L.L.....
 -    LLLLLLLLLL
 -    L.LLLLLL.L
 -    L.LLLLL.LL
 -
 -    #.##.##.##
 -    #######.##
 -    #.#.#..#..
 -    ####.##.##
 -    #.##.##.##
 -    #.#####.##
 -    ..#.#.....
 -    ##########
 -    #.######.#
 -    #.#####.##
 -
 -    #.LL.LL.L#
 -    #LLLLLL.LL
 -    L.L.L..L..
 -    LLLL.LL.LL
 -    L.LL.LL.LL
 -    L.LLLLL.LL
 -    ..L.L.....
 -    LLLLLLLLL#
 -    #.LLLLLL.L
 -    #.LLLLL.L#
 -
 -    #.L#.##.L#
 -    #L#####.LL
 -    L.#.#..#..
 -    ##L#.##.##
 -    #.##.#L.##
 -    #.#####.#L
 -    ..#.#.....
 -    LLL####LL#
 -    #.L#####.L
 -    #.L####.L#
 -
 -    #.L#.L#.L#
 -    #LLLLLL.LL
 -    L.L.L..#..
 -    ##LL.LL.L#
 -    L.LL.LL.L#
 -    #.LLLLL.LL
 -    ..L.L.....
 -    LLLLLLLLL#
 -    #.LLLLL#.L
 -    #.L#LL#.L#
 -
 -    #.L#.L#.L#
 -    #LLLLLL.LL
 -    L.L.L..#..
 -    ##L#.#L.L#
 -    L.L#.#L.L#
 -    #.L####.LL
 -    ..#.#.....
 -    LLL###LLL#
 -    #.LLLLL#.L
 -    #.L#LL#.L#
 -
 -    #.L#.L#.L#
 -    #LLLLLL.LL
 -    L.L.L..#..
 -    ##L#.#L.L#
 -    L.L#.LL.L#
 -    #.LLLL#.LL
 -    ..#.L.....
 -    LLL###LLL#
 -    #.LLLLL#.L
 -    #.L#LL#.L#
 -
 - Again, at this point, people stop shifting around and the seating area
 - reaches equilibrium. Once this occurs, you count 26 occupied seats.
 -
 - Given the new visibility method and the rule change for occupied seats
 - becoming empty, once equilibrium is reached, how many seats end up occupied?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Array as A
import qualified Control.Monad as C
import qualified Data.Maybe as Maybe
import Util (converge)

main :: IO ()
main = defaultMain parseInput handleInput

type SeatLayout = A.Array (Int, Int) Tile

data Tile = Floor | Empty | Filled deriving (Show, Eq, Ord)

handleInput :: SeatLayout -> IO ()
handleInput = print . countOccupied . converge step

step :: SeatLayout -> SeatLayout
step seating = A.listArray bounds . map (mapTile seating) . A.indices $ seating
  where
    bounds = A.bounds seating

mapTile :: SeatLayout -> (Int, Int) -> Tile
mapTile seating index = case seating A.! index of
    Empty -> if occupied == 0 then Filled else Empty
    Filled -> if occupied >= 5 then Empty else Filled
    Floor -> Floor
  where
    occupied = visiblyOccupied seating index

visiblyOccupied :: SeatLayout -> (Int, Int) -> Int
visiblyOccupied seating index = length . filter (==Filled) $ visible seating index

visible :: SeatLayout -> (Int, Int) -> [Tile]
visible seating (x, y) =
    Maybe.catMaybes [north, south, east, west, northeast, northwest, southeast,
        southwest]
  where
    north = firstSeat . map (\d -> (x, y+d)) $ [1..]
    south = firstSeat . map (\d -> (x, y-d)) $ [1..]
    east = firstSeat . map (\d -> (x+d, y)) $ [1..]
    west = firstSeat . map (\d -> (x-d, y)) $ [1..]
    northeast = firstSeat . map (\d -> (x+d, y+d)) $ [1..]
    northwest = firstSeat . map (\d -> (x-d, y+d)) $ [1..]
    southeast = firstSeat . map (\d -> (x+d, y-d)) $ [1..]
    southwest = firstSeat . map (\d -> (x-d, y-d)) $ [1..]

    firstSeat
        = Maybe.listToMaybe
        . filter isSeat
        . map Maybe.fromJust
        . takeWhile Maybe.isJust
        . map (lookupSeat seating)

lookupSeat :: SeatLayout -> (Int, Int) -> Maybe Tile
lookupSeat seating index@(x, y)
    | x >= 0 && x <= xBound && y >= 0 && y <= yBound = Just $ seating A.! index
    | otherwise = Nothing
  where
    (_, (xBound, yBound)) = A.bounds seating

countOccupied :: SeatLayout -> Int
countOccupied = length . filter (==Filled) . A.elems

isSeat :: Tile -> Bool
isSeat Empty = True
isSeat Filled = True
isSeat Floor = False

justs :: [Maybe x] -> [x]
justs [] = []
justs (Nothing:xs) = justs xs
justs (Just x:xs) = x:justs xs

parseInput :: T.Text -> Either P.ParseError SeatLayout
parseInput = P.parse (parseSeatLayout <* P.eof) ""

parseSeatLayout :: P.Parsec T.Text () SeatLayout
parseSeatLayout = do
    seatLines <- parseSeatLine `P.endBy` P.newline

    -- Check rows.
    let rows = length seatLines
    C.guard $ rows > 0

    -- Check columns.
    let columns = length . head $ seatLines
    C.guard . all (==columns) . map length $ seatLines

    let bounds = ((0, 0), (rows-1, columns-1))

    return $! A.listArray bounds (concat seatLines)

parseSeatLine :: P.Parsec T.Text () [Tile]
parseSeatLine = P.many $ P.choice tiles
  where
    tiles =
        [ P.char 'L' *> pure Empty
        , P.char '.' *> pure Floor
        , P.char '#' *> pure Filled
        ]
