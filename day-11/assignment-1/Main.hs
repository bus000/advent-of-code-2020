{- --- Day 11: Seating System ---
 -
 - Your plane lands with plenty of time to spare. The final leg of your journey
 - is a ferry that goes directly to the tropical island where you can finally
 - start your vacation. As you reach the waiting area to board the ferry, you
 - realize you're so early, nobody else has even arrived yet!
 -
 - By modeling the process people use to choose (or abandon) their seat in the
 - waiting area, you're pretty sure you can predict the best place to sit. You
 - make a quick map of the seat layout (your puzzle input).
 -
 - The seat layout fits neatly on a grid. Each position is either floor (.), an
 - empty seat (L), or an occupied seat (#). For example, the initial seat layout
 - might look like this:
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
 - Now, you just need to model the people who will be arriving shortly.
 - Fortunately, people are entirely predictable and always follow a simple set
 - of rules. All decisions are based on the number of occupied seats adjacent to
 - a given seat (one of the eight positions immediately up, down, left, right,
 - or diagonal from the seat). The following rules are applied to every seat
 - simultaneously:
 -
 - * If a seat is empty (L) and there are no occupied seats adjacent to it, the
 -   seat becomes occupied.
 - * If a seat is occupied (#) and four or more seats adjacent to it are also
 -   occupied, the seat becomes empty.
 - * Otherwise, the seat's state does not change.
 -
 - Floor (.) never changes; seats don't move, and nobody sits on the floor.
 -
 - After one round of these rules, every seat in the example layout becomes
 - occupied:
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
 - After a second round, the seats with four or more occupied adjacent seats
 - become empty again:
 -
 -    #.LL.L#.##
 -    #LLLLLL.L#
 -    L.L.L..L..
 -    #LLL.LL.L#
 -    #.LL.LL.LL
 -    #.LLLL#.##
 -    ..L.L.....
 -    #LLLLLLLL#
 -    #.LLLLLL.L
 -    #.#LLLL.##
 -
 - This process continues for three more rounds:
 -
 -    #.##.L#.##
 -    #L###LL.L#
 -    L.#.#..#..
 -    #L##.##.L#
 -    #.##.LL.LL
 -    #.###L#.##
 -    ..#.#.....
 -    #L######L#
 -    #.LL###L.L
 -    #.#L###.##
 -
 -    #.#L.L#.##
 -    #LLL#LL.L#
 -    L.L.L..#..
 -    #LLL.##.L#
 -    #.LL.LL.LL
 -    #.LL#L#.##
 -    ..L.L.....
 -    #L#LLLL#L#
 -    #.LLLLLL.L
 -    #.#L#L#.##
 -
 -    #.#L.L#.##
 -    #LLL#LL.L#
 -    L.#.L..#..
 -    #L##.##.L#
 -    #.#L.LL.LL
 -    #.#L#L#.##
 -    ..L.L.....
 -    #L#L##L#L#
 -    #.LLLLLL.L
 -    #.#L#L#.##
 -
 - At this point, something interesting happens: the chaos stabilizes and
 - further applications of these rules cause no seats to change state! Once
 - people stop moving around, you count 37 occupied seats.
 -
 - Simulate your seating area by applying the seating rules repeatedly until no
 - seats change state. How many seats end up occupied?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Array as A
import qualified Control.Monad as C
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
    Filled -> if occupied >= 4 then Empty else Filled
    Floor -> Floor
  where
    occupied = occupiedNeighbours seating index

occupiedNeighbours :: SeatLayout -> (Int, Int) -> Int
occupiedNeighbours seating index = length . filter (==Filled) $ neighbours seating index

neighbours :: SeatLayout -> (Int, Int) -> [Tile]
neighbours seating (x, y) = justs . map (lookupSeat seating) $ indices
  where
    indices =
        [ (x-1, y-1), (x+0, y-1), (x+1, y-1)
        , (x-1, y+0),             (x+1, y+0)
        , (x-1, y+1), (x+0, y+1), (x+1, y+1)
        ]

lookupSeat :: SeatLayout -> (Int, Int) -> Maybe Tile
lookupSeat seating index@(x, y)
    | x >= 0 && x <= xBound && y >= 0 && y <= yBound = Just $ seating A.! index
    | otherwise = Nothing
  where
    (_, (xBound, yBound)) = A.bounds seating

countOccupied :: SeatLayout -> Int
countOccupied = length . filter (==Filled) . A.elems

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
