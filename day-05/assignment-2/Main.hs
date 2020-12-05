{- Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
 -
 - It's a completely full flight, so your seat should be the only missing
 - boarding pass in your list. However, there's a catch: some of the seats at
 - the very front and back of the plane don't exist on this aircraft, so they'll
 - be missing from your list as well.
 -
 - Your seat wasn't at the very front or back, though; the seats with IDs +1 and
 - -1 from yours will be in your list.
 -
 - What is the ID of your seat?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified System.Exit as S
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data Direction = DOWN | UP
    deriving (Show, Eq, Ord)

data RowDefinition = RowDefinition Direction Direction Direction Direction
    Direction Direction Direction
    deriving (Show, Eq, Ord)

data ColumnDefinition = ColumnDefinition Direction Direction Direction
    deriving (Show, Eq, Ord)

data Seating = Seating RowDefinition ColumnDefinition
    deriving (Show, Eq, Ord)

instance Enum Direction where
    toEnum 0 = DOWN
    toEnum 1 = UP
    toEnum _ = error "No corresponding enum"

    fromEnum DOWN = 0
    fromEnum UP = 1

handleInput :: [Seating] -> IO ()
handleInput [] = putStrLn "Seating list empty" >> S.exitFailure
handleInput seatings = case findFirstMissingSeatId seatings of
    Just missingSeat -> print missingSeat
    Nothing -> putStrLn "There is no missing seat" >> S.exitFailure

findFirstMissingSeatId :: [Seating] -> Maybe Int
findFirstMissingSeatId seatings = fmap ((+) 1 . fst) $ L.find jump idsAndJumps
  where
    ids = L.sort $ map computeSeatId seatings
    jumps = zipWith (-) (tail ids) ids
    idsAndJumps = zip ids jumps
    jump (_, j) = j > 1

computeSeatId :: Seating -> Int
computeSeatId (Seating row column) = rowNumber * 8 + columnNumber
  where
    rowNumber = computeRowNumber row
    columnNumber = computeColumnNumber column

computeRowNumber :: RowDefinition -> Int
computeRowNumber (RowDefinition d1 d2 d3 d4 d5 d6 d7) = sum values
  where
    rowValues = [64, 32, 16, 8, 4, 2, 1]
    upOrDown = map fromEnum [d1, d2, d3, d4, d5, d6, d7]
    values = zipWith (*) rowValues upOrDown

computeColumnNumber :: ColumnDefinition -> Int
computeColumnNumber (ColumnDefinition d1 d2 d3) = sum values
  where
    columnValues = [4, 2, 1]
    upOrDown = map fromEnum [d1, d2, d3]
    values = zipWith (*) columnValues upOrDown

parseInput :: T.Text -> Either P.ParseError [Seating]
parseInput = P.parse (parseSeatings <* P.eof) ""

parseSeatings :: P.Parsec T.Text () [Seating]
parseSeatings = P.many parseSeating

parseSeating :: P.Parsec T.Text () Seating
parseSeating = Seating <$> parseRow <*> parseColumn <* P.newline

parseRow :: P.Parsec T.Text () RowDefinition
parseRow = fromString <$> P.count 7
    (P.choice [P.char 'F' *> pure DOWN, P.char 'B' *> pure UP])
  where
    fromString [c1, c2, c3, c4, c5, c6, c7] = RowDefinition c1 c2 c3 c4 c5 c6 c7
    fromString _ = error "Impossible case"

parseColumn :: P.Parsec T.Text () ColumnDefinition
parseColumn = fromString <$> P.count 3
    (P.choice [P.char 'L' *> pure DOWN, P.char 'R' *> pure UP])
  where
    fromString [c1, c2, c3] = ColumnDefinition c1 c2 c3
    fromString _ = error "Impossible case"
