{- --- Day 12: Rain Risk ---
 -
 - Your ferry made decent progress toward the island, but the storm came in
 - faster than anyone expected. The ferry needs to take evasive actions!
 -
 - Unfortunately, the ship's navigation computer seems to be malfunctioning;
 - rather than giving a route directly to safety, it produced extremely
 - circuitous instructions. When the captain uses the PA system to ask if anyone
 - can help, you quickly volunteer.
 -
 - The navigation instructions (your puzzle input) consists of a sequence of
 - single-character actions paired with integer input values. After staring at
 - them for a few minutes, you work out what they probably mean:
 -
 - * Action N means to move north by the given value.
 - * Action S means to move south by the given value.
 - * Action E means to move east by the given value.
 - * Action W means to move west by the given value.
 - * Action L means to turn left the given number of degrees.
 - * Action R means to turn right the given number of degrees.
 - * Action F means to move forward by the given value in the direction the ship
 -   is currently facing.
 -
 - The ship starts by facing east. Only the L and R actions change the direction
 - the ship is facing. (That is, if the ship is facing east and the next
 - instruction is N10, the ship would move north 10 units, but would still move
 - east if the following action were F.)
 -
 - For example:
 -
 -    F10
 -    N3
 -    F7
 -    R90
 -    F11
 -
 - These instructions would be handled as follows:
 -
 - * F10 would move the ship 10 units east (because the ship starts by facing
 -   east) to east 10, north 0.
 - * N3 would move the ship 3 units north to east 10, north 3.
 - * F7 would move the ship another 7 units east (because the ship is still
 -   facing east) to east 17, north 3.
 - * R90 would cause the ship to turn right by 90 degrees and face south; it
 -   remains at east 17, north 3.
 - * F11 would move the ship 11 units south to east 17, south 8.
 -
 - At the end of these instructions, the ship's Manhattan distance (sum of the
 - absolute values of its east/west position and its north/south position) from
 - its starting position is 17 + 8 = 25.
 -
 - Figure out where the navigation instructions lead. What is the Manhattan
 - distance between that location and the ship's starting position?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data Direction = North | South | East | West
    deriving (Show, Eq, Ord)

data Action
    = Move !Direction !Int
    | TurnLeft
    | TurnRight
    | TurnReverse
    | Forward !Int
    deriving (Show, Eq, Ord)

type NavigationInstruction = [Action]

data Ferry = Ferry !Direction !(Int, Int) deriving (Show, Eq, Ord)

handleInput :: NavigationInstruction -> IO ()
handleInput = print . (\(Ferry _ pos) -> manhattanDistance pos) . takeActions

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

takeActions :: NavigationInstruction -> Ferry
takeActions = foldl takeAction ferry
  where
    ferry = Ferry East (0, 0)

takeAction :: Ferry -> Action -> Ferry
takeAction (Ferry direction (x, y)) (Move North d) = Ferry direction (x, y + d)
takeAction (Ferry direction (x, y)) (Move South d) = Ferry direction (x, y - d)
takeAction (Ferry direction (x, y)) (Move East d) = Ferry direction (x + d, y)
takeAction (Ferry direction (x, y)) (Move West d) = Ferry direction (x - d, y)

takeAction (Ferry North position) TurnLeft = Ferry West position
takeAction (Ferry South position) TurnLeft = Ferry East position
takeAction (Ferry East position) TurnLeft = Ferry North position
takeAction (Ferry West position) TurnLeft = Ferry South position

takeAction (Ferry North position) TurnRight = Ferry East position
takeAction (Ferry South position) TurnRight = Ferry West position
takeAction (Ferry East position) TurnRight = Ferry South position
takeAction (Ferry West position) TurnRight = Ferry North position

takeAction (Ferry North position) TurnReverse = Ferry South position
takeAction (Ferry South position) TurnReverse = Ferry North position
takeAction (Ferry East position) TurnReverse = Ferry West position
takeAction (Ferry West position) TurnReverse = Ferry East position

takeAction ferry@(Ferry direction _) (Forward d) =
    takeAction ferry (Move direction d)

parseInput :: T.Text -> Either P.ParseError NavigationInstruction
parseInput = P.parse (parseActions <* P.eof) ""

parseActions :: P.Parsec T.Text () NavigationInstruction
parseActions = parseAction `P.endBy` P.newline

parseAction :: P.Parsec T.Text () Action
parseAction = P.choice
    [parseMove, parseLeft, parseRight, parseReverse, parseForward]
  where
    parseMove = Move <$> parseDirection <*> P.int
    parseLeft = P.choice
        [ P.try $ P.string "L90" *> pure TurnLeft
        , P.try $ P.string "R270" *> pure TurnLeft
        ]
    parseRight = P.choice
        [ P.try $ P.string "R90" *> pure TurnRight
        , P.try $ P.string "L270" *> pure TurnRight
        ]
    parseReverse = P.choice
        [ P.try $ P.string "L180" *> pure TurnReverse
        , P.try $ P.string "R180" *> pure TurnReverse
        ]
    parseForward = Forward <$> (P.char 'F' *> P.int)
    parseDirection = P.choice
        [ P.char 'N' *> pure North
        , P.char 'S' *> pure South
        , P.char 'E' *> pure East
        , P.char 'W' *> pure West
        ]
