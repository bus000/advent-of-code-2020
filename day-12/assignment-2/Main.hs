{- Before you can give the destination to the captain, you realize that the
 - actual action meanings were printed on the back of the instructions the whole
 - time.
 -
 - Almost all of the actions indicate how to move a waypoint which is relative
 - to the ship's position:
 -
 - * Action N means to move the waypoint north by the given value.
 - * Action S means to move the waypoint south by the given value.
 - * Action E means to move the waypoint east by the given value.
 - * Action W means to move the waypoint west by the given value.
 - * Action L means to rotate the waypoint around the ship left
 -   (counter-clockwise) the given number of degrees.
 - * Action R means to rotate the waypoint around the ship right (clockwise) the
 -   given number of degrees.
 - * Action F means to move forward to the waypoint a number of times equal to
 -   the given value.
 -
 - The waypoint starts 10 units east and 1 unit north relative to the ship. The
 - waypoint is relative to the ship; that is, if the ship moves, the waypoint
 - moves with it.
 -
 - For example, using the same instructions as above:
 -
 - * F10 moves the ship to the waypoint 10 times (a total of 100 units east and
 -   10 units north), leaving the ship at east 100, north 10. The waypoint stays
 -   10 units east and 1 unit north of the ship.
 - * N3 moves the waypoint 3 units north to 10 units east and 4 units north of
 -   the ship. The ship remains at east 100, north 10.
 - * F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28
 -   units north), leaving the ship at east 170, north 38. The waypoint stays 10
 -   units east and 4 units north of the ship.
 - * R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to
 -   4 units east and 10 units south of the ship. The ship remains at east 170,
 -   north 38.
 - * F11 moves the ship to the waypoint 11 times (a total of 44 units east and
 -   110 units south), leaving the ship at east 214, south 72. The waypoint
 -   stays 4 units east and 10 units south of the ship.
 -
 - After these operations, the ship's Manhattan distance from its starting
 - position is 214 + 72 = 286.
 -
 - Figure out where the navigation instructions actually lead. What is the
 - Manhattan distance between that location and the ship's starting position?
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

data Ferry = Ferry
    { _position :: !(Int, Int)
    , _velocity :: !(Int, Int)
    } deriving (Show, Eq, Ord)

handleInput :: NavigationInstruction -> IO ()
handleInput = print . manhattanDistance . _position . takeActions

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

takeActions :: NavigationInstruction -> Ferry
takeActions = foldl takeAction ferry
  where
    ferry = Ferry (0, 0) (10, 1)

takeAction :: Ferry -> Action -> Ferry
takeAction ferry@(Ferry _ (vx, vy)) (Move North d) =
    ferry { _velocity = (vx, vy + d) }
takeAction ferry@(Ferry _ (vx, vy)) (Move South d) =
    ferry { _velocity = (vx, vy - d) }
takeAction ferry@(Ferry _ (vx, vy)) (Move East d) =
    ferry { _velocity = (vx + d, vy) }
takeAction ferry@(Ferry _ (vx, vy)) (Move West d) =
    ferry { _velocity = (vx - d, vy) }
takeAction ferry@(Ferry _ (vx, vy)) TurnRight = ferry { _velocity = (vy, -vx) }
takeAction ferry TurnLeft =
    flip takeAction TurnRight . flip takeAction TurnRight . flip takeAction TurnRight $ ferry
takeAction ferry TurnReverse =
    flip takeAction TurnRight . flip takeAction TurnRight $ ferry
takeAction ferry@(Ferry (x, y) (vx, vy)) (Forward d) =
    ferry { _position = (x + (d * vx), y + (d * vy)) }

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
