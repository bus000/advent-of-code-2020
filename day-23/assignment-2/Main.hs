{- Due to what you can only assume is a mistranslation (you're not exactly
 - fluent in Crab), you are quite surprised when the crab starts arranging many
 - cups in a circle on your raft - one million (1000000) in total.
 -
 - Your labeling is still correct for the first few cups; after that, the
 - remaining cups are just numbered in an increasing fashion starting from the
 - number after the highest number in your list and proceeding one by one until
 - one million is reached. (For example, if your labeling were 54321, the cups
 - would be numbered 5, 4, 3, 2, 1, and then start counting up from 6 until one
 - million is reached.) In this way, every number from one through one million
 - is used exactly once.
 -
 - After discovering where you made the mistake in translating Crab Numbers, you
 - realize the small crab isn't going to do merely 100 moves; the crab is going
 - to do ten million (10000000) moves!
 -
 - The crab is going to hide your stars - one each - under the two cups that
 - will end up immediately clockwise of cup 1. You can have them if you predict
 - what the labels on those cups will be when the crab is finished.
 -
 - In the above example (389125467), this would be 934001 and then 159792;
 - multiplying these together produces 149245887792.
 -
 - Determine which two cups will end up immediately clockwise of cup 1. What do
 - you get if you multiply their labels together?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Maybe as Maybe
import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Int] -> IO ()
handleInput = print . simulateGame 10000000

data GameState = GameState
    { gsCurrent :: !Int
    , gsList    :: !(Map.Map Int Int)
    } deriving (Show, Eq, Ord)

simulateGame :: Int -> [Int] -> Int
simulateGame steps cups
    = product
    . drop 1
    . take 3
    . toList
    . (\gs -> gs { gsCurrent = 1 })
    . head
    . drop steps
    . iterate step
    $ initialState
  where
    initialPosition = head cups
    initialList = Map.fromList $ (last cups, head cups):zip cups (tail cups)
    initialState = GameState initialPosition initialList

step :: GameState -> GameState
step gs@(GameState current list) = gs { gsCurrent = current', gsList = list' }
  where
    [first, second, third, fourth] = drop 1 . take 5 . toList $ gs
    target = head . filter (`notElem` [first, second, third])
       $ [current-1, current-2..1] ++ [1000000, 1000000-1..1]
    afterTarget = lookupGame gs target

    list' = insertMany list
        [(current, fourth), (target, first), (third, afterTarget)]
    current' = fourth

lookupGame :: GameState -> Int -> Int
lookupGame gs x = Maybe.fromJust . Map.lookup x $ gsList gs

toList :: GameState -> [Int]
toList gs = current:L.unfoldr f current
  where
    current = gsCurrent gs
    f x = do
        x' <- Just $ lookupGame gs x
        M.guard $ x' /= current
        return (x', x')

insertMany :: Ord k => Map.Map k a -> [(k, a)] -> Map.Map k a
insertMany = foldr (uncurry Map.insert)

parseInput :: T.Text -> Either P.ParseError [Int]
parseInput = P.parse (parseList <* P.eof) ""

parseList :: P.Parsec T.Text () [Int]
parseList = (\x -> map fromD x ++ [10..1000000]) <$> P.many P.digit <* P.newline
  where
    fromD '0' = 0
    fromD '1' = 1
    fromD '2' = 2
    fromD '3' = 3
    fromD '4' = 4
    fromD '5' = 5
    fromD '6' = 6
    fromD '7' = 7
    fromD '8' = 8
    fromD '9' = 9
    fromD _ = error "Impossible case"
