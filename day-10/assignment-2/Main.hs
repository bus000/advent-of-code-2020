{- To completely determine whether you have enough adapters, you'll need to
 - figure out how many different ways they can be arranged. Every arrangement
 - needs to connect the charging outlet to your device. The previous rules about
 - when adapters can successfully connect still apply.
 -
 - The first example above (the one that starts with 16, 10, 15) supports the
 - following arrangements:
 -
 -    (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
 -    (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)
 -
 - (The charging outlet and your device's built-in adapter are shown in
 - parentheses.) Given the adapters from the first example, the total number of
 - arrangements that connect the charging outlet to your device is 8.
 -
 - The second example above (the one that starts with 28, 33, 18) has many
 - arrangements. Here are a few:
 -
 -    (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
 -    32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, (52)
 -
 -    (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
 -    32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 49, (52)
 -
 -    (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
 -    32, 33, 34, 35, 38, 39, 42, 45, 46, 48, 49, (52)
 -
 -    (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
 -    32, 33, 34, 35, 38, 39, 42, 45, 46, 49, (52)
 -
 -    (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
 -    32, 33, 34, 35, 38, 39, 42, 45, 47, 48, 49, (52)
 -
 -    (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
 -    46, 48, 49, (52)
 -
 -    (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
 -    46, 49, (52)
 -
 -    (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
 -    47, 48, 49, (52)
 -
 -    (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
 -    47, 49, (52)
 -
 -    (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
 -    48, 49, (52)
 -
 - In total, this set of adapters can connect the charging outlet to your device
 - in 19208 distinct arrangements.
 -
 - You glance back down at your bag and try to remember why you brought so many
 - adapters; there must be more than a trillion valid ways to arrange them!
 - Surely, there must be an efficient way to count the arrangements.
 -
 - What is the total number of distinct ways you can arrange the adapters to
 - connect the charging outlet to your device?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Ord (Down(..))

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Int] -> IO ()
handleInput = print . adapterPathNumbers

adapterPathNumbers :: [Int] -> Int
adapterPathNumbers adapters = Map.findWithDefault 0 0 pathNumbers
  where
    adapters' = 0:adapters
    sorted = L.sortOn Down adapters'
    target = head sorted + 3
    initialMap = Map.singleton target 1
    pathNumbers = foldl f initialMap sorted

f :: Map.Map Int Int -> Int -> Map.Map Int Int
f cache x = Map.insert x result cache
  where
    r1 = Map.findWithDefault 0 (x + 1) cache
    r2 = Map.findWithDefault 0 (x + 2) cache
    r3 = Map.findWithDefault 0 (x + 3) cache
    result = r1 + r2 + r3

parseInput :: T.Text -> Either P.ParseError [Int]
parseInput = P.parse (parseInts <* P.eof) ""

parseInts :: P.Parsec T.Text () [Int]
parseInts = P.int `P.endBy` P.newline
