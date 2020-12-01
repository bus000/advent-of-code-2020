{- The Elves in accounting are thankful for your help; one of them even offers
 - you a starfish coin they had left over from a past vacation. They offer you a
 - second one if you can find three numbers in your expense report that meet the
 - same criteria.
 -
 - Using the above example again, the three entries that sum to 2020 are 979,
 - 366, and 675. Multiplying them together produces the answer, 241861950.
 -
 - In your expense report, what is the product of the three entries that sum to
 - 2020?
 -}
module Main where

import AdventOfCode
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Text as T

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Int] -> IO ()
handleInput
    = print
    . (\(x, y, z) -> x * y * z)
    . head
    . filter (\(x, y, z) -> x + y + z == 2020)
    . triplets

triplets :: [a] -> [(a, a, a)]
triplets ls = do
    x <- ls
    y <- ls
    z <- ls
    return (x, y, z)

parseInput :: T.Text -> Either P.ParseError [Int]
parseInput = P.parse (parseLines <* P.eof) ""

parseLines :: P.Parsec T.Text () [Int]
parseLines = P.many (P.int <* P.char '\n')
