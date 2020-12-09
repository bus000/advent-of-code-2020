{- The final step in breaking the XMAS encryption relies on the invalid number
 - you just found: you must find a contiguous set of at least two numbers in
 - your list which sum to the invalid number from step 1.
 -
 - Again consider the above example:
 -
 -    35
 -    20
 -    15
 -    25
 -    47
 -    40
 -    62
 -    55
 -    65
 -    95
 -    102
 -    117
 -    150
 -    182
 -    127
 -    219
 -    299
 -    277
 -    309
 -    576
 -
 - In this list, adding up all of the numbers from 15 through 40 produces the
 - invalid number from step 1, 127. (Of course, the contiguous set of numbers in
 - your actual list might be much longer.)
 -
 - To find the encryption weakness, add together the smallest and largest number
 - in this contiguous range; in this example, these are 15 and 47, producing 62.
 -
 - What is the encryption weakness in your XMAS-encrypted list of numbers?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Integer] -> IO ()
handleInput xs = case findWeakness xs of
    Just weakness -> print weakness
    Nothing -> putStrLn "Could not find any weakness in Xmas ciphercode."

findWeakness :: [Integer] -> Maybe Integer
findWeakness xs = do
    weakness <- checkXmas 25 xs
    contiguous <- findContiguousSum weakness xs
    return $! minimum contiguous + maximum contiguous

findContiguousSum :: Integer -> [Integer] -> Maybe [Integer]
findContiguousSum target xs = headMay . filter targetSum $ contiguousNumbers
  where
    contiguousNumbers = concatMap L.inits . L.tails $ xs

    headMay [] = Nothing
    headMay (y:_) = Just y

    targetSum ys = sum ys == target

checkXmas :: Int -> [Integer] -> Maybe Integer
checkXmas size xs = checkXmas' size preample cipher
  where
    preample = sumNext size xs
    cipher = drop size xs

checkXmas' :: Int -> [Integer] -> [Integer] -> Maybe Integer
checkXmas' _ _ [] = Nothing
checkXmas' size possible (x:xs)
    | x `elem` sums = checkXmas' size possible' xs
    | otherwise = Just x
  where
    sums = take (size * size) possible
    possible' = drop size possible

sumNext :: Num a => Int -> [a] -> [a]
sumNext size l = do
    (x:xs) <- map (take $ size + 1) . L.tails $ l
    y <- xs
    return $! x + y

parseInput :: T.Text -> Either P.ParseError [Integer]
parseInput = P.parse (parseInts <* P.eof) ""

parseInts :: P.Parsec T.Text () [Integer]
parseInts = P.int `P.endBy` P.newline
