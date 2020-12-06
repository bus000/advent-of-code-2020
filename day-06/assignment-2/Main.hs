{- As you finish the last group's customs declaration, you notice that you
 - misread one word in the instructions:
 -
 - You don't need to identify the questions to which anyone answered "yes"; you
 - need to identify the questions to which everyone answered "yes"!
 -
 - Using the same example as above:
 -
 - abc
 -
 - a
 - b
 - c
 -
 - ab
 - ac
 -
 - a
 - a
 - a
 - a
 -
 - b
 -
 - This list represents answers from five groups:
 -
 - * In the first group, everyone (all 1 person) answered "yes" to 3 questions:
 -   a, b, and c.
 - * In the second group, there is no question to which everyone answered "yes".
 - * In the third group, everyone answered yes to only 1 question, a. Since some
 -   people did not answer "yes" to b or c, they don't count.
 - * In the fourth group, everyone answered yes to only 1 question, a.
 - * In the fifth group, everyone (all 1 person) answered "yes" to 1 question,
 -   b.
 - * In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.
 -
 - For each group, count the number of questions to which everyone answered
 - "yes". What is the sum of those counts?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set

data Group = Group Votes [Votes] deriving (Show)

type Votes = Set.Set Vote

type Vote = Char

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Group] -> IO ()
handleInput = print . sum . map uniqueVoteNumber

uniqueVoteNumber :: Group -> Int
uniqueVoteNumber (Group votes rest) =
    Set.size $ foldr Set.intersection votes rest

parseInput :: T.Text -> Either P.ParseError [Group]
parseInput = P.parse (parseGroups <* P.eof) ""

parseGroups :: P.Parsec T.Text () [Group]
parseGroups = parseGroup `P.sepBy` P.newline

parseGroup :: P.Parsec T.Text () Group
parseGroup = Group
    <$> parseVotes <* P.newline
    <*> parseVotes `P.endBy` P.newline

parseVotes :: P.Parsec T.Text () Votes
parseVotes = Set.fromList <$> P.many1 P.letter
