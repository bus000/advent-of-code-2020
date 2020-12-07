{- It's getting pretty expensive to fly these days - not because of ticket
 - prices, but because of the ridiculous number of bags you need to buy!
 -
 - Consider again your shiny gold bag and the rules from the above example:
 -
 - * faded blue bags contain 0 other bags.
 - * dotted black bags contain 0 other bags.
 - * vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted
 -   black bags.
 - * dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black
 -   bags.
 -
 - So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags
 - within it) plus 2 vibrant plum bags (and the 11 bags within each of those):
 - 1 + 1*7 + 2 + 2*11 = 32 bags!
 -
 - Of course, the actual rules have a small chance of going several levels
 - deeper than this example; be sure to count all of the bags, even if the
 - nesting becomes topologically impractical!
 -
 - Here's another example:
 -
 - * shiny gold bags contain 2 dark red bags.
 - * dark red bags contain 2 dark orange bags.
 - * dark orange bags contain 2 dark yellow bags.
 - * dark yellow bags contain 2 dark green bags.
 - * dark green bags contain 2 dark blue bags.
 - * dark blue bags contain 2 dark violet bags.
 - * dark violet bags contain no other bags.
 -
 - In this example, a single shiny gold bag must contain 126 other bags.
 -
 - How many individual bags are required inside your single shiny gold bag?
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map

data BagId = BagId !T.Text deriving (Show, Eq, Ord)

data Regulation = Regulation
    -- Bag that is regulated in this regulation.
    { _bag             :: !BagId
    -- The required content of the bag.
    , _requiredContent :: ![RequiredContent]
    } deriving (Show, Eq, Ord)

data RequiredContent = RequiredContent !Int !BagId deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Regulation] -> IO ()
handleInput = print . countRequired (BagId "shiny gold")

countRequired :: BagId -> [Regulation] -> Integer
countRequired bagId regulations = countBagsInTree rules bagId - 1
  where
    rules = initialSet regulations

countBagsInTree :: Map.Map BagId [RequiredContent] -> BagId -> Integer
countBagsInTree rules bagId = sum subBagCounts + 1
  where
    required = rules Map.! bagId
    subBagCounts = map f . subBagCount $ required
    f (bag, n) = n * countBagsInTree rules bag

subBagCount :: [RequiredContent] -> [(BagId, Integer)]
subBagCount = map (\(RequiredContent n bag) -> (bag, fromIntegral n))

initialSet :: [Regulation] -> Map.Map BagId [RequiredContent]
initialSet = Map.fromList . map requiredBags
  where
    requiredBags (Regulation bagId required) = (bagId, required)

parseInput :: T.Text -> Either P.ParseError [Regulation]
parseInput = P.parse (parseRegulations <* P.eof) ""

parseRegulations :: P.Parsec T.Text () [Regulation]
parseRegulations = parseRegulation `P.endBy` P.newline

parseRegulation :: P.Parsec T.Text () Regulation
parseRegulation = Regulation
    <$> (parseBagId <* P.string " contain ") <*> parseRequiredContents

parseRequiredContents :: P.Parsec T.Text () [RequiredContent]
parseRequiredContents =
    P.choice [P.try noRequiredContent, requiredContent] <* P.char '.'
  where
    noRequiredContent = P.string "no other bags" *> pure []
    requiredContent = parseRequiredContent `P.sepBy` P.string ", "

parseRequiredContent :: P.Parsec T.Text () RequiredContent
parseRequiredContent = RequiredContent <$> (P.int <* P.space) <*> parseBagId

parseBagId :: P.Parsec T.Text () BagId
parseBagId = BagId . T.pack
    <$> P.manyTill P.anyChar (P.try $ P.string " bag" <* P.optional (P.char 's'))
