{- --- Day 7: Handy Haversacks ---
 -
 - You land at the regional airport in time for your next flight. In fact, it
 - looks like you'll even have time to grab some food: all flights are currently
 - delayed due to issues in luggage processing.
 -
 - Due to recent aviation regulations, many rules (your puzzle input) are being
 - enforced about bags and their contents; bags must be color-coded and must
 - contain specific quantities of other color-coded bags. Apparently, nobody
 - responsible for these regulations considered how long they would take to
 - enforce!
 -
 - For example, consider the following rules:
 -
 - * light red bags contain 1 bright white bag, 2 muted yellow bags.
 - * dark orange bags contain 3 bright white bags, 4 muted yellow bags.
 - * bright white bags contain 1 shiny gold bag.
 - * muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
 - * shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
 - * dark olive bags contain 3 faded blue bags, 4 dotted black bags.
 - * vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
 - * faded blue bags contain no other bags.
 - * dotted black bags contain no other bags.
 -
 - These rules specify the required contents for 9 bag types. In this example,
 - every faded blue bag is empty, every vibrant plum bag contains 11 bags (5
 - faded blue and 6 dotted black), and so on.
 -
 - You have a shiny gold bag. If you wanted to carry it in at least one other
 - bag, how many different bag colors would be valid for the outermost bag? (In
 - other words: how many colors can, eventually, contain at least one shiny gold
 - bag?)
 -
 - In the above rules, the following options would be available to you:
 -
 - * A bright white bag, which can hold your shiny gold bag directly.
 - * A muted yellow bag, which can hold your shiny gold bag directly, plus some
 -   other bags.
 - * A dark orange bag, which can hold bright white and muted yellow bags,
 -   either of which could then hold your shiny gold bag.
 - * A light red bag, which can hold bright white and muted yellow bags, either
 -   of which could then hold your shiny gold bag.
 -
 - So, in this example, the number of bag colors that can eventually contain at
 - least one shiny gold bag is 4.
 -
 - How many bag colors can eventually contain at least one shiny gold bag? (The
 - list of rules is quite long; make sure you get all of it.)
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import Util (converge)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map
import qualified Data.Set as Set

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
handleInput = print . length . findValidStartBags (BagId "shiny gold")

findValidStartBags :: BagId -> [Regulation] -> [BagId]
findValidStartBags target rules =
    Map.keys . Map.filter containsTarget $ canContainBag
  where
    canContainBag = converge expandSets $ initialSet rules
    containsTarget = Set.member target

expandSets :: Map.Map BagId (Set.Set BagId) -> Map.Map BagId (Set.Set BagId)
expandSets sets = Map.map f sets
  where
    f = Set.unions . Set.map g
    g bagId = Set.insert bagId $ sets Map.! bagId

initialSet :: [Regulation] -> Map.Map BagId (Set.Set BagId)
initialSet = Map.fromList . map requiredIds
  where
    requiredIds (Regulation bagId required) =
        (bagId, Set.fromList $ map extractRequired required)
    extractRequired (RequiredContent _ bagId) = bagId

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
