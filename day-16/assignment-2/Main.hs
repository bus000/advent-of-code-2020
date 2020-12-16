{- Now that you've identified which tickets contain invalid values, discard
 - those tickets entirely. Use the remaining valid tickets to determine which
 - field is which.
 -
 - Using the valid ranges for each field, determine what order the fields appear
 - on the tickets. The order is consistent between all tickets: if seat is the
 - third field, it is the third field on every ticket, including your ticket.
 -
 - For example, suppose you have the following notes:
 -
 -    class: 0-1 or 4-19
 -    row: 0-5 or 8-19
 -    seat: 0-13 or 16-19
 -
 -    your ticket:
 -    11,12,13
 -
 -    nearby tickets:
 -    3,9,18
 -    15,1,5
 -    5,14,9
 -
 - Based on the nearby tickets in the above example, the first position must be
 - row, the second position must be class, and the third position must be seat;
 - you can conclude that in your ticket, class is 12, row is 11, and seat is 13.
 -
 - Once you work out which field is which, look for the six fields on your
 - ticket that start with the word departure. What do you get if you multiply
 - those six values together?
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import Util (converge)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data TicketRule = TicketRule
    { trName           :: !T.Text
    , trValidIntervals :: ![Interval]
    } deriving (Show, Eq, Ord)

data Interval = Interval
    { intervalLower :: !Int
    , intervalUpper :: !Int
    } deriving (Show, Eq, Ord)

data Ticket = Ticket
    { ticketFields :: ![UnknownField]
    } deriving (Show, Eq, Ord)

data UnknownField = UnknownField
    { unknownTicketPosition :: !Int
    , unknownTicketValue    :: !Int
    } deriving (Show, Eq, Ord)

data Input = Input
    { inputRules         :: ![TicketRule]
    , inputMyTicket      :: !Ticket
    , inputNearbyTickets :: ![Ticket]
    } deriving (Show, Eq, Ord)

data Problem = Problem
    { problemIndex    :: !Int
    , problemPossible :: ![TicketRule]
    , problemValues   :: ![Int]
    } deriving (Show, Eq, Ord)

handleInput :: Input -> IO ()
handleInput = print . departureProduct

departureProduct :: Input -> Int
departureProduct (Input rules myTicket@(Ticket myValues) nearby) =
    product $ map (\ix -> myValues' !! ix) departures
  where
    validTickets = filter (checkTicket rules) (myTicket:nearby)
    fields
        = map (map unknownTicketValue)
        . L.groupBy position
        . L.sortOn unknownTicketPosition
        . concatMap ticketFields
        $ validTickets
    problems = zipWith3 Problem [0..] (repeat rules) fields
    position (UnknownField pos1 _) (UnknownField pos2 _) = pos1 == pos2
    solvedProblems = solveProblems problems
    departures = findDepartures solvedProblems
    myValues' = map unknownTicketValue myValues

findDepartures :: [Problem] -> [Int]
findDepartures problems = map problemIndex . filter p $ problems
  where
    p (Problem _ (TicketRule name _:_) _) = "departure" `T.isPrefixOf` name
    p _ = False

solveProblems :: [Problem] -> [Problem]
solveProblems problems = converge removeSingletons problems'
  where
    problems' = map filterImpossible problems

removeSingletons :: [Problem] -> [Problem]
removeSingletons problems = map go problems
  where
    singletons = findSingletons problems

    go (Problem ix [rule] values) =
        Problem ix [rule] values
    go (Problem ix rules values) =
        Problem ix (filter (\x -> notElem x singletons) rules) values

findSingletons :: [Problem] -> [TicketRule]
findSingletons [] = []
findSingletons (Problem _ [rule] _:problems) = rule:findSingletons problems
findSingletons (_:problems) = findSingletons problems

filterImpossible :: Problem -> Problem
filterImpossible (Problem ix rules values) = Problem ix rules' values
  where
    rules' = filter (\rule -> all (`checkRule` rule) values) rules

cInterval :: Int -> Int -> Interval
cInterval x y
    | x < y = Interval x y
    | otherwise = Interval y x

inInterval :: Int -> Interval -> Bool
inInterval x (Interval lower upper) = x >= lower && x <= upper

checkRule :: Int -> TicketRule -> Bool
checkRule x (TicketRule _ intervals) = any (inInterval x) intervals

checkRules :: [TicketRule] -> Int -> Bool
checkRules rules x = any (checkRule x) rules

checkTicket :: [TicketRule] -> Ticket -> Bool
checkTicket rules (Ticket fields) = all (checkRules rules) values
  where
    values = map unknownTicketValue fields

parseInput :: T.Text -> Either P.ParseError Input
parseInput = P.parse (parseInput' <* P.eof) ""

parseInput' :: P.Parsec T.Text () Input
parseInput' = Input
    <$> parseRules <* P.string "\nyour ticket:\n"
    <*> parseTicket <* P.string "\n\nnearby tickets:\n"
    <*> parseTicket `P.endBy` P.newline

parseRules :: P.Parsec T.Text () [TicketRule]
parseRules = parseRule `P.endBy` P.newline

parseRule :: P.Parsec T.Text () TicketRule
parseRule = TicketRule
    <$> (T.pack <$> P.manyTill (P.oneOf identifierChars) (P.string ": "))
    <*> parseIntervals
  where
    identifierChars = ' ':['a'..'z']

parseIntervals :: P.Parsec T.Text () [Interval]
parseIntervals = parseInterval `P.sepBy` P.string " or "

parseInterval :: P.Parsec T.Text () Interval
parseInterval = cInterval <$> P.int <* P.char '-' <*> P.int

parseTicket :: P.Parsec T.Text () Ticket
parseTicket = Ticket <$> parseTicketFields

parseTicketFields :: P.Parsec T.Text () [UnknownField]
parseTicketFields = zipWith UnknownField [0..] <$> P.int `P.sepBy` P.char ','
