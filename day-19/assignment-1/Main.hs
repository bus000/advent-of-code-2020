{- --- Day 19: Monster Messages ---
 -
 - You land in an airport surrounded by dense forest. As you walk to your
 - high-speed train, the Elves at the Mythical Information Bureau contact you
 - again. They think their satellite has collected an image of a sea monster!
 - Unfortunately, the connection to the satellite is having problems, and many
 - of the messages sent back from the satellite have been corrupted.
 -
 - They sent you a list of the rules valid messages should obey and a list of
 - received messages they've collected so far (your puzzle input).
 -
 - The rules for valid messages (the top part of your puzzle input) are numbered
 - and build upon each other. For example:
 -
 -    0: 1 2
 -    1: "a"
 -    2: 1 3 | 3 1
 -    3: "b"
 -
 - Some rules, like 3: "b", simply match a single character (in this case, b).
 -
 - The remaining rules list the sub-rules that must be followed; for example,
 - the rule 0: 1 2 means that to match rule 0, the text being checked must match
 - rule 1, and the text after the part that matched rule 1 must then match rule
 - 2.
 -
 - Some of the rules have multiple lists of sub-rules separated by a pipe (|).
 - This means that at least one list of sub-rules must match. (The ones that
 - match might be different each time the rule is encountered.) For example, the
 - rule 2: 1 3 | 3 1 means that to match rule 2, the text being checked must
 - match rule 1 followed by rule 3 or it must match rule 3 followed by rule 1.
 -
 - Fortunately, there are no loops in the rules, so the list of possible matches
 - will be finite. Since rule 1 matches a and rule 3 matches b, rule 2 matches
 - either ab or ba. Therefore, rule 0 matches aab or aba.
 -
 - Here's a more interesting example:
 -
 -    0: 4 1 5
 -    1: 2 3 | 3 2
 -    2: 4 4 | 5 5
 -    3: 4 5 | 5 4
 -    4: "a"
 -    5: "b"
 -
 - Here, because rule 4 matches a and rule 5 matches b, rule 2 matches two
 - letters that are the same (aa or bb), and rule 3 matches two letters that are
 - different (ab or ba).
 -
 - Since rule 1 matches rules 2 and 3 once each in either order, it must match
 - two pairs of letters, one pair with matching letters and one pair with
 - different letters. This leaves eight possibilities: aaab, aaba, bbab, bbba,
 - abaa, abbb, baaa, or babb.
 -
 - Rule 0, therefore, matches a (rule 4), then any of the eight options from
 - rule 1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb,
 - abaaab, or ababbb.
 -
 - The received messages (the bottom part of your puzzle input) need to be
 - checked against the rules so you can determine which are valid and which are
 - corrupted. Including the rules and the messages together, this might look
 - like:
 -
 -    0: 4 1 5
 -    1: 2 3 | 3 2
 -    2: 4 4 | 5 5
 -    3: 4 5 | 5 4
 -    4: "a"
 -    5: "b"
 -
 -    ababbb
 -    bababa
 -    abbbab
 -    aaabbb
 -    aaaabbb
 -
 - Your goal is to determine the number of messages that completely match rule
 - 0. In the above example, ababbb and abbbab match, but bababa, aaabbb, and
 - aaaabbb do not, producing the answer 2. The whole message must match all of
 - rule 0; there can't be extra unmatched characters in the message. (For
 - example, aaaabbb might appear to match rule 0 above, but it has an extra
 - unmatched b on the end.)
 -
 - How many messages completely match rule 0?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

data Rule
    = RuleRef !Int
    | RuleLit !Char
    | RuleAlt !Rule !Rule
    | RuleFol !Rule !Rule
  deriving (Show, Eq, Ord)

data Validator
    = Fol !Validator !Validator
    | Alt !Validator !Validator
    | Lit !Char
  deriving (Show, Eq, Ord)

data Input = Input
    { inputRules    :: !(Map.Map Int Rule)
    , inputMessages :: ![String]
    }
  deriving (Show, Eq, Ord)

handleInput :: Input -> IO ()
handleInput (Input rules messages) = case toValidator rules 0 of
    Just validator -> print . length . filter (validate validator) $ messages
    Nothing -> putStrLn "Constructing validator failed."

toValidator :: Map.Map Int Rule -> Int -> Maybe Validator
toValidator rules n = Map.lookup n rules >>= go
  where
    go (RuleRef ref) = do
        rule <- Map.lookup ref rules
        rule' <- go rule

        return $! rule'
    go (RuleLit char) =
        Just $ Lit char
    go (RuleAlt left right) = do
        left' <- go left
        right' <- go right

        return $! Alt left' right'
    go (RuleFol rule1 rule2) = do
        rule1' <- go rule1
        rule2' <- go rule2

        return $! Fol rule1' rule2'

validate :: Validator -> String -> Bool
validate validator string = case go validator string of
    (True, "") -> True
    _ -> False
  where
    go (Lit _) [] = (False, [])
    go (Lit c) (c':cs)
        | c == c' = (True, cs)
        | otherwise = (False, c':cs)
    go (Alt left right) cs = case go left cs of
        (True, cs') -> (True, cs')
        (False, _) -> case go right cs of
            (True, cs'') -> (True, cs'')
            (False, _) -> (False, cs)
    go (Fol left right) cs = case go left cs of
        (True, cs') -> case go right cs' of
            (True, cs'') -> (True, cs'')
            (False, _) -> (False, cs)
        (False, _) -> (False, cs)

parseInput :: T.Text -> Either P.ParseError Input
parseInput = P.parse (parseInput' <* P.eof) ""

parseInput' :: P.Parsec T.Text () Input
parseInput' = Input <$> (parseRules <* P.newline) <*> parseMessages

parseRules :: P.Parsec T.Text () (Map.Map Int Rule)
parseRules = Map.fromList <$> parseRule `P.endBy` P.newline

parseRule :: P.Parsec T.Text () (Int, Rule)
parseRule = (,) <$> P.int <* P.string ": " <*> parseRule0

parseRule0 :: P.Parsec T.Text () Rule
parseRule0 = parseRule1 `P.chainl1` parseAlt
  where
    parseAlt = P.string " | " *> pure RuleAlt

parseRule1 :: P.Parsec T.Text () Rule
parseRule1 = parseRule2 `P.chainl1` parseFol
  where
    parseFol = P.try (P.char ' ' <* P.notFollowedBy (P.char '|')) *> pure RuleFol

parseRule2 :: P.Parsec T.Text () Rule
parseRule2 = parseRef <|> parseLiteral
  where
    parseRef = RuleRef <$> P.int
    parseLiteral = RuleLit <$> quotedString
    quotedString = P.between (P.char '"') (P.char '"') P.letter

parseMessages :: P.Parsec T.Text () [String]
parseMessages = P.many P.letter `P.endBy` P.newline
