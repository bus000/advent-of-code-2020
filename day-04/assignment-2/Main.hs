{- The line is moving more quickly now, but you overhear airport security
 - talking about how passports with invalid data are getting through. Better add
 - some data validation, quick!
 -
 - You can continue to ignore the cid field, but each other field has strict
 - rules about what values are valid for automatic validation:
 -
 - * byr (Birth Year) - four digits; at least 1920 and at most 2002.
 - * iyr (Issue Year) - four digits; at least 2010 and at most 2020.
 - * eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
 - * hgt (Height) - a number followed by either cm or in:
 -     * If cm, the number must be at least 150 and at most 193.
 -     * If in, the number must be at least 59 and at most 76.
 - * hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
 - * ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
 - * pid (Passport ID) - a nine-digit number, including leading zeroes.
 - * cid (Country ID) - ignored, missing or not.
 -
 - Your job is to count the passports where all required fields are both present
 - and valid according to the above rules. Here are some example values:
 -
 - byr valid:   2002
 - byr invalid: 2003
 -
 - hgt valid:   60in
 - hgt valid:   190cm
 - hgt invalid: 190in
 - hgt invalid: 190
 -
 - hcl valid:   #123abc
 - hcl invalid: #123abz
 - hcl invalid: 123abc
 -
 - ecl valid:   brn
 - ecl invalid: wat
 -
 - pid valid:   000000001
 - pid invalid: 0123456789
 -
 - Here are some invalid passports:
 -
 - eyr:1972 cid:100
 - hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
 -
 - iyr:2019
 - hcl:#602927 eyr:1967 hgt:170cm
 - ecl:grn pid:012533040 byr:1946
 -
 - hcl:dab227 iyr:2012
 - ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
 -
 - hgt:59cm ecl:zzz
 - eyr:2038 hcl:74454a iyr:2023
 - pid:3556412378 byr:2007
 -
 - Here are some valid passports:
 -
 - pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
 - hcl:#623a2f
 -
 - eyr:2029 ecl:blu cid:129 byr:1989
 - iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
 -
 - hcl:#888785
 - hgt:164cm byr:2001 iyr:2015 cid:88
 - pid:545766238 ecl:hzl
 - eyr:2022
 -
 - iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
 -
 - Count the number of valid passports - those that have all required fields and
 - valid values. Continue to treat cid as optional. In your batch file, how many
 - passports are valid?
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Maybe as Maybe
import Control.Monad (guard)
import Numeric (readHex)

main :: IO ()
main = defaultMain parseInput handleInput

data Passport = Passport
    { _birthYear      :: !Int
    , _issueYear      :: !Int
    , _expirationYear :: !Int
    , _height         :: !Height
    , _hairColor      :: !Int
    , _eyeColor       :: !EyeColor
    , _passportId     :: !Int
    , _countryId      :: !(Maybe T.Text)
    } deriving (Show, Eq, Ord)

type PasswordPart = (T.Text, T.Text)

type UnvalidatedPassword = [PasswordPart]

data Height = Inches Int | Cm Int
  deriving (Show, Eq, Ord)

data EyeColor = Amber | Blue | Brown | Grey | Green | Hazel | Other
  deriving (Show, Eq, Ord)

handleInput :: [UnvalidatedPassword] -> IO ()
handleInput = print . length . filter Maybe.isJust . map newPassport

newPassport :: UnvalidatedPassword -> Maybe Passport
newPassport parts = do
    birthYear <- lookup "byr" parts >>= parseIntBetween (1920, 2002)
    issueYear <- lookup "iyr" parts >>= parseIntBetween (2010, 2020)
    expirationYear <- lookup "eyr" parts >>= parseIntBetween (2020, 2030)
    height <- lookup "hgt" parts >>= parseHeight
    hairColor <- lookup "hcl" parts >>= parseHairColor
    eyeColor <- lookup "ecl" parts >>= parseEyeColor
    passportId <- lookup "pid" parts >>= parsePassportId
    let countryId = lookup "cid" parts

    return $! Passport birthYear issueYear expirationYear height hairColor
        eyeColor passportId countryId

parseIntBetween :: (Int, Int) -> T.Text -> Maybe Int
parseIntBetween (lower, upper) txt = do
    int <- toMaybe $ P.parse (P.int <* P.eof) "" txt
    guard $ int >= lower && int <= upper
    return int

parseHeight :: T.Text -> Maybe Height
parseHeight txt = case P.parse (height <* P.eof) "" txt of
    Right (Inches h) | h >= 59 && h <= 76 -> Just $ Inches h
    Right (Cm h) | h >= 150 && h <= 193 -> Just $ Cm h
    _ -> Nothing
  where
    height = P.choice $ map P.try
        [ Inches <$> P.int <* P.string "in"
        , Cm <$> P.int <* P.string "cm"
        ]

parseHairColor :: T.Text -> Maybe Int
parseHairColor = toMaybe . P.parse (hairColor <* P.eof) ""
  where
    hairColor = fst . head . readHex
        <$> (P.char '#' *> P.count 6 (P.oneOf hexChars))
    hexChars = ['0'..'9'] ++ ['a'..'f']

parseEyeColor :: T.Text -> Maybe EyeColor
parseEyeColor = toMaybe . P.parse (eyeColor <* P.eof) ""
  where
    eyeColor = P.choice
        [ P.try (P.string "amb") *> pure Amber
        , P.try (P.string "blu") *> pure Blue
        , P.try (P.string "brn") *> pure Brown
        , P.try (P.string "gry") *> pure Grey
        , P.try (P.string "grn") *> pure Green
        , P.try (P.string "hzl") *> pure Hazel
        , P.try (P.string "oth") *> pure Other
        ]

parsePassportId :: T.Text -> Maybe Int
parsePassportId = toMaybe . P.parse (passportId <* P.eof) ""
  where
    passportId = read <$> P.count 9 P.digit

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

parseInput :: T.Text -> Either P.ParseError [UnvalidatedPassword]
parseInput = P.parse (parsePassports <* P.eof) ""

parsePassports :: P.Parsec T.Text () [UnvalidatedPassword]
parsePassports = P.sepBy parsePassport (P.string "\n")

parsePassport :: P.Parsec T.Text () UnvalidatedPassword
parsePassport = P.many parseKeyValue
  where
    parseKeyValue = (\x y -> (T.pack x, T.pack y))
        <$> P.choice keys <* P.char ':'
        <*> P.manyTill P.anyChar P.space
    keys = map (P.try . P.string)
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
