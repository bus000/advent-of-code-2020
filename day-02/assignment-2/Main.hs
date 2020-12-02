{- While it appears you validated the passwords correctly, they don't seem to be
 - what the Official Toboggan Corporate Authentication System is expecting.
 -
 - The shopkeeper suddenly realizes that he just accidentally explained the
 - password policy rules from his old job at the sled rental place down the
 - street! The Official Toboggan Corporate Policy actually works a little
 - differently.
 -
 - Each policy actually describes two positions in the password, where 1 means
 - the first character, 2 means the second character, and so on. (Be careful;
 - Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
 - these positions must contain the given letter. Other occurrences of the
 - letter are irrelevant for the purposes of policy enforcement.
 -
 - Given the same example list from above:
 -
 - * 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
 - * 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
 - * 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
 -
 - How many passwords are valid according to the new interpretation of the
 - policies?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Char as Char
import Control.Monad (void)

main :: IO ()
main = defaultMain parseInput handleInput

data PasswordPolicy = PasswordPolicy
    { _position1 :: !Int
    , _position2 :: !Int
    , _character :: !Char
    }

newtype Password = Password T.Text

handleInput :: [(PasswordPolicy, Password)] -> IO ()
handleInput = print . length . filter (uncurry validPassword)

validPassword :: PasswordPolicy -> Password -> Bool
validPassword (PasswordPolicy pos1 pos2 char) (Password pass) =
    (T.index pass (pos1 - 1) == char) /= (T.index pass (pos2 - 1) == char)

parseInput :: T.Text -> Either P.ParseError [(PasswordPolicy, Password)]
parseInput = P.parse (parseLines <* P.eof) ""

parseLines :: P.Parsec T.Text () [(PasswordPolicy, Password)]
parseLines = P.many parseLine

parseLine :: P.Parsec T.Text () (PasswordPolicy, Password)
parseLine = do
    pos1 <- P.int
    void $ P.char '-'
    pos2 <- P.int
    void $ P.char ' '
    char <- P.anyChar
    void $ P.string ": "
    password <- P.many (P.satisfy $ not . Char.isSpace)
    void $ P.char '\n'

    return $! (PasswordPolicy pos1 pos2 char, Password $ T.pack password)
