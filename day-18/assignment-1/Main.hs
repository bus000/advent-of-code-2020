{- --- Day 18: Operation Order ---
 -
 - As you look out the window and notice a heavily-forested continent slowly
 - appear over the horizon, you are interrupted by the child sitting next to
 - you. They're curious if you could help them with their math homework.
 -
 - Unfortunately, it seems like this "math" follows different rules than you
 - remember.
 -
 - The homework (your puzzle input) consists of a series of expressions that
 - consist of addition (+), multiplication (*), and parentheses ((...)). Just
 - like normal math, parentheses indicate that the expression inside must be
 - evaluated before it can be used by the surrounding expression. Addition still
 - finds the sum of the numbers on both sides of the operator, and
 - multiplication still finds the product.
 -
 - However, the rules of operator precedence have changed. Rather than
 - evaluating multiplication before addition, the operators have the same
 - precedence, and are evaluated left-to-right regardless of the order in which
 - they appear.
 -
 - For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are
 - as follows:

 -    1 + 2 * 3 + 4 * 5 + 6
 -      3   * 3 + 4 * 5 + 6
 -          9   + 4 * 5 + 6
 -             13   * 5 + 6
 -                 65   + 6
 -                     71
 -
 - Parentheses can override this order; for example, here is what happens if
 - parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):
 -
 -    1 + (2 * 3) + (4 * (5 + 6))
 -    1 +    6    + (4 * (5 + 6))
 -         7      + (4 * (5 + 6))
 -         7      + (4 *   11   )
 -         7      +     44
 -                51
 -
 - Here are a few more examples:
 -
 - * 2 * 3 + (4 * 5) becomes 26.
 - * 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
 - * 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
 - * ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.
 -
 - Before you can help with the homework, you need to understand it yourself.
 - Evaluate the expression on each line of the homework; what is the sum of the
 - resulting values?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Expr] -> IO ()
handleInput = print . sum . map evaluate

data Expr
    = Lit !Int
    | Add !Expr !Expr
    | Mul !Expr !Expr
  deriving (Show, Eq, Ord)

evaluate :: Expr -> Int
evaluate (Lit x) = x
evaluate (Add left right) = evaluate left + evaluate right
evaluate (Mul left right) = evaluate left * evaluate right

parseInput :: T.Text -> Either P.ParseError [Expr]
parseInput = P.parse (parseExprs <* P.eof) ""

parseExprs :: P.Parsec T.Text () [Expr]
parseExprs = parseExpr0 `P.endBy` P.newline

parseExpr0 :: P.Parsec T.Text () Expr
parseExpr0 = parseExpr1 `P.chainl1` (parseAdd <|> parseMul)
  where
    parseAdd = infixOp '+' Add
    parseMul = infixOp '*' Mul

parseExpr1 :: P.Parsec T.Text () Expr
parseExpr1 = parseLiteral <|> parseBracket
  where
    parseLiteral = Lit <$> token P.int
    parseBracket =
        P.between (token $ P.char '(') (token $ P.char ')') parseExpr0

infixOp :: Char -> (a -> a -> a) -> P.Parsec T.Text () (a -> a -> a)
infixOp op f = token (P.char op) *> pure f

token :: P.Parsec T.Text () a -> P.Parsec T.Text () a
token f = spaces *> f <* spaces
  where
    spaces = P.many $ P.char ' '
