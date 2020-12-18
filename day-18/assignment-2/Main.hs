{- You manage to answer the child's questions and they finish part 1 of their
 - homework, but get stuck when they reach the next section: advanced math.
 -
 - Now, addition and multiplication have different precedence levels, but
 - they're not the ones you're familiar with. Instead, addition is evaluated
 - before multiplication.
 -
 - For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are
 - now as follows:
 -
 -    1 + 2 * 3 + 4 * 5 + 6
 -      3   * 3 + 4 * 5 + 6
 -      3   *   7   * 5 + 6
 -      3   *   7   *  11
 -         21       *  11
 -             231
 -
 - Here are the other examples from above:
 -
 - * 1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
 - * 2 * 3 + (4 * 5) becomes 46.
 - * 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
 - * 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
 - * ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
 -
 - What do you get if you add up the results of evaluating the homework problems
 - using these new rules?
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
parseExpr0 = parseExpr1 `P.chainl1` parseMul
  where
    parseMul = infixOp '*' Mul

parseExpr1 :: P.Parsec T.Text () Expr
parseExpr1 = parseExpr2 `P.chainl1` parseAdd
  where
    parseAdd = infixOp '+' Add

parseExpr2 :: P.Parsec T.Text () Expr
parseExpr2 = parseLiteral <|> parseBracket
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
