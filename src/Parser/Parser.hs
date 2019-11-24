module Parser.Parser where

import Control.Applicative
import Data.Either

type Position      = (Int, Int)
type ParserInput   = (Position, String)
type ParseError    = (Position, [String])
type Parsed a      = (Position, String, a)
type ParseResult a = Either ParseError (Parsed a)

runBase :: Parser a -> String -> ParseResult a
runBase p s = run p ((0, 0), s)

newtype Parser a = Parser {
  run :: ParserInput -> ParseResult a
}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \val -> do
      (position, rest, parsed) <- p val
      Right (position, rest, f parsed)

instance Applicative Parser where
  pure x = Parser $ \(pos, tokens) -> Right (pos, tokens, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \(pos, tokens) -> do
      (pos',  tokens',  f) <- p1 (pos,  tokens)
      (pos'', tokens'', v) <- p2 (pos', tokens')
      Right (pos'', tokens'', f v)

instance Alternative Parser where
  empty = Parser $ const $ Left ((0, 0), [])
  (Parser p1) <|> (Parser p2) =
    Parser $ \val -> f (p1 val) (p2 val)
    where
      f r@(Right _)      _                = r
      f _                r@(Right _)      = r
      f (Left (p, exp1)) (Left (_, exp2)) = Left (p, exp1 ++ exp2)
