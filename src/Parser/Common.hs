module Parser.Common where

import Parser.Parser
import Data.Char
import Control.Applicative
import Debug.Trace (trace)
import GHC.IO.Unsafe (unsafePerformIO)

char :: Char -> Parser Char
char expected = match (== expected) [expected]

string :: String -> Parser String
string = traverse char

while :: (Char -> Bool) -> Parser String
while f =
  Parser $ \((line, char), val) ->
    let (parsed, tokens') = span f val
        newLines = length $ filter ('\n' ==) parsed
        char' = length $ takeWhile ('\n' /=) (reverse parsed)
        pos = if newLines > 0 then (newLines, char') else (line, char + char')
     in Right (pos, tokens', parsed)

match :: (Char -> Bool) -> String -> Parser Char
match condition message = Parser parse
  where
    parse ((line, char), found:rest)
      | condition found = Right (pos,          rest, found)
      | otherwise =         Left  ((line, char), [message])
        where pos = if found == '\n' then (line + 1, 0) else (line, char + 1)
    parse ((line, char), []) = Left ((line, char), [message])

whitespace :: Parser String
whitespace = while isSpace

-- TODO: fix
--stringLiteral :: Parser String
--stringLiteral = char '\"' *> while ('\"' /=) <* char '\"'
-- TODO: fix
--charLiteral :: Parser Char
--charLiteral = char '\'' *> match (const True) "Char literal" <* char '\''

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy element separator = (:) <$> element <*> many (separator *> element) <|> pure []

sign :: Parser String
sign = string "+" <|> string "-" <|> string ""

digit :: Parser Char
digit = match isDigit "Digit"

integer :: Parser Integer
integer = Parser $ \inp -> do
  (pos,  tokens, sign)    <- run sign inp
  (pos', tokens', digits) <- run (some digit) (pos, tokens)
  Right (pos', tokens', read $ if sign == "-" then sign ++ digits
                                              else digits)

float :: Parser Float
float = Parser $ \inp -> do
  (pos,  tokens, sign)       <- run sign inp
  (pos', tokens', digits)    <- run (some digit) (pos, tokens)
  (pos'', tokens'', dot)     <- run (string "." <|> string "") (pos', tokens')
  (pos''', tokens''', float) <- run (many digit) (pos'', tokens'')
  let  rest = digits ++ dot ++ float in
    Right (pos''', tokens''', read $ if sign == "-" then sign ++ rest
                                                    else rest)
