module Parser.Common where

import Parser.Parser
import Data.Char
import Control.Applicative
import Debug.Trace (trace)
import GHC.IO.Unsafe (unsafePerformIO)

char :: Char -> Parser Char
char expected = Parser parse
  where
    parse ((line, char), found:rest)
      | found == expected = Right (pos,          rest, found)
      | otherwise =         Left  ((line, char), [[expected]])
        where pos = if found == '\n' then (line + 1, 0) else (line, char + 1)
    parse ((line, char), []) = Left ((line, char), [[expected]])

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

stringLiteral :: Parser String
stringLiteral = char '\"' *> while ('\"' /=) <* char '\"'

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy element separator = (:) <$> element <*> many (separator *> element) <|> pure []

sign :: Parser String
sign = string "+" <|> string "-" <|> string ""

digit :: Parser Char
digit = match isDigit "Digit"

integer :: Parser String
integer = Parser $ \inp -> do
  (pos,  tokens, sign)    <- run sign inp
  (pos', tokens', digits) <- run (some digit) (pos, tokens)
  Right (pos', tokens', read . show $ sign ++ digits)
  
{--float :: Parser Float
float = Parser $ \inp -> do
  (pos,  tokens, sign)       <- run sign inp
  (pos', tokens', digits)    <- run (some digit) (pos, tokens)
  (pos'', tokens'', dot)     <- run (string "." <|> string "") (pos', tokens')
  (pos''', tokens''', float) <- run (many digit) (pos'', tokens'')
  Right (pos''', tokens''', read . show $ sign ++ digits ++ dot ++ float)-}
