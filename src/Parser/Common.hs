module Parser.Common where

import Parser.Parser
import Data.Char
import Control.Applicative

char :: Char -> Parser Char
char expected = match (== expected) [expected]

string :: String -> Parser String
string expected = traverse toMatch expected
  where toMatch :: Char -> Parser Char
        toMatch c = match (== c) expected

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

-- TODO: support escaped chars
stringLiteral :: Parser String
stringLiteral = Parser $ \inp -> do
  (pos,   tokens, _)    <- run (char '\"') inp
  (pos',  tokens', str) <- run (while (/= '\"')) (pos, tokens)
  (pos'', tokens'', _)  <- run (char '\"') (pos', tokens')
  Right (pos'', tokens'', str)

charLiteral :: Parser Char
charLiteral = Parser $ \inp -> do
  (pos,   tokens, _)    <- run (char '\'') inp
  (pos',  tokens', chr) <- run (match (const True) "Character") (pos, tokens)
  (pos'', tokens'', _)  <- run (char '\'') (pos', tokens')
  Right (pos'', tokens'', chr)

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
  (pos''', tokens''', float) <- run (if dot == "." then some digit else string "") (pos'', tokens'')
  let  rest = digits ++ dot ++ float in
    Right (pos''', tokens''', read $ if sign == "-" then sign ++ rest
                                                    else rest)
