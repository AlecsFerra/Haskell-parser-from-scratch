module Parser.Common where

import Parser.Parser
import Data.Char (isSpace, isDigit)
import Control.Applicative (many, (<|>))
import Debug.Trace (trace)

parseChar :: Char -> Parser Char
parseChar expected = Parser parse
  where
    parse ((line, char), found:rest)
      | found == expected = Right (pos,          rest, found)
      | otherwise =         Left  ((line, char), [[expected]])
        where pos = if expected == '\n' then (line + 1, 0) else (line, char + 1)
    parse ((line, char), []) = Left ((line, char), [[expected]])

parseString :: String -> Parser String
parseString = traverse parseChar

parseMatch :: (Char -> Bool) -> Parser String
parseMatch f =
  Parser $ \((line, char), val) ->
    let (parsed, tokens') = span f val
        newLines = length $ filter ('\n' ==) parsed
        char' = length $ takeWhile ('\n' /=) (reverse parsed)
        pos = if newLines > 0 then (newLines, char') else (line, char + char')
     in Right (pos, tokens', parsed)


whitespace :: Parser String
whitespace = parseMatch isSpace

stringLiteral :: Parser String
stringLiteral = parseChar '\"' *> parseMatch ('\"' /=) <* parseChar '\"'

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy element separator = (:) <$> element <*> many (separator *> element) <|> pure []

-- BROKEN TODO: Fix
integer :: Parser Integer
integer =
  Parser $ \(pos, tokens) -> do
    (pos', tokens', sign)     <- run signParser (pos, tokens)
    (pos'', tokens'', digits) <- run (parseMatch isDigit) (pos', tokens')
    Right (pos'' `debug` (sign:digits) , tokens'', read (sign:digits))
    where
      signParser = parseChar '+' <|> parseChar '-' <|> pure ' '

debug = flip trace