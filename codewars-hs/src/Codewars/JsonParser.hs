-- https://www.codewars.com/kata/55aa170b54c32468c30000a9

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Codewars.JsonParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

data Value
  = String String
  | Number Double
  | Object [(Value, Value)]
  | Array [Value]
  | Boolean Bool
  | Null
  deriving (Show)

parse :: String -> Maybe Value
parse input = case runParser parseValue input of
  Just ("", v) -> Just v
  _ -> Nothing

parseValue :: Parser Value
parseValue = do
  _ <- spaces0
  v <- parseString <|> parseNumber <|> parseObject <|> parseArray <|> parseBoolean <|> parseNull
  _ <- spaces0
  pure v

parseString :: Parser Value
parseString = do
  _ <- string "\""
  s <- while0 (/= '"')
  _ <- string "\""
  pure (String s)

parseNumber :: Parser Value
parseNumber = do
  int <- parseInt
  frac <- fromMaybe "" <$> optional parseFrac
  pure (Number (read (int <> frac)))
  where
    parseInt :: Parser String
    parseInt = do
      sign <- fromMaybe "" <$> optional (string "-")
      digits <- (do first <- parseDigit1_9; rest <- many1 parseDigit; pure (first : rest)) <|> (pure <$> parseDigit)

      pure (sign <> digits)

    parseFrac :: Parser String
    parseFrac = do
      dot <- char '.'
      digits <- many1 parseDigit
      pure (dot : digits)

    parseDigit :: Parser Char
    parseDigit = do
      c <- anyChar
      if c `elem` ['0' .. '9'] then pure c else empty

    parseDigit1_9 :: Parser Char
    parseDigit1_9 = do
      c <- anyChar
      if c `elem` ['1' .. '9'] then pure c else empty

parseObject :: Parser Value
parseObject = do
  _ <- char '{'
  _ <- spaces0
  pairs <- parsePairs
  _ <- spaces0
  _ <- char '}'
  pure (Object pairs)
  where
    parsePairs :: Parser [(Value, Value)]
    parsePairs = fromMaybe [] <$> optional parsePairs'

    parsePairs' :: Parser [(Value, Value)]
    parsePairs' = do
      first <- parsePair
      rest <- many0 (char ',' >> parsePair)
      pure (first : rest)

    parsePair :: Parser (Value, Value)
    parsePair = do
      key <- parseString
      _ <- spaces0
      _ <- char ':'
      _ <- spaces0
      value <- parseValue
      pure (key, value)

parseArray :: Parser Value
parseArray = do
  _ <- char '['
  elements <- parseElements
  _ <- char ']'
  pure (Array elements)
  where
    parseElements :: Parser [Value]
    parseElements = fromMaybe [] <$> optional parseElements'

    parseElements' :: Parser [Value]
    parseElements' = do
      first <- parseValue
      rest <- many0 (char ',' >> parseValue)
      pure (first : rest)

parseBoolean :: Parser Value
parseBoolean = (string "true" >> pure (Boolean True)) <|> (string "false" >> pure (Boolean False))

parseNull :: Parser Value
parseNull = string "null" >> pure Null

-- Parser combinators.

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser (Just . (,a))
  p1 <*> p2 = Parser $ \s -> do
    (s2, f) <- runParser p1 s
    (s3, a) <- runParser p2 s2
    pure (s3, f a)

instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (s2, a) <- runParser p s
    runParser (f a) s2

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

anyChar :: Parser Char
anyChar = Parser $ \case
  (c : s) -> Just (s, c)
  _ -> Nothing

char :: Char -> Parser Char
char c = anyChar >>= \c' -> if c == c' then pure c' else empty

string :: String -> Parser String
string s = Parser (fmap (,s) . stripPrefix s)

while0 :: (Char -> Bool) -> Parser String
while0 f = many0 (anyChar >>= \c -> if f c then pure c else empty)

while1 :: (Char -> Bool) -> Parser String
while1 f = do
  s <- while0 f
  if null s then empty else pure s

many0 :: Parser a -> Parser [a]
many0 = go []
  where
    go acc p = Parser $ \s -> case runParser p s of
      Nothing -> Just (s, acc)
      Just (s', x) -> runParser (go (acc <> [x]) p) s'

many1 :: Parser a -> Parser [a]
many1 p = do
  xs <- many0 p
  if null xs then empty else pure xs

spaces0 :: Parser ()
spaces0 = void $ while0 (== ' ')

