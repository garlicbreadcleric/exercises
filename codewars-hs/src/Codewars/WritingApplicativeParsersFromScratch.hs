-- https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Codewars.WritingApplicativeParsersFromScratch where

import Data.Bifunctor (second)
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P \s -> second f <$> unP p s

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P \case
  (c:cs) | p c -> [(cs, c)]
  _ -> []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P \s -> do
  (s, f) <- unP pf s
  (s, x) <- unP px s
  pure (s, f x)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P \s -> do
  (s, a) <- unP pa s
  (s, _) <- unP pb s
  pure (s, a)

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P \s -> do
  (s, _) <- unP pa s
  (s, b) <- unP pb s
  pure (s, b)

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP "" = inject ""
stringP (c:cs) = (:) <#> charP c <@> stringP cs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P (const [])

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p1 <<>> p2 = P \s -> unP p1 s <> unP p2 s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = ((:) <#> p <@> many p) <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> (some p <<>> inject [])


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd . filter ((== "") . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
  [x] -> Just x
  _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr (BinOpE AddBO x y) = evalExpr x + evalExpr y
evalExpr (BinOpE MulBO x y) = evalExpr x * evalExpr y
evalExpr (NegE x) = -(evalExpr x)
evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique parseExpr'
  where
    parseExpr' :: Parser Expr
    parseExpr' = parseConst <<>> parseBinOpExpr <<>> parseNeg <<>> parseZero
    
    parseConst :: Parser Expr
    parseConst = ConstE <#> (read <#> some (foldr1 (<<>>) $ map charP ['0'..'9']))

    parseBinOpExpr :: Parser Expr
    parseBinOpExpr = flip BinOpE <#> (charP '(' @> parseExpr') <@> (charP ' ' @> parseBinOp <@ charP ' ') <@> (parseExpr' <@ charP ')')
  
    parseBinOp :: Parser BinOp
    parseBinOp = (charP '+' @> inject AddBO) <<>> (charP '*' @> inject MulBO)
    
    parseNeg :: Parser Expr
    parseNeg = NegE <#> (charP '-' @> parseExpr')
    
    parseZero :: Parser Expr
    parseZero = charP 'z' @> inject ZeroE
