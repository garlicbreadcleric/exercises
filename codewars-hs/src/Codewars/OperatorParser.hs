-- https://www.codewars.com/kata/5842777813ee50ef38000020

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Codewars.OperatorParser where

import Control.Applicative hiding (many)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b
  = Op (OpTree a b) a (OpTree a b)
  | Term b
  deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
  deriving (Show, Eq, Functor)

data AssociativityTag = LeftAssociative | RightAssociative | NotAssociative
  deriving (Show, Eq)

type Priority = Int

data Operator a = Operator {operator :: a, associativity :: AssociativityTag, priority :: Priority, isClosed :: Bool}
  deriving (Show)

unAssociativity :: Associativity a -> a
unAssociativity (L x) = x
unAssociativity (R x) = x
unAssociativity (NoAssociativity x) = x

associativityToTag :: Associativity a -> AssociativityTag
associativityToTag (L _) = LeftAssociative
associativityToTag (R _) = RightAssociative
associativityToTag _ = NotAssociative

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree _ (Term x) = x
foldTree f (Op l o r) = f o (foldTree f l) (foldTree f r)

mapOperators :: (a -> b) -> OpTree a c -> OpTree b c
mapOperators _ (Term x) = Term x
mapOperators f (Op l o r) = Op (mapOperators f l) (f o) (mapOperators f r)

-- | Return a parser such that: given 'op s a', if s matches, the parser
-- | returns a.
op :: String -> a -> ReadP a
op s v = string s $> v

-- | Accept two arguments:
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity;
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree.
parseOperators :: (Show a, Show b) => [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators operatorParsers parseTerm = mapOperators operator <$> (rebalanceOperators =<< parseOperators')
  where
    operatorParsers' :: [ReadP (Operator _)]
    operatorParsers' = concatMap parserWithPriority $ zip [0 .. length operatorParsers] operatorParsers

    parserWithPriority :: (Priority, Associativity [ReadP a]) -> [ReadP (Operator a)]
    parserWithPriority (priority, aParser) =
      let aTag = associativityToTag aParser
       in fmap (\operator -> Operator {operator, associativity = aTag, priority, isClosed = False}) <$> unAssociativity aParser

    parseOperator :: ReadP (Operator _)
    parseOperator = asum operatorParsers'

    parseOperatorWithTerm :: ReadP (Operator _, _)
    parseOperatorWithTerm = do
      o <- parseOperator
      _ <- skipSpaces
      t <- parseTerm'
      pure (o, t)

    parseOperators' :: ReadP (OpTree (Operator _) _)
    parseOperators' = do
      first <- parseTerm'
      rest <- many (skipSpaces >> parseOperatorWithTerm)
      pure $ foldr (\(op, t2) t1 -> Op t1 op t2) first (reverse rest)

    parseTerm' :: ReadP (OpTree (Operator _) _)
    parseTerm' =
      Term <$> parseTerm <|> do
        _ <- char '('
        _ <- skipSpaces
        tree <- closeTree <$> parseOperators'
        _ <- skipSpaces
        _ <- char ')'
        pure tree

    closeTree :: OpTree (Operator _) _ -> OpTree (Operator _) _
    closeTree t@(Term _) = t
    closeTree (Op l op r) = Op l op {isClosed = True} r

    rebalanceOperators :: OpTree _ _ -> ReadP (OpTree _ _)
    rebalanceOperators tree = do
      (tree', rebalanced) <- go tree
      if rebalanced then rebalanceOperators tree' else pure tree'
      where
        go :: OpTree _ _ -> ReadP (OpTree _ _, Bool)
        go (Op e1 op1 (Op e2 op2 e3))
          | shouldRebalanceLeft op1 op2 = pure (Op (Op e1 op1 {isClosed = isClosed op2} e2) op2 {isClosed = isClosed op1} e3, True)
          | shouldFailNoAssociativity op1 op2 = empty
        go (Op (Op e1 op1 e2) op2 e3)
          | shouldRebalanceRight op1 op2 = pure (Op e1 op1 {isClosed = isClosed op2} (Op e2 op2 {isClosed = isClosed op1} e3), True)
          | shouldFailNoAssociativity op1 op2 = empty
        go (Op e1 op e2) = do
          (e1', r1) <- go e1
          (e2', r2) <- go e2
          pure (Op e1' op e2', r1 || r2)
        go t@(Term _) = pure (t, False)

    shouldRebalanceLeft :: Operator _ -> Operator _ -> Bool
    shouldRebalanceLeft op1 op2 = (p1 > p2 || p1 == p2 && a1 == LeftAssociative && a2 == LeftAssociative) && not (isClosed op2)
      where
        p1 = priority op1
        p2 = priority op2
        a1 = associativity op1
        a2 = associativity op2

    shouldRebalanceRight :: Operator _ -> Operator _ -> Bool
    shouldRebalanceRight op1 op2 = (p1 < p2 || p1 == p2 && a1 == RightAssociative && a2 == RightAssociative) && not (isClosed op1)
      where
        p1 = priority op1
        p2 = priority op2
        a1 = associativity op1
        a2 = associativity op2
    
    shouldFailNoAssociativity :: Operator _ -> Operator _ -> Bool
    shouldFailNoAssociativity Operator {associativity = NotAssociative, isClosed = False} Operator {associativity = NotAssociative, isClosed = False} = True
    shouldFailNoAssociativity _ _ = False
