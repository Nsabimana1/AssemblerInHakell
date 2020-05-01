-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Data.Char
import           Control.Applicative

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a where
  P :: (String -> Maybe (a, String)) -> Parser a

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) = f

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing    -- fail on an empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, I've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = P f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b 
  fmap g (P f) = P (\s -> case (f s) of
                         Nothing -> Nothing
                         Just(a, s) -> Just((g a), s))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\s -> Just(a, s)) 

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  P g <*> P f = P
      (\s -> case g s of
        Nothing -> Nothing
        Just(fb, s') ->  case (f s') of 
          Nothing -> Nothing
          Just (a, s'') -> Just(fb a, s'')
      )


instance Alternative Parser where
    empty :: Parser a
    empty = P(\_ -> Nothing)

    (<|>) :: Parser a -> Parser a -> Parser a
    (P f1) <|> (P f2) =  P 
          (\s -> case (f1 s) of 
            Nothing -> case (f2 s) of 
              Nothing -> Nothing
              Just(a'', s'') -> Just(a'', s'')
            Just(a', s') -> Just(a', s')
          )


posChar :: Parser Char
posChar = P f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isUpper xs

openParen :: Parser Char
openParen = char '('

closedParen :: Parser Char
closedParen = char ')'

