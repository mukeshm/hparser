module Parsec where

import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad



newtype Parser a = Parser {parse :: String -> [(a, String)]}

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> 
    concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f p = Parser (\s -> 
    [(f a, s') | (a,s') <- parse p s])

instance Applicative Parser where
  pure a = unit a
  Parser cs1 <*> Parser cs2  = Parser (\s -> 
    [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    p >>= f = bind p f
    return a = pure a