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

failure :: Parser a
failure = Parser (\cs -> [])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- class Monad m => MonadPlus m where
--   mzero :: m a
--   mplus :: m a -> m a -> m a

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of 
        [] -> parse q s 
        res -> res 


-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

-- | One or more.
-- some :: f a -> f [a]
-- some v = some_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- | Zero or more.
-- many :: f a -> f [a]
-- many v = many_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- instances of Alternative should satisfy the monoid laws
-- empty <|> x = x
-- x <|> empty = x
-- (x <|> y) <|> z = x <|> (y <|> z)

instance Alternative Parser where
    empty = mzero
    (<|>) = option