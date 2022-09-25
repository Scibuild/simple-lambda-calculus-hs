{-# LANGUAGE LambdaCase #-}
module Parser where

import qualified Data.Bifunctor
import Control.Applicative 
import Control.Monad.Fail

newtype Parser a b = Parser { parse :: [a] -> Maybe (b, [a]) }
-- input string -> maybe could parse (result, remaining string)

instance Functor (Parser a) where
  -- fmap :: (a -> b) -> (Parser a -> Parser b)
  fmap f (Parser p) = Parser $ \s -> fmap (Data.Bifunctor.first f) (p s)

instance Applicative (Parser a) where
  pure x = Parser $ \s -> Just (x, s)
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  (Parser f) <*> (Parser a) = Parser $ \s -> do
    (f', s1) <- f s
    (a', s2) <- a s1
    return (f' a', s2)

instance Monad (Parser a) where
  -- >>= :: Parser c a -> (a -> Parser c b) -> Parser c b
  (Parser a) >>= f = Parser $ \s -> do
    (a', s1) <- a s
    parse (f a') s1

instance MonadFail (Parser a) where
  -- fail :: String -> Parser a
  fail _ = Parser $ const Nothing

instance Alternative (Parser a) where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \s -> 
    case a s of
      Just x -> Just x
      Nothing -> b s

item :: Parser a a
item = Parser $ \case
  [] -> Nothing
  (x:xs) -> Just (x, xs)

satisfy :: (a -> Bool) -> Parser a a
satisfy f = do 
  c <- item
  if f c then return c else empty

match :: (Eq a) => a -> Parser a a
match a = satisfy (== a)

spaces :: Parser Char String
spaces = many $ satisfy (`elem` " \n\r\t")

try :: (Alternative m, Monad m) => m a -> m (Maybe a)
try p = (Just <$> p) <|> return Nothing

matchList :: (Eq a) => [a] -> Parser a [a]
matchList [] = pure []
matchList (x:xs) = do
  match x
  ys <- matchList xs
  return $ x : ys

