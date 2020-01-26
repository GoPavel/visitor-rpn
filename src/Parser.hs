{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad.State
import Data.Char (isDigit, digitToInt, isSpace)


data Token
  = LEFT | RIGHT | EOF
  | ADD | MUL | DIV | SUB
  | NUM Int
  deriving (Show, Eq)

data TokenizerState
  = Number Int
  | Go
  | Error String
  | End
  deriving (Show, Eq)

withNewState :: b -> State b a -> State b a
withNewState v = withState (const v)

tokenizer :: String -> State TokenizerState [Token]
tokenizer (x:xs) = do
  get >>= \case
    Number a -> if isDigit x
      then do
        put $ Number $ a * 10 + digitToInt x
        tokenizer xs
      else
        withNewState Go $
          (NUM a : ) <$> tokenizer (x:xs)
    Go ->
      if isDigit x then
        withNewState (Number 0) $
          tokenizer (x:xs)
      else if isSpace x then
        tokenizer xs
      else case x of
          '(' -> (LEFT :)  <$> tokenizer xs
          ')' -> (RIGHT :) <$> tokenizer xs
          '+' -> (ADD :)   <$> tokenizer xs
          '-' -> (SUB :)   <$> tokenizer xs
          '*' -> (MUL :)   <$> tokenizer xs
          '/' -> (DIV :)   <$> tokenizer xs
          c   -> withNewState (Error $ "Unexpected symbol: " <> show c) $
                  return []
    Error msg -> return []
    End -> return []
tokenizer [] = do
  get >>= \case
    Error _  -> return []
    Number a -> withNewState End $ return [NUM a]
    _       -> withNewState End $ return []
