{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Parser (tokenizer, Token, TokenizerState(Go, End))
import Control.Monad.State
import Control.Monad (guard)


main :: IO ()
main = do
  str <- getLine
  putStrLn $ "Getting str: "
  print str
  let (tokens, state) = runState (tokenizer str) Go
  putStrLn $ "Tokenizer finish with state" <> show state
  guard (state == End)
  print tokens
