{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Catch (SomeException, throwM)
import Control.Monad (guard)
import Lens.Micro.Mtl ((.=))

import Parser (Token(..), BinOp(..), TokenizerState(..)
              , tokenize, parse_rpn)
import Visitor (Visitor, acc, stack, pop, push, visit, throwVisitor)

show_visitor :: Token -> Visitor String String ()
show_visitor (NUM a) = push $ show a
show_visitor (LeftBinOp _ op) = do
  b <- pop; a <- pop
  let str_op = case op of {ADD -> " + "; SUB -> " - "; MUL -> " * "; DIV -> " / "}
  push $ concat ["(", a, str_op, b,")"]
show_visitor EOF = pop >>= (acc .=)
show_visitor _  = throwVisitor "Broken RPN-expression!"

calc_visitor :: Token -> Visitor Int Int ()
calc_visitor (NUM a) = push a
calc_visitor (LeftBinOp _ op) = do
  b <- pop; a <- pop
  let f = case op of { ADD -> (+); SUB -> (-); MUL -> (*); DIV -> div}
  push (f a b)
calc_visitor EOF = pop >>= (acc .=)
calc_visitor _  = throwVisitor "Broken RPN-expression!"

from_error :: Either SomeException a -> IO a
from_error = either (throwM) return

main :: IO ()
main = do
  str <- getLine
  putStrLn $ ">Getting str: "
  print str
  putStrLn $ ">TOKENS:"
  tokens <- from_error $ tokenize str
  print tokens

  putStrLn $ ">RPN:"
  rpn <- from_error $ parse_rpn tokens
  print rpn

  putStrLn $ ">calc-visitor:"
  print $ visit rpn 0 calc_visitor

  putStrLn $ ">show-visitor:"
  putStrLn $ either (error . show) id
           $ visit rpn "" show_visitor
  return ()
