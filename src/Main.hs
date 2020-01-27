{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (guard)
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Mtl

import Parser (Token(..), BinOp(..), TokenizerState(..)
              , tokenizer, parser_visitor)
import Visitor (Visitor, res, stack, pop, push, visit, throwVisitor)

show_visitor :: Token -> Visitor String String ()
show_visitor (NUM a) = push $ show a
show_visitor (LeftBinOp _ op) = do
  b <- pop; a <- pop
  let str_op = case op of {ADD -> " + "; SUB -> " - "; MUL -> " * "; DIV -> " / "}
  push $ concat ["(", a, str_op, b,")"]
show_visitor EOF = pop >>= (res .=)
show_visitor _  = throwVisitor "Broken RPN-expression!"

calc_visitor :: Token -> Visitor Int Int ()
calc_visitor (NUM a) = push a
calc_visitor (LeftBinOp _ op) = do
  b <- pop; a <- pop
  let f = case op of { ADD -> (+); SUB -> (-); MUL -> (*); DIV -> div}
  push (f a b)
calc_visitor EOF = pop >>= (res .=)
calc_visitor _  = throwVisitor "Broken RPN-expression!"


main :: IO ()
main = do
  str <- getLine
  putStrLn $ ">Getting str: "
  print str
  let (tokens, state) = runState (tokenizer str) Go
  putStrLn $ ">Tokenizer finish with state:" <> show state
  guard (state == End)
  print tokens

  putStrLn $ ">RPN:"
  rpn <- return $ either (error . show) reverse
               $ visit tokens [] parser_visitor
  print rpn

  putStrLn $ ">calc-visitor:"
  print $ visit rpn 0 calc_visitor
  putStrLn $ ">show-visitor:"
  putStrLn $ either (error . show) id
           $ visit rpn "" show_visitor
  return ()
