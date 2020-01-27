{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser
  ( Token(..), BinOp(..), TokenizerState(..), tokenizer, parser_visitor
  ) where

import Control.Monad.State
import Data.Char (isDigit, digitToInt, isSpace)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import Debug.Trace

import Visitor (Visitor, res, stack, pop, push, visit, throwVisitor)

data Token
  = LEFT | RIGHT | EOF
  | LeftBinOp Int BinOp
  | NUM Int
  deriving (Show, Eq)

data TokenizerState
  = Number Int
  | Go
  | Error String
  | End
  deriving (Show, Eq)

data BinOp
  = ADD | MUL | DIV | SUB
  deriving (Show, Eq)

withNewState :: b -> State b a -> State b a
withNewState v = withState (const v)

withNewStateT :: Monad m => b -> StateT b m a -> StateT b m a
withNewStateT v = withStateT (const v)

infixl 4 <:>
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x xs = (x :) <$> xs

-- TODO: rewrite tokenizer to visitor
tokenizer :: String -> State TokenizerState [Token]
tokenizer (x:xs) = do
  get >>= \case
    Number a -> if isDigit x
      then do
        put $ Number $ a * 10 + digitToInt x
        tokenizer xs
      else
        withNewState Go $
          NUM a <:> tokenizer (x:xs)
    Go ->
      if isDigit x then
        withNewState (Number 0) $
          tokenizer (x:xs)
      else if isSpace x then
        tokenizer xs
      else case x of
          '(' -> LEFT  <:> tokenizer xs
          ')' -> RIGHT <:> tokenizer xs
          '+' -> LeftBinOp 1 ADD  <:> tokenizer xs
          '-' -> LeftBinOp 1 SUB <:> tokenizer xs
          '*' -> LeftBinOp 2 MUL <:> tokenizer xs
          '/' -> LeftBinOp 2 DIV <:> tokenizer xs
          c   -> withNewState (Error $ "Unexpected symbol: " <> show c) $
                  return []
    Error msg -> return []
    End -> withNewState (Error "Unexpected end of expression") $ return []
tokenizer [] = do
  get >>= \case
    Error _  -> return []
    Number a -> withNewState End $ return [NUM a, EOF]
    _        -> withNewState End $ return [EOF]


parser_visitor :: Token -> Visitor Token [Token] ()
parser_visitor cur = case cur of
    NUM a -> res %= (NUM a :)
    LeftBinOp p op -> do
      findLeftArg cur
      push cur
    LEFT  -> push LEFT
    RIGHT -> findLeft
    EOF   -> popAllOperator >> res %= (EOF :)
  where
    findLeftArg :: Token -> Visitor Token [Token] ()
    findLeftArg cur@(LeftBinOp preced _)  = do
      st <- use stack
      let top = st ^? _head
      case top of
        Just (LeftBinOp p op) ->
          when (p >= preced) $ do
            top <- pop
            res %= (top :)
            findLeftArg cur
        _ -> return ()

    findLeft :: Visitor Token [Token] ()
    findLeft = do
      st <- use stack
      -- traceM $ show st
      when (not $ has _head st) $ throwVisitor "Mismatch parentheses"
      pop >>= \case
        LEFT  -> return ()
        token -> res %= (token :) >> findLeft

    popAllOperator :: Visitor Token [Token] ()
    popAllOperator = do
      st <- use stack
      case st ^? _head of
        Just (LeftBinOp p op) -> do
            top <- pop
            res %= (top :)
            popAllOperator
        Nothing -> return ()
        Just _ -> throwVisitor "Mismatch parentheses in end"
