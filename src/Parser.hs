{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser
  ( Token(..), BinOp(..), TokenizerState(..), tokenize, parse_rpn
  ) where

import Control.Monad.Catch (SomeException)
import Control.Monad.State (StateT, State, when, withStateT, withState)
import Data.Char           (isDigit, digitToInt, isSpace)
-- import Debug.Trace
import Lens.Micro          (has, _head, (^.), (^?))
import Lens.Micro.Mtl      (use, (%=), (.=))
import Lens.Micro.TH       (makeLenses)

import Visitor (Visitor, acc, stack, pop, push, visit, throwVisitor
               , visitWithState)

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

tokenVisitor :: Char -> Visitor Token TokenizerState ()
tokenVisitor ch = do
  use acc >>= \case
    Go -> if isDigit ch
         then do
           acc .= (Number 0)
           tokenVisitor ch
         else if isSpace ch
         then return ()
         else case ch of
           '(' -> push LEFT
           ')' -> push RIGHT
           '+' -> push (LeftBinOp 1 ADD)
           '-' -> push (LeftBinOp 1 SUB)
           '*' -> push (LeftBinOp 2 MUL)
           '/' -> push (LeftBinOp 2 DIV)
           c   -> throwVisitor $ "Unexpected symbol: " <> show c
    Number a -> if isDigit ch
         then acc .= Number (a * 10 + digitToInt ch)
         else do
           acc .= Go
           push $ NUM a
    End -> throwVisitor "Unexpected end of expression"

tokenize :: String -> Either SomeException [Token]
tokenize xs = do
  st <- visitWithState xs Go tokenVisitor
  let rest =
        case st ^. acc of
          Number a -> [EOF, NUM a]
          _ -> [EOF]
  return $ reverse
         $ rest ++ (st ^. stack)

parser_visitor :: Token -> Visitor Token [Token] ()
parser_visitor cur = case cur of
    NUM a -> acc %= (NUM a :)
    LeftBinOp p op -> do
      findLeftArg cur
      push cur
    LEFT  -> push LEFT
    RIGHT -> findLeft
    EOF   -> popAllOperator >> acc %= (EOF :)
  where
    findLeftArg :: Token -> Visitor Token [Token] ()
    findLeftArg cur@(LeftBinOp preced _)  = do
      st <- use stack
      let top = st ^? _head
      case top of
        Just (LeftBinOp p op) ->
          when (p >= preced) $ do
            top <- pop
            acc %= (top :)
            findLeftArg cur
        _ -> return ()

    findLeft :: Visitor Token [Token] ()
    findLeft = do
      st <- use stack
      -- traceM $ show st
      when (not $ has _head st) $ throwVisitor "Mismatch parentheses"
      pop >>= \case
        LEFT  -> return ()
        token -> acc %= (token :) >> findLeft

    popAllOperator :: Visitor Token [Token] ()
    popAllOperator = do
      st <- use stack
      case st ^? _head of
        Just (LeftBinOp p op) -> do
            top <- pop
            acc %= (top :)
            popAllOperator
        Nothing -> return ()
        Just _ -> throwVisitor "Mismatch parentheses in end"

parse_rpn :: [Token] -> Either SomeException [Token]
parse_rpn tokens = reverse <$> visit tokens [] parser_visitor
