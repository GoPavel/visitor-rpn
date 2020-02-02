{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Visitor
  ( Visitor, StateVisitor(..), VisitorRuntimeException(..),
    throwVisitor, pop, push, visit, acc, stack, visitWithState
  ) where

import Control.Monad.Catch ( Exception, SomeException, MonadThrow, toException
                           , throwM)
import Control.Monad.State (runStateT, execStateT, StateT)
import Lens.Micro ((<&>))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (view, (%=), (.=), use)
import Type.Reflection (Typeable)

data StateVisitor s r
  = MkStateVisitor { _acc :: r     -- accumulator
                   , _stack :: [s] -- stack
                   }

$(makeLenses ''StateVisitor)

{-|
Walk around 
  type paramters:
  s - type of stack elements
  r - type of accumulative result
  a - monadic parameter
-}
type Visitor s r a = StateT (StateVisitor s r)
                            (Either SomeException) a

data VisitorRuntimeException = VisitorRuntimeException String
  deriving (Show, Typeable)
instance Exception VisitorRuntimeException


throwVisitor :: MonadThrow m => String -> m a
throwVisitor = throwM . toException . VisitorRuntimeException

pop :: Visitor s r s
pop = do
  use stack >>= \case
    []     -> throwVisitor "Pop from empty stack"
    (x:xs) -> stack .= xs >> return x

push :: Show s => s -> Visitor s r ()
push s = do
  stack %= (s :)


visit :: (Foldable t)
      => t a                     -- elements for visiting
      -> r                       -- init accumulator
      -> (a -> Visitor s r ())     -- visitor
      -> Either SomeException r  -- result
visit xs z f = st <&> view acc
  where
    st = visitWithState xs z f

visitWithState
     :: (Foldable t)
     => t a
     -> r
     -> (a -> Visitor s r ())
     -> Either SomeException (StateVisitor s r)
visitWithState xs z f = st
  where
    st = execStateT (f `mapM_` xs) base_state
    base_state = MkStateVisitor z []
