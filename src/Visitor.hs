{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Visitor where

import Control.Monad.Catch
import Control.Monad.State
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Type.Reflection

data StateVisitor s r
  = MkStateVisitor { _res :: r
                   , _stack :: [s]
                   }

$(makeLenses ''StateVisitor)

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


visit :: [a]
      -> r                   -- init
      -> (a -> Visitor s r ()) -- visitor
      -> Either SomeException r     -- result
visit xs z f = _res <$> st
  where
    st = execStateT walk base_state
    base_state = MkStateVisitor z []
    walk = mapM_ f xs
