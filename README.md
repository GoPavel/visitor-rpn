# Visitor-RPN

Implements visitor pattern and tries to use it for some case:
- tokenize expression with binary operators and constants
- parse it to [Reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation)
- calc RPN
- prettify RPN as usual infix expression

## Visitor interface

```haskell

data StateVisitor s r
  = MkStateVisitor { _acc :: r     -- accumulator
                   , _stack :: [s] -- stack
                   }
type Visitor s r a = StateT (StateVisitor s r)
                            (Either SomeException) a

```

## Example

Evaluation [Reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation).

```haskell
calc_visitor :: Token -> Visitor Int Int ()
calc_visitor (NUM a) = push a
calc_visitor (LeftBinOp _ op) = do
  b <- pop; a <- pop
  let f = case op of { ADD -> (+); SUB -> (-); MUL -> (*); DIV -> div}
  push (f a b)
calc_visitor EOF = pop >>= (acc .=)
calc_visitor _  = throwVisitor "Broken RPN-expression!"
```
