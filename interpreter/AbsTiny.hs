

module AbsTiny where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = Program Stm
  deriving (Eq, Ord, Show, Read)

data Exp
    = ENum Integer
    | EVar Ident
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
  deriving (Eq, Ord, Show, Read)

data BExp
    = BTrue | BFalse | BLeq Exp Exp | BNeg BExp | BAnd BExp BExp
  deriving (Eq, Ord, Show, Read)

data Stm
    = SSkip
    | SAss Ident Exp
    | SIfel BExp Stm Stm
    | SWhile BExp Stm
    | SPrint Exp
    | SBlock [Stm]
  deriving (Eq, Ord, Show, Read)

