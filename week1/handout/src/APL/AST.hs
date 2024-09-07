module APL.AST
  (
     Exp (..),
     Error,
     Val (..),
     VName,
     Env
  )
where

type VName = String
type Env = [(VName, Val)]

data Exp
    = CstInt Integer
    | CstBool Bool
    | Var VName
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Pow Exp Exp
    | Eql Exp Exp
    | If Exp Exp Exp
    | Let VName Exp Exp
    | Lambda VName Exp
    | Apply Exp Exp
    | TryCatch Exp Exp
    deriving (Eq, Show)

data Val
    = ValInt Integer
    | ValBool Bool
    | ValFun Env VName Exp
  deriving (Eq, Show)

-- data Either a b = Left a
--                 | Right b




type Error = String

