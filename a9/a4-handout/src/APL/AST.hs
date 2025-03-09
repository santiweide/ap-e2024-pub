module APL.AST
  ( VName,
    Exp (..),
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  | Print String Exp
  | KvPut Exp Exp
  | KvGet Exp
  -- New expressions for CSV queries
  | LoadCSV FilePath
  | SelectColumns [Int] Exp
  -- | FilterRows (Exp -> Exp) Exp
  | CartesianProduct Exp Exp
  | PermuteAndMatch Exp
  | ExistenceCheck Exp
  | CopyAndConstant Exp
  | LeftMerge Exp Exp
  deriving (Eq, Show)

