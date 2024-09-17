module APL.AST
  ( VName,
    Exp (..),
    printExp,
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
  -- TODO: add cases
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp e =
    case e of
        CstInt x -> show x 
        CstBool x -> show x
        Var x -> x
        Add e1 e2 -> "(" ++ (printExp e1) ++ ") + (" ++ (printExp e2) ++ ")"
        Sub e1 e2 -> "(" ++(printExp e1) ++ ") - (" ++ (printExp e2) ++ ")"
        Mul e1 e2 -> "(" ++(printExp e1) ++ ") * (" ++ (printExp e2) ++ ")"
        Div e1 e2 -> "(" ++(printExp e1) ++ ") / (" ++ (printExp e2) ++ ")"
        Pow e1 e2 -> "(" ++(printExp e1) ++ ") ** (" ++ (printExp e2) ++ ")"
        Eql e1 e2 -> "(" ++(printExp e1) ++ ") == (" ++ (printExp e2) ++ ")"
        If e1 e2 e3 -> "if ("++(printExp e1) ++") then ("++(printExp e2) ++ ") else ("++(printExp e3)++ ")" 
        Let name e1 e2 -> "let " ++ name ++ " = ("++(printExp e1) ++") in (" ++(printExp e2) ++ ")"
        Lambda name e1 -> "\\" ++ name ++ " -> (" ++ (printExp e1) ++ ")"
        Apply e1 e2 -> "(" ++ (printExp e1) ++ ") (" ++ (printExp e2) ++ ")" 
        TryCatch e1 e2 -> "try (" ++ (printExp e1) ++ ") catch (" ++ (printExp e2) ++ ")"


