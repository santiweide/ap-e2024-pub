module APL.Eval

  (
    envEmpty,
    eval,
    printExp
  )
where

import APL.AST (
        Exp (..),
        Error,
        Val(..),
        VName,
        Env
        )

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []
-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup _ [] = Nothing
envLookup k (x:xs) = case (fst x) == k of
    True -> Just (snd x)
    False -> envLookup k xs
-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend k v e = [(k, v)] ++ e


envMerge :: Env -> Env -> Env
envMerge e1 e2 = e1 ++ filter (\(k, _) -> k `notElem` map fst e1) e2


evalIntBinaryOpWithErr:: (Integer -> Integer -> Either Error Integer) -> Env -> Exp ->  Exp -> Either Error Val
evalIntBinaryOpWithErr f env e1 e2 = 
    case (eval env e1, eval env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (ValInt x), Right (ValInt y)) -> case f x y of
                Left err -> Left err
                Right z -> Right $ ValInt z
        (Right _, Right _) -> Left "[error] Non-integer oprand"


evalIntBinaryOp :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinaryOp f env e1 e2 = 
    evalIntBinaryOpWithErr ferr env e1 e2
        where ferr x y = Right $ f x y


eval :: Env -> Exp -> Either Error Val

eval _ (CstInt x) = Right $ (ValInt x)
eval _ (CstBool b) = Right $ ValBool $ b
eval env (Var k) = 
    case (envLookup k env) of
        Nothing -> Left $ "[error] Unkonw Variable"
        Just v -> Right $ v

eval env (Add e1 e2) = evalIntBinaryOp (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinaryOp (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinaryOp (*) env e1 e2

eval env (Div e1 e2) = evalIntBinaryOpWithErr divWithErr env e1 e2
    where
        divWithErr _ 0 = Left "[error] Division by zero"
        divWithErr x y = Right (x `div` y)

eval env (Pow e1 e2) = evalIntBinaryOpWithErr powWithErr env e1 e2
    where
        powWithErr x y = case y < 0 of
            True  -> Left $ "[error] Negative exponent"
            False -> Right (x ^ y)


eval env (Eql e1 e2) = 
    case (eval env e1, eval env e2) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
        (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
        (Right _, Right _) -> Left "[error] Invalid oprands to equality"

eval env (If cond e1 e2) = 
    case (eval env cond) of 
        Left err -> Left err
        Right (ValBool True) -> eval env e1
        Right (ValBool False) -> eval env e2
        Right _ -> Left $ "[error] cannot eval first arguement with the non-bool type"

-- set k,e2 if e1
eval env (Let k e1 e2) =
    case (eval env e1) of
        Left err -> Left err          
        Right v -> eval (envExtend k v env) e2

-- Lambda stores the unknow variable name int env
-- and then when called it will assign
eval env (Lambda k e) =  Right (ValFun env k e) 

-- e1 The function expression must evaluate to a ValFun.
-- e2 The argument expression can evaluate to an argument value of any type. 
-- here we put the lambda into env for further use while apply
eval env (Apply e1 e2) =
    case (eval env e1, eval env e2) of
        (Left err, _) -> Left err 
        (_, Left err) -> Left err
        (Right (ValFun env1 k exp1), Right (ValInt y)) -> eval (envMerge env env1) (Let k (CstInt y) exp1)
        (Right (ValFun env1 k exp1), Right (ValBool y)) -> eval (envMerge env env1) (Let k (CstBool y) exp1)     
        (Right (ValFun env1 k exp1), Right (ValFun env2 _ _)) ->
            case envLookup k (envMerge (envMerge env env1) env2) of
                Nothing  -> Right (ValFun (envMerge (envMerge env env1) env2) k exp1) 
                Just (ValInt x) -> eval (envMerge (envMerge env env1) env2) (Let k (CstInt x) exp1)
                Just (ValBool x) -> eval (envMerge (envMerge env env1) env2) (Let k (CstBool x) exp1)
                Just (ValFun _ _ _) -> Right (ValFun (envMerge (envMerge env env1) env2) k exp1) -- a func cannot apply to a func :P, so just leave it there
        (_, _) -> Left "the first expression must evaluate to a ValFun."

eval env (TryCatch e1 e2) = 
    case (eval env e1) of
        Right val -> Right val
        Left _ -> case (eval env e2) of
            Right val -> Right val
            Left err -> Left err

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


