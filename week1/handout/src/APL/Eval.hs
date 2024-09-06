module APL.Eval

  (
    envEmpty,
    eval,
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


eval env (Eql e1 e2) = case (eval env (Sub e1 e2) ) of
    Left err -> Left err
    Right (ValInt 0) -> Right (ValBool True)
    Right _ -> Right (ValBool False)


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


