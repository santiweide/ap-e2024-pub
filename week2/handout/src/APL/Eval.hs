module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

failure :: String -> EvalM a
failure str = EvalM $ \env -> Left str

newtype EvalM a = EvalM ( Env -> Either Error a)

instance Functor EvalM where
    -- fmap :: (a -> b) -> (f a) -> (f b)
    fmap f (EvalM a) = EvalM $ \env -> fmap f (a env)

instance Applicative EvalM where
    -- pure a = f a
    -- <*> :: (f a) -> (f a->b) -> (f b)
    pure a = EvalM $ \env -> Right a
    (EvalM f) <*> (EvalM a) = EvalM $ \env -> case (f env) of
        Left err -> Left err
        Right f' -> case (a env) of
            Left err -> Left err
            Right a' -> Right (f' a')

    -- EvalM $ \env -> (Right $ f) (Right $ a)

instance Monad EvalM where
    -- >>= :: (f a) -> (a -> f b) -> (f b)
   (EvalM m) >>= f = EvalM $ \env -> 
        case (m env) of
            Left error -> Left error
            Right m' -> let (EvalM n) = f m' in n env
-- question: why cannot we just return f m' but the EvalM n = f m', => n
-- 因为我们不希望返回的结果中还包括env作为参数的东西，我们希望在case 中是narrow down的，即使用我们最开始定义的env来narrow down into -> n env
-- Here our type b is Either Error a
--      and type a is EvalM (Env -> Either Error a)
-- That's rationable!!


runEval :: EvalM a -> Either Error a
runEval (EvalM a)  = a envEmpty

-- like "get a env"
-- when executing EvalM (monad action), askEnv will provide an environment.
askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

-- localEnv, like a "put a env"
-- Temporarily modifies the environment for the duration of a computation.
-- Just a SnapShot
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv modifyEnv (EvalM m) = EvalM $ \env -> m (modifyEnv env)



catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

binOpWrapper :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
binOpWrapper f x y = do
    xx <- eval x
    yy <- eval y
    case (xx, yy) of
        (ValInt x', ValInt y') -> ValInt <$> (f x' y')
        (_, _) -> failure "NonInt Opr"

binOpWrapper' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
binOpWrapper' f x y = 
    binOpWrapper f' x y 
        where f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ (ValInt x)
eval (CstBool x) = pure $ (ValBool x)
eval (Var x) = do
    env <- askEnv
    case envLookup x env of
        Nothing -> failure ("Unk var " ++ x)
        Just x' -> pure x' -- get x' for calling Var x

eval (Add e1 e2) = binOpWrapper' (+) e1 e2
eval (Sub e1 e2) = binOpWrapper' (-) e1 e2
eval (Mul e1 e2) = binOpWrapper' (*) e1 e2

eval (Div e1 e2) = binOpWrapper divWithErr e1 e2
    where 
        divWithErr _ 0 = failure "Div by 0"
        divWithErr x y = pure $ (x `div` y)


        
eval (Pow e1 e2) = binOpWrapper powWithErr e1 e2
        where powWithErr x y = if y < 0 then failure "power Neg" else pure $ (x ^ y)

eval (Eql e1 e2) = do
    x <- eval e1
    y <- eval e2
    case (x, y) of
        (ValInt a, ValInt b) -> pure $ ValBool (a == b)
        (ValBool a, ValBool b) -> pure $ ValBool (a == b)
        (_, _) -> failure "Non Opr"

eval (If cond e1 e2) = do
    cond' <- eval cond
    case cond' of
        ValBool True -> eval e1
        ValBool False -> eval e2
        _ -> failure "Non Opr for cond"

eval (Let var e1 e2) = do
    x <- eval e1
    localEnv (envExtend var x) $ eval e2

eval (Lambda var body) = do
    env <- askEnv
    pure $ ValFun env var body

eval (Apply e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (ValFun f_env var body, arg) ->
                    localEnv (const $ envExtend var arg f_env) $ eval body
        (_, _) -> failure "Cannot apply non-function"

eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2

