module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    EvalM,
    envEmpty,
    failure
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

newtype EvalM a = EvalM (Either Error a)

-- fmap :: (a->b) -> f a -> f b
instance Functor EvalM where
    fmap f (EvalM (Left err)) = EvalM (Left err)
    fmap f (EvalM (Right x)) = EvalM (Right (f x))

-- pure :: a -> f a
-- <*> :: f a -> (f a->b) -> f b)
instance Applicative EvalM where
    pure x = EvalM $ Right x
    EvalM (Left e) <*>  _  = EvalM (Left e)
    _              <*> EvalM (Left e) = EvalM (Left e)
    EvalM (Right f) <*> EvalM (Right x) = EvalM (Right (f x))

-- f a -> (a -> f b) -> f b
-- EvalM a -> (a -> EvalM b) -> EvalM b
-- EvalM Bool -> (Bool -> EvalM Int) -> EvalM Int
-- 大概是一个Bool转Int的函数可能会用到Monads
-- 逐渐具体化似乎可以帮助理解
-- Eval env->true >>= trans env->1 = Eval \env 
instance Monad EvalM where
    EvalM x >>= f = EvalM $ case x of 
        Left err -> Left err
        Right x' -> let EvalM y = f x' in y


failure :: String -> EvalM a
failure str = EvalM (Left str)

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

-- Monadic evaluation
-- Implement the eval function to make use of EvalM monad
-- You should not directly construct or destruct EvalM values in the eval function
-- leave that to the Monad instance and the failure function
-- When finished, we could use runEval (eval [] e) to evaluate an expression e
-- Remember to add appropriate tests to APL_tests.hs
eval :: Env -> Exp -> EvalM Val
eval env (CstInt x) = pure $ ValInt x -- pure其实是可以少写一次外层类型
eval env (CstBool x) = pure $ ValBool x -- 这里的外层类型是EvalM也就是Reader

eval env (Var v) = do
    case (envLookup v env) of
        Just x -> pure x
        Nothing -> failure $ "Unk: " ++ v



eval env (Add e1 e2) = eval env e1 >>= \x -> 
                    eval env e2 >>= \y ->
                        case (x, y) of 
                        (ValInt xx, ValInt yy) -> pure $ ValInt $ (xx + yy)
                        _ -> failure "NInt opr"


eval env (Pow e1 e2) = eval env e1 >>= \x ->
                    eval env e2 >>= \y ->
                        case (x, y) of
                            (ValInt xx, ValInt yy) -> 
                                case yy < 0 of
                                    True -> failure "Neg Pow"
                                    False -> pure $ ValInt $ (xx ^ yy)
                            _ -> failure "NInt opr"


eval env (If cond e1 e2) = eval env cond >>= \c ->
                            case c of 
                                (ValBool x) -> case x of
                                    True -> eval env e1
                                    False -> eval env e2
                                _ -> failure "NBool opr"
eval env (Let v e1 e2) = eval env e1 >>= \x ->
                    (extend v env (ValInt x)) e2
-- question: how to give out Error in this expression?
-- it seems to be pured..


-- Here it comes an applicative style try-catch~~~
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ 
    case m1 of
        Left _ -> m2
        Right x -> Right x

eval env (TryCatch e1 e2) =
    eval env e1 `catch` eval env e2


-- adding an environment to EvalM
-- Factor error handling into Eval itself hassignificantly reduced the amount of boilier plate code in eval, but there is still one piece left!
-- the explicit passing of the environment when evaluating subexpressions
-- We are trying to extend EvalM to maintain an environment, similiar to Reader monad~~






                        
    





