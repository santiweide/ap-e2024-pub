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


-- [String] is user controlled String
newtype EvalM a = EvalM (Env -> ([String], Either Error a))
-- question1 what if 
--      newtype EvalM a String = EvalM (Env -> ([String], Either Error a))

instance Functor EvalM where
  -- fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap f (EvalM t) = EvalM $ \env ->
        let (log, x) = t env
        in case x of
            Left err -> (log, Left err)
            Right y -> (log, Right (f y))

            
instance Applicative EvalM where
  pure x = EvalM $ \_env -> ([], Right x)
  
  -- <*> :: EvalM (a -> b) -> EvalM a -> EvalM b
  (EvalM f) <*> (EvalM v) = EvalM $ \env ->
    let (log1, ff) = f env
        (log2, vv) = v env
    in case ff of
         Left err   -> (log1 ++ log2, Left err)
         Right func -> case vv of
                         Left err -> (log1 ++ log2, Left err)
                         Right val -> (log1 ++ log2, Right (func val))


instance Monad EvalM where
  -- >>= :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env ->
    let (log1, xx) = x env
    in case xx of
         Left err -> (log1, Left err)
         Right val ->
           let EvalM f' = f val
               (log2, y) = f' env
           in (log1 ++ log2, y)


-- The function evalPrint adds a string to the list of printed strings. Use this to implement the eval case for Print.

askEnv :: EvalM Env
askEnv = EvalM $ \env -> ([], Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

-- question: how to have the current [String] in the EvalM a?
-- or we have just no need for that
failure :: String -> EvalM a
failure s = EvalM $ \_env -> ([], Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    (log, Left _) -> m2 env
    (log, Right x) -> (log, Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = m envEmpty

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

evalPrint :: String -> EvalM ()
evalPrint msg = EvalM $ \env -> ([msg], Right ())

showVal :: Val -> String
showVal val = case val of
    (ValInt x) -> show x
    (ValBool x) -> show x
    (ValFun env varname x) -> "#<func>"

eval :: Exp -> EvalM Val
eval (Print str exp) = do
    val <- eval exp
    evalPrint (str ++ ": " ++ (showVal val))
    pure $ val
            
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2



