module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    isEql
  )
where

import APL.AST (Exp (..), VName)
-- import Control.Monad (ap, liftM)

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
type State =( [String], [(Val, Val)] ) 


mergeLists :: [(Val, Val)] -> [(Val, Val)] -> [(Val, Val)]
mergeLists list1 list2 =
    let filteredList1 = filter (\(k1, _) -> notElem k1 (map fst list2)) list1
    in list2 ++ filteredList1

mergeState :: State -> State -> State
mergeState (_, kv1) (y, kv2) = (y, kv)
        where kv = mergeLists kv1 kv2

merge3 :: State -> State -> State -> State
merge3 a b c = mergeState (mergeState a b) c

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  -- fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap f (EvalM t) = EvalM $ \env state ->
        let (state1, x) = t env state
            state2 = mergeState state state1
        in case x of
            Left err -> (state2, Left err)
            Right y -> (state2, Right (f y))

            
instance Applicative EvalM where
  pure x = EvalM $ \_env state -> (state, Right x)
  -- <*> :: EvalM (a -> b) -> EvalM a -> EvalM b
  (EvalM f) <*> (EvalM v) = EvalM $ \env state ->
    let (state1, ff) = f env state
        (state2, vv) = v env state
        state4 = merge3 state state1 state2
    in case ff of
         Left err   -> (state4, Left err)
         Right func -> case vv of
                         Left err -> (state4, Left err)
                         Right val -> (state4, Right (func val))


instance Monad EvalM where
  -- >>= :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env state ->
    let (state1, res) = x env state
        state2 = mergeState state state1
    in case res of
         Left err -> (state2, Left err)
         Right val ->
           let EvalM f' = f val 
               (state3, y) = f' env state2
               state4 = mergeState state2 state3
           in (state4, y)


-- The function evalPrint adds a string to the list of printed strings. Use this to implement the eval case for Print.

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (state, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (state, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state ->
  case m1 env state of
    (state1, Left _) -> m2 env (mergeState state state1)
    (state1, Right x) -> ((mergeState state state1), Right x)

runEval :: EvalM a -> (State, Either Error a)
runEval (EvalM m) = m envEmpty ([],[])


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
evalPrint msg = EvalM $ \_ state -> (((fst state) ++ [msg], snd state), Right ())

showVal :: Val -> String
showVal val = case val of
    (ValInt x) -> show x
    (ValBool x) -> show x
    (ValFun _ _ _) -> "#<func>"

showTypeVal :: Val -> String
showTypeVal val = case val of
    (ValInt x) -> "ValInt " ++ show x
    (ValBool x) -> "ValBool " ++ show x
    (ValFun _ _ _) -> "ValFun #<func>"

evalKvGet :: Val -> EvalM Val
evalKvGet key = EvalM $ \_ state ->
    let kvs = snd state
    in case lookup key kvs of
        Just val -> (state, Right val)
        Nothing -> (state, Left ("Invalid key: " ++ (showTypeVal key)))

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = EvalM $ \_ state -> 
    let kvs = snd state
        newKvs = (key, val) : filter (\(k, _) -> k /= key) kvs
        logs = fst state
    in ((logs, newKvs), Right ())

eval :: Exp -> EvalM Val
eval (Print str e) = do
    val <- eval e
    evalPrint (str ++ ": " ++ (showVal val))
    pure $ val

eval (KvPut key value) = do
    k <- eval key
    v <- eval value
    evalKvPut k v
    pure $ v

eval (KvGet key) = do
    k <- eval key
    evalKvGet k

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


isEql :: EvalM Val -> EvalM Val -> EvalM Val
isEql evalM1 evalM2 = pure isEqlFunc <*> evalM1 <*> evalM2
  where
    isEqlFunc :: Val -> Val -> Val
    isEqlFunc (ValInt x) (ValInt y) = ValBool (x == y)
    isEqlFunc (ValBool x) (ValBool y) = ValBool (x == y)
    isEqlFunc _ _ = ValBool False

