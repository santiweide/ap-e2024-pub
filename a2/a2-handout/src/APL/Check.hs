--  check for is that the program does not contain references to variables that are not in scope
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String
type Env = [VName]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Env -> Env
envExtend v env = v : env

envLookup :: VName -> Env -> Maybe Error
envLookup v [] = Just ("Variable not in scope: " ++ v)
envLookup v (x:xs) = if v == x then Nothing else envLookup v xs

newtype CheckM a = CheckM (Env -> Maybe a)

runCheck :: CheckM a -> Env -> Maybe a
runCheck (CheckM f) env = f env

instance Functor CheckM where
    fmap f (CheckM a) = CheckM $ \env -> fmap f (a env)

instance Applicative CheckM where
    pure x = CheckM $ \_ -> Just x
    (CheckM mf) <*> (CheckM ma) = CheckM $ \env -> 
        case mf env of
            Just f  -> fmap f (ma env)
            Nothing -> Nothing

instance Monad CheckM where
    (CheckM a) >>= f = CheckM $ \env -> 
        case a env of
            Just x  -> let (CheckM b) = f x in b env
            Nothing -> Nothing

-- if valid return an Exp
-- if invalid return Nothing
check :: Exp -> CheckM Exp
check (CstInt x) = pure (CstInt x)
check (CstBool x) = pure (CstBool x)
check (Var name) = CheckM $ \env -> 
    case envLookup name env of
        Nothing  -> Just (Var name)
        Just _ -> Nothing
check (Lambda v body) = CheckM $ \env -> 
    runCheck (check body) (envExtend v env)
check (Let v e1 e2) = do
    _ <- check e1
    CheckM $ \env -> runCheck (check e2) (envExtend v env)

check (Add e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Add e1 e2)
check (Sub e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Sub e1 e2)
check (Mul e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Sub e1 e2)
check (Div e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Sub e1 e2)
check (Pow e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Pow e1 e2)
check (Eql e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Eql e1 e2)
check (If cond e1 e2) = do
    _ <- check cond
    _ <- check e1
    _ <- check e2
    pure (If cond e1 e2)
check (Apply e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (Apply e1 e2)
check (TryCatch e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure (TryCatch e1 e2)
check (Print str e) = do
    _ <- check e
    pure (Print str e)

-- Entrance
checkExp :: Exp -> Maybe Error
checkExp e = case runCheck (check e) envEmpty of
    Just _  -> Nothing
    Nothing -> Just "Variable not in scope."
