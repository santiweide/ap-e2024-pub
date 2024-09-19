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
envLookup _ [] = Nothing
envLookup v (x:xs) = if v == x then Just v else envLookup v xs

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
check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()
check (Var name) = CheckM $ \env -> 
        case envLookup name env of
            Just _  -> Just ()
            Nothing -> Nothing

check (Lambda v body) = CheckM $ \env -> runCheck (check body) (envExtend v env)

check (Let v e1 e2) = do
    _ <- check e1
    CheckM $ \env -> runCheck (check e2) (envExtend v env)

check (Add e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Sub e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Mul e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Div e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Pow e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Eql e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (If cond e1 e2) = do
    _ <- check cond
    _ <- check e1
    _ <- check e2
    pure ()

check (Apply e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (TryCatch e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (Print _ e) = do
    _ <- check e
    pure ()

check (KvPut e1 e2) = do
    _ <- check e1
    _ <- check e2
    pure ()

check (KvGet e) = do
    _ <- check e
    pure ()

-- Entrance
checkExp :: Exp -> Maybe Error
checkExp e = case runCheck (check e) envEmpty of
    Just _  -> Nothing
    Nothing -> Just "Variable not in scope."
