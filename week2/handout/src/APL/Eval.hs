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

-- the three introduced methods are for the function combined definations)
newtype EvalM a = EvalM (Env -> Either Error a))

instance Functor EvalM where
    -- fmap :: (a->b) -> a -> b
    fmap (Left err) = Left err
    fmap (Right EvalM a) = Right (fmap EvalM a)

instance Applicative EvalM where
    -- pure :: a -> f a
    pure x = Right (EvalM x)
    -- (<*>) :: f (a -> b) -> f a -> f b 
    (Right (EvalM a)) <*> (Right (EvalM b)) = EvalM (a <*> b)
    _ <*> _ = Left "invalid evalm"
    
instance Monad EvalM wherewq
    -- (>>=) :: m a -> (a -> m b) -> m b
    Left err >>= _ = Left err
    Right EvalM a >>= f = Right f (EvalM a)


runEval :: EvalM a -> Either Error a
runEval = 


eval :: Env -> Exp -> EvalM Val
eval = undefined -- TODO
