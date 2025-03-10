module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalPrint,
    catch,
    failure,
    evalKvGet,
    evalKvPut,
    transaction,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  | ValCSV [[String]]  -- New CSV representation
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f
-- evalPrint "First" >> evalPrint "Second"
-- Free $ PrintOp "First" $ pure () >> Free $ PrintOp "Second" $ pure ()
-- Free $ PrintOp "First" $ pure () >>= \_ -> Free $ PrintOp "Second" $ pure ()
  
-- (Free $ TryCatchOp (transaction badPut) (evalPrint "doing m2")  >> get0)

-- Free $ h <$> TryCatchOp (transaction badPut) (evalPrint "doing m2")
-- where h x = x >>= get0

-- Free $ TryCatchOp (h transaction badPut) (h evalPrint "doing m2")

-- Free $ TryCatchOp (transaction badPut >>= get0) (evalPrint "doing m2"  >>= get0)

-- Free $ TryCatchOp (transaction badPut >>= get0) (evalPrint "doing m2"  >>= get0)
-- !!!!
-- (Free $ TryCatchOp (transaction badPut >> get0) (evalPrint "doing m2" >> get0))
-- 
-- Free EvalOp () >>= EvalM Val -- also a (Free EvalOp Val)
-- g :: EvalOp()
-- f :: EvalM Val, get0
-- Free $ h <$> g 
-- Free $ TransactionOp m (h a) = Free $ TransactionOp m (a >>= get0)
-- where h x = x >>= f
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error
  | TryCatchOp a a 
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a
  | TransactionOp (EvalM ()) a
  -- ** New CSV Operations **
  | LoadCSVOp FilePath (Val -> a)
  | SelectColumnsOp [Int] Val (Val -> a)
  | CartesianProductOp Val Val (Val -> a)
  | PermuteAndMatchOp Val (Val -> a)
  | ExistenceCheckOp Val (Val -> a)
  | CopyAndConstantOp Val (Val -> a)
  | LeftMergeOp Val Val (Val -> a)

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s m) = StatePutOp s $ f m
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (TryCatchOp a b) = TryCatchOp (f a) (f b)
  fmap f (KvGetOp key k) = KvGetOp key (f . k)
  fmap f (KvPutOp key val m) = KvPutOp key val (f m)
  fmap f (TransactionOp m a) = TransactionOp m (f a)
  -- ** CSV Operations **
  fmap f (LoadCSVOp path k) = LoadCSVOp path (f . k)
  fmap f (SelectColumnsOp cols val k) = SelectColumnsOp cols val (f . k)
  fmap f (CartesianProductOp v1 v2 k) = CartesianProductOp v1 v2 (f . k)
  fmap f (PermuteAndMatchOp v k) = PermuteAndMatchOp v (f . k)
  fmap f (ExistenceCheckOp v k) = ExistenceCheckOp v (f . k)
  fmap f (CopyAndConstantOp v k) = CopyAndConstantOp v (f . k)
  fmap f (LeftMergeOp v1 v2 k) = LeftMergeOp v1 v2 (f . k)

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGetOp $ \s -> pure s

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM a -> EvalM a -> EvalM a
catch a b = do 
  state <- getState -- stash state
  Free $ TryCatchOp 
    a 
    (do
      putState state
      b)

evalKvGet :: Val -> EvalM Val
evalKvGet key = Free $ KvGetOp key pure

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = Free $ KvPutOp key val $ pure ()

transaction :: EvalM () -> EvalM ()
transaction e = Free $ TransactionOp e $ pure ()
