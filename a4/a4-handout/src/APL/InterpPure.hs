module APL.InterpPure (runEval) where

import APL.Monad

-- TODO add a transcation state inside?
runEval :: EvalM a -> ([String], Either Error a)
runEval m = let ((prints, states), res) = runEval' envEmpty stateInitial m
  in (prints, res) where
    runEval' :: Env -> State -> EvalM a -> (([String], State), Either Error a)
    runEval' _ s (Pure x) = (([], s), pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s 
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let ((ps, s), res) = runEval' r s m
       in ((p : ps, s), res)
    runEval' _ s (Free (ErrorOp e)) = (([], s), Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) = 
      let res = runEval' r s m1
      in case res of
          (_, Left _) -> runEval' r s m2
          (_, Right _) -> res
    runEval' r s (Free (KvGetOp key k)) = 
      case lookup key s of
        Just val -> runEval' r s $ k val
        Nothing -> (([], s), Left $ "Key not found: " ++ show key)
    runEval' r s (Free (KvPutOp key val m)) = 
      let s' = (key, val) : filter ((/= key) . fst) s
      in runEval' r s' m
    runEval' r s (Free (TransactionOp m n)) =
      let ((print1, state1), res) = runEval' r s m in
        case res of
          Left e -> let 
              ((print2, state2), res') = runEval' r s n
              print3 = print1 ++ print2
            in ((print3, state2), res')
          Right _ -> let 
              ((print2, state2), res') = runEval' r state1 n
              print3 = print1 ++ print2
            in ((print3, state2), res')
    