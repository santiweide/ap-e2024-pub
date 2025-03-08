module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval m = let ((prints, _), res) = runEval' envEmpty [] stateInitial m
  in (prints, res) where
    runEval' :: Env -> [String] -> State -> EvalM a -> (([String], State), Either Error a)
    runEval' _ p s (Pure x) = ((p, s), pure x)
    runEval' r p s (Free (ReadOp k)) = runEval' r p s $ k r
    runEval' r p s (Free (StateGetOp k)) = runEval' r p s $ k s 
    runEval' r p _ (Free (StatePutOp s' k)) = runEval' r p s' k
    runEval' r p s (Free (PrintOp p' k)) =
      let ((ps, s'), res) = runEval' r p s k
       in ((p' : ps, s'), res)
    runEval' _ p s (Free (ErrorOp e)) = ((p, s), Left e)
    runEval' r p s (Free (TryCatchOp m1 m2)) = 
      let res = runEval' r p s m1
      in case res of
          (_, Left _) -> runEval' r p s m2
          (_, Right _) -> res
    runEval' r p s (Free (KvGetOp key k)) = 
      case lookup key s of
        Just val -> runEval' r p s $ k val
        Nothing -> ((p, s), Left $ "Key not found: " ++ show key) -- maybe should not clear the log?
    runEval' r p s (Free (KvPutOp key val k)) = 
      let s' = (key, val) : filter ((/= key) . fst) s
      in runEval' r p s' k
    -- TODO instead of runEval', use a more Monadic way
    -- runEval' r p s (Free (TransactionOp k n)) =
    --   let ((p1, s1), res1) = runEval' r p s k in
    --     case res1 of
    --       Left _ -> let 
    --           ((p2, s2), res2) = runEval' r p1 s n -- keep p1 for printed log before all failures are reserved
    --         in ((p2, s2), res2)
    --       Right _ -> let 
    --           ((p2, s2), res2) = runEval' r p1 s1 n
    --         in ((p2, s2), res2)
    
    runEval' r p s (Free (TransactionOp k n)) = 
      let ((p1, s1), res1) = runEval' r p s k in
        case res1 of
          Left _ -> runEval' r p1 s n
          Right _ ->runEval' r p1 s1 n