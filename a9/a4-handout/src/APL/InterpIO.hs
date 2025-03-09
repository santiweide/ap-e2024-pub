module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

import Data.List (sort)              -- Import `sort` from Data.List
import Data.List.Split (splitOn)      -- Import `splitOn` from Data.List.Split

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
    --  runEvalIO clears the database on each execution, 
    --  so database values will not persist between invocations of runEvalIO.
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do 
      res <- readDB db -- state is in IO, so no need to append to params
      case res of
        Left err -> pure $ Left err
        Right dbState -> runEvalIO' r db (k dbState)

    runEvalIO' r db (Free (StatePutOp s m)) = do 
      writeDB db s
      runEvalIO' r db m
 
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p -- print to IO...no need to append to the params.
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      res <- runEvalIO' r db m1
      case res of
        Left _ -> runEvalIO' r db m2 -- TODO need to roll back if res1 failed, should we modify it?
        Right val -> pure $ Right val
    runEvalIO' r db (Free (KvGetOp key k)) = do
      res <- readDB db
      case res of
        Left err -> pure $ Left err
        Right dbState -> case lookup key dbState of
          Just val -> runEvalIO' r db (k val) -- continue further operation by passing as a param
          Nothing -> do
            input <- prompt ("Invalid key: " ++ show key ++ ". Enter a replacement: ")
            let readval = readVal input -- deserialize
            case readval of
              Just val -> runEvalIO' r db $ k val
              Nothing -> pure $ Left $ "Invalid value input: " ++ input
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      res <- readDB db
      case res of
        Left err -> pure $ Left err
        Right dbState -> do
          let dbState' = (key, val) : filter ((/= key) . fst) dbState
          writeDB db dbState'
          runEvalIO' r db m
    -- TODO should we consider copyDB failure inside copyDB? because there is a readDB and writeDB
    runEvalIO' r dbPath (Free (TransactionOp m n)) =
      withTempDB $ \tempDB -> do
        copyDB dbPath tempDB
        res1 <- runEvalIO' r tempDB m
        case res1 of
          Left _ -> do
            runEvalIO' r dbPath n -- nothing happens
          Right _ -> do
            copyDB tempDB dbPath -- update to origin db before exiting the withTempDB
            runEvalIO' r dbPath n
    -- Handle CSV File Loading
    runEvalIO' r dbPath (Free (LoadCSVOp filePath k)) = do
      content <- readFile filePath
      let rows = map (splitOn ",") (lines content)
      runEvalIO' r dbPath (k (ValCSV rows))

    -- Select specific columns from a CSV file
    runEvalIO' r dbPath (Free (SelectColumnsOp indices (ValCSV rows) k)) =
      let selectedRows = [ [row !! i | i <- indices, i < length row] | row <- rows ]
      in runEvalIO' r dbPath (k (ValCSV selectedRows))

    -- Perform Cartesian Product
    runEvalIO' r dbPath (Free (CartesianProductOp (ValCSV a) (ValCSV b) k)) =
      let productRows = [x ++ y | x <- a, y <- b]
      in runEvalIO' r dbPath (k (ValCSV productRows))

    -- Permute and Match
    runEvalIO' r dbPath (Free (PermuteAndMatchOp (ValCSV rows) k)) =
      let matchedRows = [ [a3, a1] | [a1, a2, a3] <- rows, a1 == a2 ]
      in runEvalIO' r dbPath (k (ValCSV (sort matchedRows)))

    -- Existence Check
    runEvalIO' r dbPath (Free (ExistenceCheckOp (ValCSV rows) k)) =
      let filteredRows = [ [a1, a2] | [a1, a2] <- rows, not (null a2) ]
      in runEvalIO' r dbPath (k (ValCSV (sort filteredRows)))

    -- Copy and Constant
    runEvalIO' r dbPath (Free (CopyAndConstantOp (ValCSV rows) k)) =
      let newRows = [ [a1, "foo", a1] | [a1] <- rows ]
      in runEvalIO' r dbPath (k (ValCSV (sort newRows)))

    -- Left Merge
    runEvalIO' r dbPath (Free (LeftMergeOp (ValCSV p) (ValCSV q) k)) =
      let mergedRows = [ [p1, if null p2 then q2 else p2, if null p3 then q3 else p3, if null p4 then q4 else p4]
                          | [p1, p2, p3, p4] <- p, [q1, q2, q3, q4] <- q, p1 == q1]
      in runEvalIO' r dbPath (k (ValCSV (sort mergedRows)))