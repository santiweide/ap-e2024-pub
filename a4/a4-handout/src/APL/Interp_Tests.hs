module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        testCase "print 2" $ do
           (out, res) <-
             captureIO [] $
               evalIO' $ -- expected type ‘Exp’
                 Print "This is also 1" $
                   Print "This is 1" $
                     CstInt 1
           (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
        testCase "Pure-catch-failure-on-exp1" $ 
            (runEval $ do catch (failure "Oh no!") (pure "Success!"))
            @?= ([],Right "Success!"),
        testCase "Pure-m1m2-invisible" $ 
            let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
            in  (runEval $ (Free $ TryCatchOp (badPut) (evalPrint "doing m2") ) >> getState)
            @?= (["doing m2"],Right []),
        testCase "IO-catch-failure-on-exp1" $ do
           (out, res) <-
             captureIO [] $
               runEvalIO $ do -- expected EvalM Val, aka EvalOp (Free EvalOp Val)  by type EvalM a = Free EvalOp a
                  catch (failure "Oh no!") (pure "Success!")
           (out, res) @?= ([],Right "Success!"),
        testCase "IO-trycatch-failure-on-exp1" $ do
           let badEql = CstInt 0 `Eql` CstBool True
               divZero = CstInt 1 `Div` CstInt 0
           (out, res) <-
             captureIO [] $
               evalIO' $ 
                 TryCatch badEql divZero
           (out, res) @?= ([], Left "Division by zero"),
        testCase "IO-trycatch-succeed-on-exp1" $ do
           let divZero = CstInt 1 `Div` CstInt 0
           (out, res) <-
             captureIO [] $
               evalIO' $ 
                 TryCatch (CstInt 5) divZero
           (out, res) @?= ([],Right (ValInt 5)),
        testCase "IO-trycatch-m1 m2 visible" $ do
           let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
           (out, res) <-
             captureIO [] $
                 runEvalIO $ 
                  (Free $ TryCatchOp (badPut) (evalPrint "doing m2") ) >> getState
           (out, res) @?= (["doing m2"],Right [(ValInt 0,ValBool False)]),
        testCase "IO-trycatch-m1 m2 invisible-transaction" $ do
           let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
           (out, res) <-
             captureIO [] $
                 runEvalIO $ 
                  (Free $ TryCatchOp (transaction badPut) (transaction (evalPrint "doing m2") ) ) >> getState
           (out, res) @?= ([],Right []),
        testCase "Pure-kv-vanilla: kv is a k-ranged operation" $
           let put0 m = Free $ KvPutOp (ValInt 0) (ValInt 1) m
               put1 m = Free $ KvPutOp (ValInt 1) (ValInt 2) m
               put2 m = Free $ KvPutOp (ValInt 0) (ValInt 100) m
               get0 = Free $ KvGetOp (ValInt 0) $ \val -> pure val
            in (runEval $ do put0 $ put1 $ put2 $ get0)
           @?= ([],Right (ValInt 100)),
        testCase "IO-kv-vanilla: kv is a k-ranged operation" $ do
           let put0 m = Free $ KvPutOp (ValInt 0) (ValInt 1) m
               put1 m = Free $ KvPutOp (ValInt 1) (ValInt 1) m
               put2 m = Free $ KvPutOp (ValInt 0) (ValInt 100) m
               get0 = Free $ KvGetOp (ValInt 0) $ \val -> pure val
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                 put0 $ put1 $ put2 $ get0
           (out, res) @?= ([],Right (ValInt 100)),
        testCase "IO-state-vanilla: state is a global-ranged operation" $ do
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                  Free $ StatePutOp [(ValInt 10, ValInt 2)] $ Free $ StatePutOp [(ValInt 1, ValInt 2)] $ getState
           (out, res) @?= ([],Right [(ValInt 1,ValInt 2)]),
        testCase "IO-state X kv, state flush" $ do
           let sput0 m = Free $ StatePutOp [(ValInt 0, ValInt 2)] m
               kput0 m = Free $ KvPutOp (ValInt 0) (ValInt 1) m
               sput1 m = Free $ StatePutOp [(ValBool True, ValInt 2)] m
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                  sput0 $ kput0 $ sput1 $ getState
           (out, res) @?= ([],Right [(ValBool True,ValInt 2)]),
        testCase "IO-Missing key Founded" $ do
            (_, res) <-
              captureIO ["ValInt 1"] $
                runEvalIO $
                  Free $ StatePutOp [(ValInt 1, ValInt 2)] $ 
                    Free $ KvGetOp (ValInt 0) $ \val -> pure val
            res @?= Right (ValInt 1),
        testCase "IO-Missing key Not Found" $ do
            (_, res) <-
              captureIO ["xx"] $
                runEvalIO $
                  Free $ StatePutOp [(ValInt 1, ValInt 2)] $ 
                    Free $ KvGetOp (ValBool False) $ \val -> pure val
            res @?= Left "Invalid value input: xx",
        testCase "Pure-transaction-vanilla-good" $
            let goodPut = evalKvPut (ValInt 0) (ValInt 1)
                get0    = KvGet (CstInt 0)
            in (runEval $ transaction goodPut >> eval get0)
            @?= ([], Right (ValInt 1)),
        testCase "Pure-transaction-vanilla-bad" $
            let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
                get0    = KvGet (CstInt 0)
            in (runEval $ transaction badPut >> eval get0)
            @?= ([], Left "Key not found: ValInt 0"),
        testCase "Pure-transaction-vanilla-print" $ 
            (runEval $ transaction $ evalPrint "weee" >> evalPrint "arr" >> evalPrint "woof" >> failure "oh shit")
            @?= (["weee", "arr", "woof"],Right ()),
        testCase "Pure-transaction X transaction print: bad but continue printing, and no state change" $
            let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
            in (runEval $ transaction badPut >> evalPrint "weee" >> transaction badPut >> evalPrint "arr" >> transaction badPut >> evalKvGet (ValInt 0))
            @?= (["weee","arr"],Left "Key not found: ValInt 0"),
        testCase "IO-bad transaction indie and missing key println" $ do
            let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
            (out, res) <-
              captureIO ["xx"] $
                runEvalIO $ 
                  transaction badPut >> evalPrint "weee" >> transaction badPut >> evalPrint "arr" >> transaction badPut >> evalKvGet (ValInt 0)
            (out, res) @?= (["weee","arr","Invalid key: ValInt 0. Enter a replacement: "],Left "Invalid value input: xx"),
        testCase "IO-bad transaction inside print and missing key println" $ do
            let badPut  = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
            (out, res) <-
              captureIO ["xx"] $
                runEvalIO $ 
                  transaction (badPut >> evalPrint "weee" >> badPut >> evalPrint "arr" >> badPut) >> evalKvGet (ValInt 0)
            (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: "],Left "Invalid value input: xx"),
        testCase "IO-good transaction and update the database" $ do
            let goodPut0 = evalKvPut (ValInt 0) (ValBool False)
                goodPut1 = evalKvPut (ValInt 1) (ValBool True)
            (out, res) <-
              captureIO ["xx"] $
                runEvalIO $ 
                  transaction (goodPut0 >> evalPrint "weee" >> goodPut1 >> evalPrint "arr") >> getState
            (out, res) @?= (["weee", "arr"], Right [(ValInt 1,ValBool True),(ValInt 0,ValBool False)]),
        testCase "IO-transaction with State and KV" $ do
            let goodPutS0 = putState [(ValInt 0, ValInt 1)]
                goodPutS1 = putState [(ValInt 2, ValInt 3)]
            (out, res) <-
              captureIO ["xx"] $
                runEvalIO $ 
                  transaction (goodPutS0 >> evalPrint "weee" >> goodPutS1 >> evalPrint "arr") >> getState
            (out, res) @?= (["weee", "arr"], Right [(ValInt 2,ValInt 3)])
    ]
