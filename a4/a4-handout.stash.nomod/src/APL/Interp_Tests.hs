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
  testGroup -- TODO implement some pure interpreter?
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
        testCase "catch-failure-on-exp1" $ do
           (out, res) <-
             captureIO [] $
               runEvalIO $ do -- expected EvalM Val, aka EvalOp (Free EvalOp Val)  by type EvalM a = Free EvalOp a
                  catch (failure "Oh no!") (pure "Success!")
           (out, res) @?= ([],Right "Success!"),
        testCase "trycatch-failure-on-exp1" $ do
           let badEql = CstInt 0 `Eql` CstBool True
               divZero = CstInt 1 `Div` CstInt 0
           (out, res) <-
             captureIO [] $
               evalIO' $ 
                 TryCatch badEql divZero
           (out, res) @?= ([], Left "Division by zero"),
        testCase "trycatch-succeed-on-exp1" $ do
           let divZero = CstInt 1 `Div` CstInt 0
           (out, res) <-
             captureIO [] $
               evalIO' $ 
                 TryCatch (CstInt 5) divZero
           (out, res) @?= ([],Right (ValInt 5)),
        testCase "kv-vanilla: kv is a k-ranged operation" $ do
           let put0 m = Free $ KvPutOp (ValInt 0) (ValInt 1) m
               put1 m = Free $ KvPutOp (ValInt 1) (ValInt 1) m
               put2 m = Free $ KvPutOp (ValInt 0) (ValInt 100) m
               get0 = Free $ KvGetOp (ValInt 0) $ \val -> pure val
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                 put0 $ put1 $ put2 $ get0
           (out, res) @?= ([],Right (ValInt 100)),
        testCase "state-vanilla: state is a global-ranged operation" $ do
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                  Free $ StatePutOp [(ValInt 10, ValInt 2)] $ Free $ StatePutOp [(ValInt 1, ValInt 2)] $ getState
           (out, res) @?= ([],Right [(ValInt 1,ValInt 2)]),
        testCase "state X kv, state flush" $ do
           let sput0 m = Free $ StatePutOp [(ValInt 0, ValInt 2)] m
               kput0 m = Free $ KvPutOp (ValInt 0) (ValInt 1) m
               sput1 m = Free $ StatePutOp [(ValBool True, ValInt 2)] m
           (out, res) <-
             captureIO [] $
               runEvalIO $ do 
                  sput0 $ kput0 $ sput1 $ getState
           (out, res) @?= ([],Right [(ValBool True,ValInt 2)]),
        testCase "Missing key Founded" $ do
            (_, res) <-
              captureIO ["ValInt 1"] $
                runEvalIO $
                  Free $ StatePutOp [(ValInt 1, ValInt 2)] $ 
                    Free $ KvGetOp (ValInt 0) $ \val -> pure val
            res @?= Right (ValInt 2),
        testCase "Missing key Not Found" $ do
            (_, res) <-
              captureIO ["xx"] $
                runEvalIO $
                  Free $ StatePutOp [(ValInt 1, ValInt 2)] $ 
                    Free $ KvGetOp (ValBool False) $ \val -> pure val
            res @?= Left "Invalid value input: xx"
    ]
