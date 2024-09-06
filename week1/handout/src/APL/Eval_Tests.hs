module APL.Eval_Tests (tests) where

import APL.AST (
    Exp (..),
    Val(..),
        )
import APL.Eval (
    envEmpty,
    eval
        )


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
        testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
        --
        testCase "Add (wrong type)" $
          eval envEmpty (Add (CstInt 2) (CstBool True))
            @?= Left "[error] Non-integer oprand",
        --
        testCase "Add (var)" $
          eval envEmpty (Add (Let "x" (CstInt 6) (Var "x"))  (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "y")) )
            @?= Left "[error] Unkonw Variable",
        --
        testCase "Sub" $

          eval envEmpty (Sub (CstInt 2) (CstInt 5))
            @?= Right (ValInt (-3)),
        --
        testCase "Div" $
          eval envEmpty (Div (CstInt 7) (CstInt 3))
            @?= Right (ValInt 2),
        --
        testCase "Div0" $
          eval envEmpty (Div (CstInt 7) (CstInt 0))
            @?= Left "[error] Division by zero",
        --
        testCase "Mul" $
          eval envEmpty (Mul (CstInt 7) (CstInt 0))
            @?= Right (ValInt 0),
        --
        testCase "Pow" $
          eval envEmpty (Pow (CstInt 2) (CstInt 3))
            @?= Right (ValInt 8),
        --
        testCase "Pow0" $
          eval envEmpty (Pow (CstInt 2) (CstInt 0))
            @?= Right (ValInt 1),
        --
        testCase "Pow (negative)" $
          eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
            @?= Left "[error] Negative exponent",
        --
        testCase "Eql (false)" $
          eval envEmpty (Eql (CstInt 2) (CstInt 3))
            @?= Right (ValBool False),
        --
        testCase "Eql (true)" $
          eval envEmpty (Eql (CstInt 2) (CstInt 2))
            @?= Right (ValBool True),
        --
        testCase "Eql (error)" $
          eval envEmpty (Eql (Div (CstInt 7) (CstInt 0)) (CstInt 2))
            @?= Left "[error] Division by zero",
        --
        testCase "If (true)" $
          eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
            @?= Right (ValInt 2),
        --
        testCase "If (false)" $
          eval envEmpty (If (CstBool False) (CstInt 2) (Div (CstInt 7) (CstInt 2)))
            @?= Right (ValInt 3),
        --
        testCase "If (Non-Bool)" $
          eval envEmpty (If (CstInt 1) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
            @?= Left "[error] cannot eval first arguement with the non-bool type",
        --
        testCase "If (error)" $
          eval envEmpty (If (Div (CstInt 7) (CstInt 0)) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
            @?= Left "[error] Division by zero",
        --
        testCase "Let" $
          eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
            @?= Right (ValInt 5),
        -- 
        testCase "Let (Error)" $
          eval envEmpty (Let "x" (Div (CstInt 7) (CstInt 0)) (Var "x"))
            @?= Left "[error] Division by zero",
        -- 
        testCase "Let (shadowing)" $
          eval
            envEmpty
            ( Let
                "x"
                (Add (CstInt 2) (CstInt 3))
                (Let "x" (CstBool True) (Var "x"))
            )
          @?= Right (ValBool True),
        --
        testCase "getVar" $
          eval envEmpty (Var "y")
          @?= Left "[error] Unkonw Variable"
        
    ]
