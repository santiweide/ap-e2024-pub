module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
        testCase "printExp(lambda)" $
          printExp (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (Let "x" (CstInt 2) (Lambda "y" (Sub (Var "x") (Var "y")))) )
            @?= "(\\y -> ((x) + (y))) (let x = (2) in (\\y -> ((x) - (y))))",
        --  
        testCase "printExp(arith)" $
          printExp  (Div (Pow (Mul (CstInt 1) (CstInt 2)) (CstInt 3)) (CstInt 3))
            @?= "(((1) * (2)) ** (3)) / (3)"  ,
        -- 
        testCase "printExp(bool)" $
          printExp (TryCatch (CstBool True) (If (Eql (CstBool True) (CstBool True))  (Let "x" (CstBool False) (Eql (Var "y") (CstBool True))) (Let "x" (CstBool True) (Eql (Var "y") (CstBool True))) ))
            @?= "try (True) catch (if ((True) == (True)) then (let x = (False) in ((y) == (True))) else (let x = (True) in ((y) == (True))))" 
    ]
