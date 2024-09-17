-- tar -czvf a2-code.tar.gz a2.cabal runtests.hs src/APL/*
module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
        testPos (Let "x" (CstInt 10) (Let "y" (CstInt 10) (Add (CstInt 2) (Var "x")))),
        testNeg (Let "x" (CstInt 10) (Let "y" (CstInt 10) (Eql (Var "x") (Mul (CstInt 2) (Var "z"))))),
        testPos (Apply (Lambda "x" (Var "x")) (CstInt 2)),
        testNeg (Apply (Lambda "x" (Var "y")) (CstInt 2)),
        testNeg (Print "Output:" (Var "y")),
        testNeg (TryCatch (Var "x") (CstInt 0)),
        testCase "checkExp" $ 
          checkExp (If (Add (CstInt 1) (CstInt 2)) (Pow (CstInt 1) (CstInt 2)) (Div (CstInt 2) (Var "x")))
          @?= Just "Variable not in scope."
        
    ]

