module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval, isEql)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Either Error Val
eval' = \e -> 
    let (_, result) = runEval (eval e) 
    in result

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= Right (ValInt 16),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= Right (ValBool True),
      --
      testCase "TryCatch(e1-effect-visible)" $
        runEval (eval (TryCatch (Let "x" (CstInt 10) (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)) ))
          @?= ([],Right (ValInt 1)),
      --
      testCase "TryCatch(e1-effect-env-invisible)" $
        runEval (eval (TryCatch ( Let "x" (Div (CstInt 2) (CstInt 1)) (Let "y" (CstBool True) (Div (CstInt 2) (CstInt 0))) ) (Var "x")))
          @?= ([],Left "Unknown variable: x"),
      --
      testCase "Print" $
        runEval (eval (Let "x" (Print "foo" $ CstInt 2)  (Print "bar" $ CstInt 3)))
        @?= (["foo: 2","bar: 3"],Right (ValInt 3)),
      --
      testCase "Print(error)" $
        runEval (eval (Let "x" (Print "foo" $ CstInt 2) (Var "bar")))
        @?= (["foo: 2"],Left "Unknown variable: bar"),
      --
      testCase "KvGet(covered)" $
        runEval (eval (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 0)))))
        @?= ([],Right (ValBool False)),
      --
      testCase "KvGet(valid key)" $
        runEval (eval (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0))))
        @?= ([],Right (ValBool True)),
      --
      testCase "KvGet(Invalid key)" $
        runEval (eval (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1))))
        @?= ([],Left "Invalid key: ValInt 1"),
      -- Identity: pure id <*> v = v
      testCase "Applicative(Idnetity)" $
        runEval (pure id <*> (pure (ValInt 3)))
        @?= ([],Right (ValInt 3)),
      -- Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
      testCase "Applicative(Composition)" $
        let 
            u = pure (\val -> case val of
              ValInt xx -> ValInt (xx + 1)
              _ -> error "Unsupported value type")
            v = pure (\val -> case val of
              ValInt xx -> ValInt (xx * 2)
              _ -> error "Unsupported value type")
            w = pure (ValInt 3)
            left = pure (.) <*> u <*> v <*> w
            right = u <*> (v <*> w)
        in runEval (isEql left right)
        @?= ([], Right (ValBool True)),
      -- Homomorphism: pure f <*> pure x = pure (f x)
      testCase "Applicative(Homomorphism)" $
        let 
            f = (\val -> case val of
              ValInt xx -> ValInt (xx + 1)
              _ -> error "Unsupported value type")
            x = (ValInt 3)
            left = pure f <*> pure x
            right = pure (f x)
        in runEval (isEql left right)
        @?= ([], Right (ValBool True)),
      -- Interchange: u <*> pure y = pure ($ y) <*> u
      testCase "Applicative(Interchange)" $
        let 
            f = pure (\val -> case val of
              ValInt xx -> ValInt (xx + 1)
              _ -> error "Unsupported value type")
            x = (ValInt 3)
            left = f <*> pure x
            right = pure ($ x) <*> f
        in runEval (isEql left right)
        @?= ([], Right (ValBool True))


    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
