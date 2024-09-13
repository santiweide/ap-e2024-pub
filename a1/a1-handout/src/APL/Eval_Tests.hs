module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
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
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
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
          -- TODO - add more
        -- 
        testCase "Apply(Argument is Lambda)" $
          eval [("y",ValInt 2)] (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) )
            @?= Left "Unknown variable: x",
        --    
        testCase "Apply(Argument is Int)" $
          eval envEmpty (Apply (Let "x" (CstInt 2)  (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3)) 
            @?= Right (ValInt 5) ,
        -- 
        testCase "Apply(Argument is Bool)" $
          eval envEmpty (Apply (Let "x" (CstBool True)  (Lambda "y" (Eql (Var "x") (Var "y")))) (CstBool True))
            @?= Right (ValBool True)  ,
        --  
        testCase "Apply(Function is not evaled to be a ValFun)" $
          eval envEmpty (Apply (CstBool True) (Let "x" (CstBool True)  (Lambda "y" (Eql (Var "x") (Var "y")))))
            @?= Left "the first expression must evaluate to a ValFun." ,
        --   
        testCase "Apply(order)" $
            eval [("y",ValInt 10)] (Apply (Lambda "y" (Add (CstInt 0) (Var "y"))) (Let "y" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) )
            @?=   Left "Non-integer operand" ,
        -- 
        testCase "Apply(Error)" $
          eval envEmpty (Apply (Let "x" (If (CstInt 1) (CstInt 2) (CstInt 3)) (CstInt 1))  (CstInt 3))
            @?= Left "Non-boolean conditional.",
        -- 
        testCase "TryCatch(noerr)" $
          eval envEmpty (TryCatch (CstInt 0) (CstInt 1))
            @?= Right (ValInt 0)  ,
        -- 
        testCase "TryCatch(err)" $
          eval envEmpty (TryCatch (Var "missing") (CstInt 1))
            @?= Right (ValInt 1) ,
        -- 
        testCase "TryCatch(double err)" $
          eval envEmpty (TryCatch (Var "missing") (Eql (CstInt 0) (CstBool True)))
            @?= Left "error occurs:Invalid operands to equality while executing Unknown variable: missing"
        -- 
    ]
