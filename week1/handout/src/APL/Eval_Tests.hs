module APL.Eval_Tests (tests) where

import APL.AST (
    Exp (..),
    Val(..),
        )
import APL.Eval (
    envEmpty,
    eval,
    printExp
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
        testCase "Eql (Invalid oprands)" $
          eval envEmpty (Eql (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 1) )
            @?= Left "[error] Invalid oprands to equality",
        --
        testCase "Eql (bool true)" $
          eval envEmpty (Eql (CstBool True) (CstBool True))
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
        testCase "Apply(Argument is Lambda; calculatable int)" $
          eval [("y",ValInt 2)] (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) )
            @?= Right (ValInt 4) ,
        --  
        testCase "Apply(Argument is Lambda; calculatable bool)" $
          eval [("y",ValBool True)] (Apply (Lambda "y" (Eql (Var "x") (Var "y"))) (Let "x" (CstBool True) (Lambda "y" (Eql (Var "x") (Var "y")))) )
            @?= Right (ValBool True) ,
        --    
        testCase "Apply(Argument is Lambda; ValFun in env)" $
          eval [("y",ValFun [] "y" (CstInt 3))] (Apply (Lambda "y" (Eql (Var "x") (Var "y"))) (Let "x" (CstBool True) (Lambda "y" (Eql (Var "x") (Var "y")))) )
            @?= Right (ValFun [("y",ValFun [] "y" (CstInt 3)),("x",ValBool True)] "y" (Eql (Var "x") (Var "y"))) ,
        --    
        testCase "Apply(Argument is Lambda, uncalculatable)" $
          eval [] (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) )
            @?= Right (ValFun [("x",ValInt 2)] "y" (Add (Var "x") (Var "y")))  ,
        -- 
        testCase "Apply(Argument is Int)" $
          eval [] (Apply (Let "x" (CstInt 2)  (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3)) 
            @?= Right (ValInt 5) ,
        -- 
        testCase "Apply(Argument is Bool)" $
          eval [] (Apply (Let "x" (CstBool True)  (Lambda "y" (Eql (Var "x") (Var "y")))) (CstBool True))
            @?= Right (ValBool True)  ,
        --  
        testCase "Apply(Function is not evaled to be a ValFun)" $
          eval [] (Apply (CstBool True) (Let "x" (CstBool True)  (Lambda "y" (Eql (Var "x") (Var "y")))))
            @?= Left "the first expression must evaluate to a ValFun." ,
        -- 
        testCase "Apply(Error)" $
          eval [] (Apply (Let "x" (Div (CstInt 7) (CstInt 0)) (Var "x")) (CstInt 3))
            @?= Left "[error] Division by zero",
        -- 
        testCase "TryCatch(noerr)" $
          eval [] (TryCatch (CstInt 0) (CstInt 1))
            @?= Right (ValInt 0)  ,
        -- 
        testCase "TryCatch(err)" $
          eval [] (TryCatch (Var "missing") (CstInt 1))
            @?= Right (ValInt 1) ,
        -- 
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
            @?= "try (True) catch (if ((True) == (True)) then (let x = (False) in ((y) == (True))) else (let x = (True) in ((y) == (True))))" ,
        --
        testCase "getVar" $
          eval envEmpty (Var "y")
          @?= Left "[error] Unkonw Variable"
        
    ]
