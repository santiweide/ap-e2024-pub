module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , frequency
  , elements
  , listOf
  )

instance Arbitrary Exp where
  arbitrary = sized (\n -> genExp n [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: Int -> [VName] -> Gen Exp
-- genExp 0 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp 0 vars = 
  frequency
    [ (4, CstInt <$> arbitrary)
    , (3, CstBool <$> arbitrary)
    , (2, if null vars then CstInt <$> arbitrary else Var <$> elements vars) -- Use existing vars
    ]

genExp size vars =
  frequency
    [ (4, CstInt <$> arbitrary)
    , (3, CstBool <$> arbitrary)
    , (2, if null vars then CstInt <$> arbitrary else Var <$> elements vars)
    , (5, Add <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
    , (4, Div <$> genExp halfSize vars <*> genExp halfSize vars)
    , (4, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
    , (3, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
    , (3, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
    , (2, Let <$> genVar <*> genExp halfSize vars <*> genExp halfSize newVars)
    , (2, Lambda <$> genVar <*> genExp (size - 1) newVars)
    , (2, Apply <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3
    newVars = genVar

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9' ]
    pure (alpha : alphaNums)

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted _ = undefined

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
