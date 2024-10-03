module APL.Tests
  ( properties
  , genExp -- for unit test, delete if no need TODO
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Control.Monad (liftM) -- for version of Gen [VName], delete if no need TODO
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , elements
  , listOf
  , frequency
  )


instance Arbitrary Exp where
  -- sized for the number of params, 
  -- rename the genExp with possible assigning of [VName]
  arbitrary = sized (\n -> genExp n [pure []]) 

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

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

-- TODO Gen [VName] or [Gen VName], which is better?
genVarMany :: Int -> [Gen VName]
genVarMany 0 = genVar:[pure []]
genVarMany size = let var = genVar in (var : (genVarMany (size - 1)))



-- TODO how to guarantee size of [VName] equals Int?
-- how to add things into [] using Int? 
-- maybe we should control how the vars are generated? or just take top size of the vars in the where
genExp :: Int -> [Gen VName] -> Gen Exp
-- no matter how many vars are there,
-- as long as size=0 there are only 2 choices
genExp 0 vars = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary] 
genExp size vars = -- TODO how to generate frequence using generate~~~ and elements~~~?
  frequency
    [ (4, CstInt <$> arbitrary)
    , (3, CstBool <$> arbitrary)
    , (5, Add <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
    , (4, Div <$> genExp halfSize vars <*> genExp halfSize vars)
    , (4, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
    , (3, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
    , (3, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
    , (1, Var <$> arbitrary) -- Makes non-trivial variable check 
    -- building let with existing vars
    , (2, Let <$> arbitrary <*> genExp halfSize vars  <*> genExp halfSize vars )
    -- bulding let with not existing vars
    , (2, Let <$> arbitrary <*> genExp halfSize vars  <*> genExp halfSize vars )
    , (2, Lambda <$> arbitrary <*> genExp (size - 1) vars )
    , (2, Apply <$> genExp halfSize vars  <*> genExp halfSize vars )
    , (2, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars ) 
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3
    vars = genVarMany size


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
