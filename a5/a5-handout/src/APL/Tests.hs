module APL.Tests
  ( properties
    , expCoverage
  )
where

import APL.Eval (askEnv)
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
  , suchThat
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


genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genVarWithOut :: [VName] -> Gen VName
genVarWithOut env = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- elements env
    pure (alpha : alphaNums)

varLenWithin :: Int -> Int -> VName -> Bool
varLenWithin  lower upper varname = let len = length varname in 
    and [(lower <= len), (len <= upper)] 

varLenWithout :: Int -> Int -> VName -> Bool
varLenWithout  lower upper varname = let len = length varname in 
    or [(lower > len), (len > upper)] 

genVarWithLenRule :: Gen VName
genVarWithLenRule = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- suchThat (listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']) (varLenWithin 1 3)
    pure (alpha : alphaNums)

genVarWithoutLenRule :: Gen VName
genVarWithoutLenRule = do
    alpha <-elements ['a' .. 'z']
    alphaNums <- suchThat (listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']) (varLenWithout 1 3)
    pure (alpha : alphaNums)

-- How to control the percent...
genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vars = -- 15 all 14*3
  frequency $
    [ (1, CstInt <$> arbitrary)
    , (1, CstBool <$> arbitrary)
    , (1, Add <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Div <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
    , (30, Var <$> suchThat genVarWithLenRule (`notElem` vars))
    , (30, Var <$> suchThat genVarWithoutLenRule  (`notElem` vars)) -- 80% percent get out of scope
    , (0 * (length vars), Var <$> elements vars) -- in scope
    , (1, Apply <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars) 
    , (1, do
        newVar <- genVar
        Let newVar <$> genExp halfSize vars <*> genExp halfSize (newVar : vars))
    , (1, do
        newVar <- genVar
        Lambda newVar <$> genExp (size - 1) (newVar : vars))
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

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
