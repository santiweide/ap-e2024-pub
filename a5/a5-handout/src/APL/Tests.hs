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

-- low+1) (upper+1
varLenWithout :: Int -> Int -> VName -> Bool
varLenWithout  lower upper varname = let len = length varname in 
    or [(lower > len), (len > upper)] 

genVarWithLenRule :: Gen VName
genVarWithLenRule = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- suchThat (listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']) (varLenWithin 1 3)
    pure (alpha : alphaNums)

genVarWithoutLenRule ::  Gen VName
genVarWithoutLenRule = do
    alpha <-elements ['a' .. 'z']
    alphaNums <- suchThat (listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']) (varLenWithout 1 3)
    pure (alpha : alphaNums)

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vars = -- let  1/(14+20*k) = X = P(CstInt) = P(CstBool) = P(Lambda)
  frequency $
    [ (100, CstInt <$> arbitrary) -- 0% error -- P(genExp is CstInt)=100/sum, sum = 100*13 + 100 + 2000*(length) = 1/(14+20*k)
    , (100, CstBool <$> arbitrary) -- 0% error -- P(genExp is CstBool) = 1/(41+20*k)
    , (100, Add <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)^2 = 1-X^2
    , (100, Sub <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)^2 = 1-X^2
    , (100, Mul <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)^2
    , (100, Div <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)^2, P(domain) = P(second CstInt is 0|no type err) = P(second CstInt is 0)/P(no type err) = P(value is CstInt 0)/P(CstInt)
    , (100, Pow <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)^2, P(domain) = P(second CstInt < 0|no type err) = P(second CstInt < 0)/P(no type err) =  P(value < CstInt 0)/P(CstInt)
    , (100, Eql <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = 1-P(genExp is CstInt)*P(genExp is CstInt) - P(genExp is CstBool)*P(genExp is Bool)=1-2/(41+20*k)^2
    , (100, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars) -- P(type err) = P(fst gen is not CstBool) = 1 - P(CstBool)
    , (50, Var <$>  genVarWithLenRule ) -- P(occur) = 1/2/(14+20*k) = y*X
    , (50, Var <$>  genVarWithoutLenRule) -- P(occur) = 1/2/(14+20*k) = y*X
    , (2000 * (length vars), Var <$> elements vars) -- 0% error ,avoiding coming into the elements [], and en-larging the probability as the case increases. but still could not be a must
    , (100, Apply <$> genExp halfSize vars <*> genExp halfSize vars) -- P(type err) = P(fst is not a function) = 1-P(lambda)
    , (2000, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars) -- P(err) = P(genExp has err), like an Amplifier
    , (100, do
        newVar <- genVarWithLenRule
        Let newVar <$> genExp halfSize vars <*> genExp halfSize (newVar : vars)) -- no error, keep balance
    , (100, do
        newVar <- genVarWithoutLenRule
        Let newVar <$> genExp halfSize vars <*> genExp halfSize (newVar : vars)) -- no error, keep balance
    , (100, do
        newVar <- genVarWithLenRule
        Lambda newVar <$> genExp (size - 1) (newVar : vars)) -- P(Lambda) = X, keep balance
    , (100, do
        newVar <- genVarWithoutLenRule
        Lambda newVar <$> genExp (size - 1) (newVar : vars)) -- P(Lambda) = X, keep balance
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
