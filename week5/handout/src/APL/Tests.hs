module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen)

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9' ]
    pure (alpha : alphaNums)

genExp :: Gen Exp
genExp = undefined
