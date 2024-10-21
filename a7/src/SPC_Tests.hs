module SPC_Tests (tests) where

import Test.Tasty (TestTree, testGroup, localOption, mkTimeout)

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup "SPC (core)" $ []