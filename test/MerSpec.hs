module MerSpec (spec) where

import Mer

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Mer"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
