module Main where

import Test.Hspec
import Day01Spec (spec)

main :: IO ()
main = hspec $ do
  describe "Day01Spec" Day01Spec.spec