module Main where

import Test.Hspec
import Day01Spec (spec)
import Day02Spec (spec)
import Day03Spec (spec)

main :: IO ()
main = hspec $ do
  describe "Day01Spec" Day01Spec.spec
  describe "Day02Spec" Day02Spec.spec
  describe "Day03Spec" Day03Spec.spec