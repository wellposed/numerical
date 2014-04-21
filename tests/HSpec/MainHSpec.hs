module Main where

import Test.Hspec
import  qualified LayoutHSpec

main :: IO ()
main = hspec $ do
  describe "Layout"     LayoutHSpec.spec 
