module Themes.ColorsSpec where

import Control.Exception.Base (evaluate)
import Test.Hspec
import Themes.Colors (makeHexColor)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "makeHexColor" $
  parallel $ do
    it "returns a color in its hexadecimal format given a valid format" $
      show (makeHexColor "282828") `shouldBe` "#282828"
    it "throws an error if there is an invalid character" $
      evaluate (makeHexColor "28%282") `shouldThrow` anyErrorCall
    it "throws an error if there is too many/not enough characters" $
      evaluate (makeHexColor "28228228") `shouldThrow` anyErrorCall
