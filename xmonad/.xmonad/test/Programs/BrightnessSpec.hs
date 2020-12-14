module Programs.BrightnessSpec where

import Data.Maybe
  ( fromJust,
    isJust,
  )
import Programs.Brightness
  ( BrightnessDevice,
    Percent,
    currentPercentage,
    decreaseBy,
    dummyDevice,
    increaseBy,
    percent,
  )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "decreaseBy" $ do
    it "decreases by a correct amount" $
      decreaseBy device' (percent' 10) `shouldBe` dummyDevice 1000 900 1000

    it "does not decrease less than 0" $
      decreaseBy (dummyDevice 0 0 1000) (percent' 10) `shouldBe` dummyDevice 0 0 1000

  describe "increaseBy" $ do
    it "increases by a correct amount" $
      increaseBy (dummyDevice 1000 900 1000) (percent' 10) `shouldBe` dummyDevice 1000 1000 1000

    it "does not increase more than the maximum" $
      increaseBy device' (percent' 10) `shouldBe` device'

  describe "currentPercentage" $ do
    it "returns 100% when the maximum is equal to the current" $
      currentPercentage device' `shouldBe` 100

    it "returns 0% when the current is zero" $ do
      currentPercentage (dummyDevice 0 0 1000) `shouldBe` 0

  describe "percent" $ do
    it "returns `Nothing` if the value is less than 0" $ do
      percent (-2) `shouldBe` Nothing

    it "returns `Nothing` if the value is more than 100" $ do
      percent 104 `shouldBe` Nothing

    it "returns a `Percent` if the value is between 0 and 100" $ do
      percent 42 `shouldSatisfy` isJust

device' :: BrightnessDevice
device' = dummyDevice 1000 1000 1000

percent' :: Integer -> Percent
percent' = fromJust . percent
