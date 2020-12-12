module Programs.BrightnessSpec where

import Data.Maybe
    ( fromJust )

import Test.Hspec
import Test.QuickCheck

import Programs.Brightness
    ( decreaseBy
    , increaseBy
    , currentPercentage
    , dummyDevice
    , percent
    , Percent
    , BrightnessDevice
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "decreaseBy" $ parallel $ do
        it "decreases by a correct amount" $
            decreaseBy device' (percent' 10) `shouldBe` dummyDevice 1000 900 1000

        it "does not decrease less than 0" $
            decreaseBy (dummyDevice 0 0 1000) (percent' 10) `shouldBe` dummyDevice 0 0 1000

    describe "increaseBy" $ parallel $ do
        it "increases by a correct amount" $
            increaseBy (dummyDevice 1000 900 1000) (percent' 10) `shouldBe` dummyDevice 1000 1000 1000

        it "does not increase more than the maximum" $
            increaseBy device' (percent' 10) `shouldBe` device'

    describe "currentPercentage" $ parallel $ do
        it "returns 100% when the maximum is equal to the current" $
            currentPercentage device' `shouldBe` 100

        it "returns 0% when the current is zero" $ do
            currentPercentage (dummyDevice 0 0 1000) `shouldBe` 0

device' :: BrightnessDevice
device' = dummyDevice 1000 1000 1000

percent' :: Integer -> Percent
percent' = fromJust . percent
