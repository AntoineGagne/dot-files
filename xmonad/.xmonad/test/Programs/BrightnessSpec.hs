module Programs.BrightnessSpec where

import Control.Exception.Base ( evaluate )
import Test.Hspec
import Test.QuickCheck

import Control.Lens
    ( makeLenses
    , (^.)
    , (.~)
    , (&)
    )

import Programs.Brightness
    ( decreaseBy
    , increaseBy
    , currentPercentage
    , dummyDevice
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "decreaseBy" $ parallel $ do
        it "decreases by a correct amount" $
            decreaseBy device' 10 `shouldBe` dummyDevice 1000 900 1000

        it "does not decrease less than 0" $
            decreaseBy (dummyDevice 0 0 1000) 10 `shouldBe` dummyDevice 0 0 1000

        it "is inverse to `increaseBy`" $ property $
            \a b c n -> increaseBy (decreaseBy (dummyDevice a b c) n) n `shouldBe` dummyDevice a b c

    describe "increaseBy" $ parallel $ do
        it "increases by a correct amount" $
            increaseBy (dummyDevice 1000 900 1000) 10 `shouldBe` dummyDevice 1000 1000 1000

        it "does not increase more than the maximum" $
            increaseBy device' 10 `shouldBe` device'

        it "is inverse to `decreaseBy`" $ property $
            \a b c n -> decreaseBy (increaseBy (dummyDevice a b c) n) n `shouldBe` dummyDevice a b c

    describe "currentPercentage" $ parallel $ do
        it "returns 100% when the maximum is equal to the current" $
            currentPercentage device' `shouldBe` 100

        it "returns 0% when the current is zero" $ do
            currentPercentage (dummyDevice 0 0 1000) `shouldBe` 0

device' = dummyDevice 1000 1000 1000
