module Themes.FontsSpec where

import Control.Exception.Base (evaluate)
import Test.Hspec
import Themes.Colors (makeHexColor)
import Themes.Fonts
  ( Font (..),
    FontStyle (..),
    FontType (..),
    urxvtResourceFontString,
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "urxvtResourceFontString" $
    parallel $
      it "returns the font in a format valid to Urxvt" $
        urxvtResourceFontString aFont `shouldBe` expectedFormat
  where
    aFont =
      Font
        { format = OpenType,
          fontName = "Fira Code Medium",
          fontSize = 10,
          antialias = True,
          fontStyle = Other "Medium"
        }
    expectedFormat = "xft:Fira Code Medium:style=Medium:size=10:antialias=true"
