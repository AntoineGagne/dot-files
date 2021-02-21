module Themes.Colors
  ( Color,
    makeHexColor,
  )
where

import Data.Char (isHexDigit)

newtype Color = HexColor String

makeHexColor :: String -> Color
makeHexColor hexcode
  | any (not . isHexDigit) hexcode = error "Colors.makeHexColor: Invalid hexadecimal color format."
  | length hexcode /= 6 = error "Colors.makeHexColor: Invalid hexadecimal color format length."
  | otherwise = HexColor $ '#' : hexcode

instance Show Color where
  show (HexColor color) = color
