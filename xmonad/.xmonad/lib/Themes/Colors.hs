module Themes.Colors
    ( Color
    , makeHexColor
    ) where

newtype Color = HexColor String

makeHexColor :: String -> Color
makeHexColor hexCode = HexColor $ '#' : hexCode

instance Show Color where
    show (HexColor color) = color
