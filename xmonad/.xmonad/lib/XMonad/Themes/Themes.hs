module XMonad.Themes.Themes
    ( Theme (..)
    , showColor
    ) where

import XMonad.Themes.Colors ( Color )
import XMonad.Themes.Fonts ( Font )
import XMonad.Themes.Palettes ( Palette )

data Theme = Theme
    { name :: String
    , font :: Font
    , palette :: Palette
    }

showColor :: (Palette -> Color) -> Theme -> String
showColor color = show . color . palette
