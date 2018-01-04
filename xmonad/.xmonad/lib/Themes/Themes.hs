module Themes.Themes
    ( Theme (..)
    , showColor
    ) where

import Themes.Colors ( Color )
import Themes.Fonts ( Font )
import Themes.Palettes ( Palette )

data Theme = Theme
    { name :: String
    , font :: Font
    , palette :: Palette
    }

showColor :: (Palette -> Color) -> Theme -> String
showColor color = show . color . palette
