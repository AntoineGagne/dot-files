module XMonad.Themes.Themes
    ( Theme (..)
    ) where

import XMonad.Themes.Fonts ( Font )
import XMonad.Themes.Palettes ( Palette
                              )

data Theme = Theme
    { name :: String
    , font :: Font
    , palette :: Palette
    }
