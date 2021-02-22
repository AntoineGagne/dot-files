module Themes.Gruvbox
  ( gruvboxTheme,
    myTheme,
    myGridConfig,
    gruvboxGridConfig,
  )
where

import Themes.Colors (makeHexColor)
import Themes.Fonts
  ( Font (..),
    FontStyle (..),
    FontType (..),
    urxvtResourceFontString,
  )
import Themes.Palettes (Palette (..))
import Themes.Themes (Theme (..))
import XMonad (Window)
import XMonad.Actions.GridSelect
  ( GSConfig (..),
    buildDefaultGSConfig,
    colorRangeFromClassName,
  )
import XMonad.Core (X)

myTheme :: Theme
myTheme = gruvboxTheme

myGridConfig :: GSConfig Window
myGridConfig = gruvboxGridConfig

gruvboxGridConfig :: GSConfig Window
gruvboxGridConfig =
  (buildDefaultGSConfig gruvboxColorizer)
    { gs_cellheight = 200,
      gs_cellwidth = 300,
      gs_cellpadding = 8,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = urxvtResourceFontString gruvboxFont
    }

gruvboxColorizer :: Window -> Bool -> X (String, String)
gruvboxColorizer =
  colorRangeFromClassName
    -- Lowest inactive bg
    (40, 40, 40)
    -- Highest inactive bg
    (40, 40, 40)
    -- Active bg
    (184, 187, 38)
    -- Inactive fg
    (251, 241, 199)
    -- Active fg
    (40, 40, 40)

gruvboxTheme :: Theme
gruvboxTheme =
  Theme
    { name = "Gruvbox",
      palette = gruvboxPalette,
      font = gruvboxFont
    }

gruvboxPalette :: Palette
gruvboxPalette =
  Palette
    { background = makeHexColor "282828",
      foreground = makeHexColor "ebdbb2",
      color0 = makeHexColor "1d2021",
      color1 = makeHexColor "cc241d",
      color2 = makeHexColor "98971a",
      color3 = makeHexColor "d79921",
      color4 = makeHexColor "458588",
      color5 = makeHexColor "b16286",
      color6 = makeHexColor "689d6a",
      color7 = makeHexColor "a89984",
      color8 = makeHexColor "928374",
      color9 = makeHexColor "fb4934",
      color10 = makeHexColor "b8bb26",
      color11 = makeHexColor "fabd2f",
      color12 = makeHexColor "83a598",
      color13 = makeHexColor "d3869b",
      color14 = makeHexColor "8ec07c",
      color15 = makeHexColor "fbf1c7",
      color16 = makeHexColor "d65d0e",
      color17 = makeHexColor "fe8019"
    }

gruvboxFont :: Font
gruvboxFont =
  Font
    { format = OpenType,
      fontName = "Fira Code Medium",
      fontSize = 10,
      antialias = True,
      fontStyle = Other "Medium"
    }
