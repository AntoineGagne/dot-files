module Prompts.SearchPrompts
    ( myPrompt
    ) where

import qualified XMonad.Prompt as Prompt

import Themes.Fonts ( urxvtResourceFontString )
import Themes.Gruvbox ( myTheme )
import Themes.Palettes ( Palette (..) )
import Themes.Themes ( Theme (..)
                     , showColor
                     )

myPrompt = Prompt.def { Prompt.font = urxvtResourceFontString . font $ myTheme
                      , Prompt.bgColor = showColor background myTheme 
                      , Prompt.fgColor = showColor foreground myTheme
                      , Prompt.fgHLight = showColor color10 myTheme
                      , Prompt.bgHLight = showColor background myTheme
                      , Prompt.promptBorderWidth = 1
                      , Prompt.borderColor = showColor color7 myTheme
                      }
