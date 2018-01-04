module XMonad.Prompts.SearchPrompts
    ( myPrompt
    ) where

import qualified XMonad.Prompt         as Prompt

import XMonad.Themes.Fonts ( urxvtResourceFontString )
import XMonad.Themes.Gruvbox ( myTheme )
import XMonad.Themes.Palettes ( Palette (..) )
import XMonad.Themes.Themes ( Theme (..)
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
