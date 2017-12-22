module XMonad.Prompts.SearchPrompts
    ( myPrompt
    ) where

import qualified XMonad.Prompt         as Prompt

import XMonad.Themes.Fonts ( urxvtResourceFontString )
import XMonad.Themes.Gruvbox ( gruvboxTheme )
import XMonad.Themes.Palettes ( Palette (..) )
import XMonad.Themes.Themes ( Theme (..) )

myPrompt = Prompt.def { Prompt.font = urxvtResourceFontString . font $ gruvboxTheme
                      , Prompt.bgColor = show . background . palette $ gruvboxTheme 
                      , Prompt.fgColor = show . foreground . palette $ gruvboxTheme
                      , Prompt.fgHLight = show . color10 . palette $ gruvboxTheme
                      , Prompt.bgHLight = show . background . palette $ gruvboxTheme
                      , Prompt.promptBorderWidth = 1
                      , Prompt.borderColor = show . color7 . palette $ gruvboxTheme
                      }

