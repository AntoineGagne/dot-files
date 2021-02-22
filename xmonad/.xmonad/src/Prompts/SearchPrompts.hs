{-# LANGUAGE NumericUnderscores #-}

module Prompts.SearchPrompts
  ( myPrompt,
  )
where

import Themes.Fonts (urxvtResourceFontString)
import Themes.Gruvbox (myTheme)
import Themes.Palettes (Palette (..))
import Themes.Themes
  ( Theme (..),
    showColor,
  )
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

myPrompt :: Prompt.XPConfig
myPrompt =
  Prompt.def
    { Prompt.alwaysHighlight = True,
      Prompt.bgColor = showColor background myTheme,
      Prompt.bgHLight = showColor background myTheme,
      Prompt.borderColor = showColor color7 myTheme,
      Prompt.fgColor = showColor foreground myTheme,
      Prompt.fgHLight = showColor color10 myTheme,
      Prompt.font = urxvtResourceFontString promptFont,
      Prompt.height = 30,
      Prompt.historyFilter = Prompt.deleteAllDuplicates . Prompt.deleteConsecutive,
      Prompt.historySize = 30_000,
      Prompt.maxComplRows = Just 50,
      Prompt.position = Prompt.CenteredAt {Prompt.xpCenterY = 0.5, Prompt.xpWidth = 1},
      Prompt.promptBorderWidth = 1,
      Prompt.promptKeymap = Prompt.vimLikeXPKeymap,
      Prompt.searchPredicate = fuzzyMatch,
      Prompt.sorter = fuzzySort
    }
  where
    promptFont = font myTheme
