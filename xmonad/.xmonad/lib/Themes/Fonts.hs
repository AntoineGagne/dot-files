{-# LANGUAGE RecordWildCards #-}

module Themes.Fonts
    ( FontType (..)
    , Font (..)
    , FontStyle (..)
    , urxvtResourceFontString
    ) where

data FontType = FreeType
              | OpenType

instance Show FontType where
    show _ = "xft"

data FontStyle = Regular
               | Italic
               | Bold
               | BoldItalic
               | Other String

instance Show FontStyle where
    show Regular = "Regular"
    show Italic = "Italic"
    show Bold = "Bold"
    show BoldItalic = "Bold Italic"
    show (Other other) = other

data Font = Font
    { format :: FontType
    , fontName :: String
    , antialias :: Bool
    , fontSize :: Integer
    , fontStyle :: FontStyle
    }

urxvtResourceFontString :: Font -> String
urxvtResourceFontString Font {..} = concat
    [ show format
    , ":"
    , fontName
    , ":style="
    , show fontStyle
    , ":size="
    , show fontSize
    , ":antialias="
    , if antialias then "true" else "false"
    ]
