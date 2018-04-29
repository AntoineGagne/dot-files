{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GHC.Generics (Generic)
import Prelude hiding (FilePath)

import Control.Lens ((^.))
import Data.Generics.Product (field)
import Turtle

data Arguments
    = Arguments
        { source :: FilePath
        , destination :: FilePath
        } deriving (Show, Generic)

parser :: Parser Arguments
parser = Arguments <$> argPath "SOURCE" "the source file"
                   <*> argPath "DESTINATION" "the destination file"

main :: IO ()
main = do
    parsedArguments <- options "Convert PDF colorspace to CMYK" parser
    void $ proc "gs" [ "-o" <> format fp (parsedArguments^.field @"destination")
                     , "-sDEVICE=pdfwrite"
                     , "-sProcessColorModel=DeviceCMYK"
                     , "-sColorConversionStrategy=CMYK"
                     , "-sColorConversionStrategyForImages=CMYK"
                     , format fp $ parsedArguments^.field @"source"
                     ] (pure "")
