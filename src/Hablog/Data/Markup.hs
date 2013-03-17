{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hablog.Data.Markup
( MarkupEngine(..)
, convertToHtml
) where

import Database.Persist
import Database.Persist.TH
import Text.Pandoc
import qualified Data.Text as T

data MarkupEngine = Markdown deriving (Show, Read, Eq)
derivePersistField "MarkupEngine"

convertToHtml :: MarkupEngine -> T.Text -> T.Text
convertToHtml engine = case engine of
  Markdown -> T.pack . writeHtmlString def . readMarkdown def . T.unpack

