module Hablog.Data.Entry
( updateHtml
) where

import Hablog.Data.Persist
import Hablog.Data.Markup
import Text.Pandoc

updateHtml :: Entry -> Entry
updateHtml entry =
  let html = convertToHtml (entryMarkupEngine entry) (entryMarkup entry)
   in entry { entryHtml = html }

