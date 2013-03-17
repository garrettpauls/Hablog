{-# LANGUAGE TypeFamilies #-}
module Hablog.Admin.Pages.Entry
( entryNew
, entryList
) where

import Prelude            hiding (head)
import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class    (liftIO)
import Data.Maybe
import Data.Time
import Database.Persist
import Hablog.Data               (Page)
import Hablog.Data.Markup
import Hablog.Data.Persist
import Hablog.Data.Sitemap       (Sitemap(..))
import Hablog.Data.Slug
import Hablog.Data.RequestState
import Hablog.Reform
import Hablog.Util
import Happstack.Server          (Input, Response, ok, toResponse)
import Text.Reform               ( CommonFormError(..), Form, FormError(..), (++>)
                                 , (<++), commonFormErrorStr, transformEither)
import Text.Reform.Happstack
import Text.Reform.Blaze.Text
import Text.Blaze.Html           (Html, toHtml)
import Web.Routes                (showURL)
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H

type EntryForm = Form Page [Input] EntryError Html ()

data EntryError = Required
                | EntryCFE (CommonFormError [Input])
                deriving Show

instance FormError EntryError where
  type ErrorInputType EntryError = [Input]
  commonFormError = EntryCFE
instance H.ToMarkup EntryError where
  toMarkup (Required)           = toHtml $ "required"
  toMarkup (EntryCFE cfe)       = toHtml $ commonFormErrorStr show cfe

required :: T.Text -> Either EntryError T.Text
required str
  | T.length str == 0 = Left Required
  | otherwise         = Right str

entryNewForm :: (Key User) -> UTCTime -> EntryForm Entry
entryNewForm userId updateDate =
  mkEntry <$> fmap (slugify . T.unpack) slugHtml
          <*> titleHtml
          <*> textHtml
          <*> publishedHtml
          <*> pubDateHtml
          <*  inputSubmit (T.pack "Post")
  where
    slugHtml      = errorList ++> label "Slug"          ++> (inputText      (T.pack "") `transformEither` required) <++ br
    titleHtml     = errorList ++> label "Title"         ++> (inputText      (T.pack "") `transformEither` required) <++ br
    textHtml      = errorList ++> label "Content"       ++> (textarea 80 20 (T.pack "") `transformEither` required) <++ br
    publishedHtml = errorList ++> label "Is published?" ++> (inputCheckbox False)                                   <++ br
    pubDateHtml   = errorList ++> label "Publish date"  ++> (inputDateTime (mkUTCTime 1 1 1 0 0 0))                 <++ br
    mkEntry :: Slug -> T.Text -> T.Text -> Bool -> UTCTime -> Entry
    mkEntry slug title text isPub pubDate =
      Entry slug title userId Nothing text Markdown (convertToHtml Markdown text) isPub pubDate updateDate

appTemplate :: T.Text -> Html -> Page Response
appTemplate head body = ok $ toResponse $ H.html $ do
  H.head $ H.title (toHtml head)
  H.body body

entryNew :: Page Response
entryNew = do
  entryUrl <- lift $ showURL AdminEntryNew >>= return . T.unpack
  entryNewPage entryUrl

entryNewPage :: String -> Page Response
entryNewPage url = do
  curTime <- liftIO $ getCurrentTime
  userId  <- getSession >>= return . sessionUserId . entityVal . fromJust
  result  <- happstackEitherForm (form url) url $ entryNewForm userId curTime
  case result of
    (Left html) -> appTemplate (T.pack "New Entry") html
    (Right entry) -> appTemplate (T.pack "Entry") $ toHtml $ show entry

entryList :: Page Response
entryList = appTemplate (T.pack "Entry List") $ H.p (toHtml "TODO: Entry list")

