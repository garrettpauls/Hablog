{-# LANGUAGE TypeFamilies #-}
module Hablog.Reform
( inputDateTime
) where

import Data.Text
import Data.Time
import Hablog.Util
import Text.Reform
import Text.Reform.Generalized as G
import Text.Blaze.Html (Html, (!), toValue)
import System.Locale
import qualified Text.Blaze.Html5  as H
import qualified Text.Blaze.Html5.Attributes as A

timeFormat :: String
timeFormat = "%FT%R"

inputDateTime :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input)
              => UTCTime
              -> Form m input error Html () UTCTime
inputDateTime = inputDateTime' getInputDate
  where
    getInputDate inp = case getInputText inp of
      Left  err  -> Left err
      Right text -> case parseTime defaultTimeLocale timeFormat (unpack text) of
        Nothing  -> Right $ mkUTCTime 9999 12 31 0 0 0
        Just dt  -> Right dt

inputDateTime' :: (Monad m, FormError error)
               => (input -> Either error UTCTime)
               -> UTCTime
               -> Form m input error Html () UTCTime
inputDateTime' getInput initialValue = G.input getInput inputField initialValue
  where
    inputField :: FormId -> UTCTime -> H.Markup
    inputField i a = H.input ! A.type_ (toValue "datetime-local") ! A.id (toValue $ show i) ! A.name (toValue $ show i) ! A.value (toValue $ formatDateTime a)
    formatDateTime :: UTCTime -> String
    formatDateTime = formatTime defaultTimeLocale timeFormat

