{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
  , ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
module Hablog.Admin.Pages.Login
( login
) where

import Prelude            hiding (head)
import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Hablog.Data               (Page, getConfig)
import Hablog.Data.Config        (decodeBodyCfg)
import Hablog.Data.Sitemap       (Sitemap(..))
import Happstack.Server          (Input, Response, ok, toResponse)
import Text.Reform               ( CommonFormError(..), Form, FormError(..), (++>)
                                 , (<++), commonFormErrorStr, transformEither)
import Text.Reform.Happstack
import Text.Reform.Blaze.Text
import Text.Blaze.Html           (Html, toHtml)
import Web.Routes                (showURL)
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H

type LoginForm = Form Page [Input] LoginError Html ()

data LoginError = Required
                | InvalidCredentials
                | LoginCFE (CommonFormError [Input])
                deriving Show

instance FormError LoginError where
  type ErrorInputType LoginError = [Input]
  commonFormError = LoginCFE
instance H.ToMarkup LoginError where
  toMarkup (Required)           = toHtml $ "required"
  toMarkup (InvalidCredentials) = toHtml $ "invalid credentials"
  toMarkup (LoginCFE cfe)       = toHtml $ commonFormErrorStr show cfe

data Credentials = Credentials
  { username :: T.Text
  , password :: T.Text
  } deriving (Eq, Ord, Read, Show)

required :: T.Text -> Either LoginError T.Text
required str
  | T.length str == 0 = Left Required
  | otherwise         = Right str

loginForm :: LoginForm Credentials
loginForm = Credentials <$> user <*> pass <* inputSubmit (T.pack "Login")
  where
    user = errorList ++> label "Username" ++> (inputText (T.pack "") `transformEither` required) <++ br
    pass = errorList ++> label "Password" ++> (inputPassword         `transformEither` required) <++ br

appTemplate :: T.Text -> Html -> Page Response
appTemplate head body = ok $ toResponse $ H.html $ do
  H.head $ H.title (toHtml head)
  H.body body

login :: Page Response
login = do
  cfg <- getConfig
  decodeBodyCfg cfg
  loginUrl <- lift $ showURL AdminLogin >>= return . T.unpack
  loginPage loginUrl
  where
    loginPage :: String -> Page Response
    loginPage url = do
      result <- happstackEitherForm (form url) url loginForm
      case result of
        (Left html)  -> appTemplate (T.pack "Login") html
        (Right cred) -> appTemplate (T.pack "TODO" ) $ toHtml $ "TODO: Actually log in. " ++ show cred

