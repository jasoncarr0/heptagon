module Heptagon.Pages.Login
( loginPage
) where

import Happstack.Server

import Heptagon.Pages (asContentType)
import Heptagon.User
import Heptagon.User.Authenticate

import Prelude hiding (span)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Pretty

loginPage :: ServerPart String
loginPage = do
            getLogin <|> postLogin

getLogin :: ServerPart String
    ok $ asContentType "text/html" $ toResponse pageHtml

postLogin :: ServerPart String
    return ""

pageHtml :: String
pageHtml = "Please login"
