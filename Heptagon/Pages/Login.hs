module Heptagon.Pages.Login
( loginPage
) where

import Happstack.Server

import Heptagon.Pages (asContentType)

import Prelude hiding (span)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Pretty

loginPage :: ServerPart String
loginPage = do
            ok $ asContentType "text/html" $ toResponse pageHtml

pageHtml :: String
pageHtml = ""
