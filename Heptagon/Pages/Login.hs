{-# LANGUAGE OverloadedStrings #-}

module Heptagon.Pages.Login
( loginPage
) where

import Happstack.Server

import Heptagon.User
import Heptagon.User.Authenticate

import Prelude hiding (span)

import Control.Monad (mplus)

loginPage :: ServerPart Response
loginPage = do
            getLogin `mplus` postLogin

getLogin :: ServerPart Response
getLogin = ok $ {-asContentType "text/html" $-} toResponse pageHtml

postLogin :: ServerPart Response
postLogin = return $ toResponse ("" :: String)

pageHtml :: String
pageHtml = "Please login"
