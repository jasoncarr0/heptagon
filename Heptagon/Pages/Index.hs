module Heptagon.Pages.Index
( pageHtml
, indexPage
) where

import Happstack.Server 

import Prelude hiding (span)


pageHtml :: String
pageHtml = "Please log in below: "

indexPage ::  ServerPart Response
indexPage = ok $ toResponse pageHtml
