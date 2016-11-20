module Heptagon.Pages.Index
( pageHtml
, indexPage
) where

import Data.Map.Lazy

import Happstack.Server 

import Heptagon.Pages
import Heptagon.Templates

import Prelude hiding (span)


pageHtml :: String
pageHtml = "Please log in below: "

indexPage ::  ServerPart Response
indexPage = fromTemplate pageHtml (Just . (!) baseVarsMap, keys baseVarsMap)
