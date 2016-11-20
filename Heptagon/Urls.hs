

module Heptagon.Urls
( urls
, pathOf
) where

import Data.Map

import Happstack.Server

type Path = String


urls :: Map String Path
urls = insert "" "" $
       empty

pathOf :: String -> Path
pathOf = (!) urls
