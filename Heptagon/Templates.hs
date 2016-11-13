module Heptagon.Templates 
( renderTemplate
, TValue (..)
, applyKeys
) where

import Data.Map
import Text.Parsec
import Heptagon.Templates.Inject




applyKeys :: ValMap -> String -> String
applyKeys vmap s = s

initial :: Char
initial = '{'

startVar :: String
startVar = "{{"

endVar :: String
endVar = "}}"

startTag :: String
startTag = "{%"

endTag :: String
endTag = "%}"

