module Heptagon.Templates 
( renderTemplate
, TValue (..)
, applyKeys
) where

import Data.Map
import Text.Parsec
import Heptagon.Templates.Inject

data TemplateTerm = RawHTML String | VarInj String | Tag String


varInject :: Stream s m t => ParsecT s u m TemplateTerm
varInject = VarInj <$> (startVar >> manyTil (notFollowedBy startVar anyChar) endVar)


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

