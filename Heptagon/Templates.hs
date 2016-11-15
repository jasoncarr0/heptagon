{-# LANGUAGE FlexibleContexts #-}

module Heptagon.Templates 
( TVal (..)
) where

import Data.Map
import Text.Parsec
import Heptagon.Templates.Inject

data TemplateTerm = RawHTML String | VarInj String | Tag String

--splitHtml :: String -> [TemplateTerm]
--splitHtml = many (varInject <|> templateTag <|> rawString)

rawString :: Stream s m Char => ParsecT s u m TemplateTerm
rawString = RawHTML <$> manyTill anyChar (startVar <|> startTag)

varInject :: Stream s m Char => ParsecT s u m TemplateTerm
varInject = VarInj <$> (startVar >> manyTill (notFollowedBy startVar) anyChar >> endVar)

templateTag :: Stream s m Char => ParsecT s u m TemplateTerm
templateTag = Tag <$> (startTag >> manyTill (notFollowedBy startTag) anyChar >> endTag)


startVar :: Stream s m Char => ParsecT s u m String
startVar = string "{{"

endVar :: Stream s m Char => ParsecT s u m String
endVar = string "}}"

startTag :: Stream s m Char => ParsecT s u m String
startTag = string "{%"

endTag :: Stream s m Char => ParsecT s u m String
endTag = string "%}"

