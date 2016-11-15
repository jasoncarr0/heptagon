{-# LANGUAGE FlexibleContexts #-}

module Heptagon.Templates 
( TVal (..)
, parseAll
) where

import Data.Map
import Text.Parsec
import Heptagon.Templates.Inject

data TemplateTerm = RawHTML String | VarInj String | Tag String deriving Show

--splitHtml :: String -> [TemplateTerm]
--splitHtml = many (varInject <|> templateTag <|> rawString)

parseAll :: Stream s m Char => ParsecT s u m [TemplateTerm]
parseAll = many (varInject <|> templateTag <|> rawString)

rawString :: Stream s m Char => ParsecT s u m TemplateTerm
rawString = RawHTML <$> many1 ((notFollowedBy $ try startTag <|> try startVar) >> anyChar)

varInject :: Stream s m Char => ParsecT s u m TemplateTerm
varInject = VarInj <$> try (startVar >> spaces >> manyTill anyChar (try $ spaces >> endVar))

templateTag :: Stream s m Char => ParsecT s u m TemplateTerm
templateTag = Tag <$> (try $ startTag >> spaces >> manyTill anyChar (try $ spaces >> endTag))


startVar :: Stream s m Char => ParsecT s u m String
startVar = string "{{"

endVar :: Stream s m Char => ParsecT s u m String
endVar = string "}}"

startTag :: Stream s m Char => ParsecT s u m String
startTag = string "{%"

endTag :: Stream s m Char => ParsecT s u m String
endTag = string "%}"

