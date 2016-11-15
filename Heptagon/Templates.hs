{-# LANGUAGE FlexibleContexts #-}

module Heptagon.Templates 
( TVal (..)
, parseAll
, interpret
) where

import Data.Map
import Data.Maybe (fromMaybe)
import Text.Parsec
import Heptagon.Templates.Inject

data TemplateTerm = RawHTML String | VarInj [String] | Tag String deriving Show

interpret :: [TemplateTerm] -> TMap -> String
interpret ((RawHTML str):strs) tmap = str ++ interpret strs tmap
interpret ((VarInj terms):strs) tmap = fromMaybe "null" (resolveVars terms tmap) 
    ++ interpret strs tmap
interpret ((Tag str):strs) tmap = "{%TAG " ++ str ++ " TAG%}" ++ interpret strs tmap
interpret [] tmap = []

parseAll :: Stream s m Char => ParsecT s u m [TemplateTerm]
parseAll = many (varInject <|> templateTag <|> rawString)

rawString :: Stream s m Char => ParsecT s u m TemplateTerm
rawString = RawHTML <$> many1 ((notFollowedBy $ try startTag <|> try startVar) >> anyChar)

varInject :: Stream s m Char => ParsecT s u m TemplateTerm
varInject = VarInj <$> (try $ do
    startVar
    spaces
    vs <- vars
    spaces
    endVar
    return vs)

vars :: Stream s m Char => ParsecT s u m [String]
vars = sepBy1 (many1 alphaNum) (char '.')


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

