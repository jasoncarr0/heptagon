module Heptagon.Templates.Parse
( applyTemplate
, parser
) where

import Text.Parsec 

applyTemplate :: String -> TMap -> Either ParseError String
applyTemplate str tmap = flip interpret tmap <$> (parse parser "text" str)


parser :: Stream s m Char => ParsecT s u m [TemplateTerm]
parser = many (varInject <|> templateTag <|> rawString)

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

