module Heptagon.Pages
( withContentType
, baseVarsMap
, fromTemplate
) where

import Data.Map.Lazy

import Happstack.Server
import Happstack.Server.Response
import Happstack.Server.Types (setHeaderBS, Response (..))

import Heptagon.Templates

import qualified Data.ByteString.Char8 as C

withContentType :: Response -> String -> Response 
withContentType res contentType = setHeaderBS (C.pack "Content-Type") (C.pack contentType) res

baseVarsMap :: Map String TVal
baseVarsMap = insert "urls" urlsVal $
              empty

fromEither :: Show s => Either s String -> ServerPart Response
fromEither (Left s) = internalServerError (toResponse $ "Page error: " ++ show s)
fromEither (Right p) = ok $ toResponse p

fromTemplate :: String -> TMap -> ServerPart Response
fromTemplate s tm = fromEither $ applyTemplate s tm

