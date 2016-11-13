module Heptagon.Pages
( asContentType
) where

import Happstack.Server.Response
import Happstack.Server.Types (setHeaderBS, Response (..))

import qualified Data.ByteString.Char8 as C

asContentType :: Response -> String -> Response 
asContentType res contentType = setHeaderBS (C.pack "Content-Type") (C.pack contentType) res


