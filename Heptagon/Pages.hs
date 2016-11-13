module Heptagon.Pages
(
) where

import Happstack.Server.Response
import Happstack.Server.Types (setHeaderBS)

import qualified Data.ByteString.Char8 as C

asContentType :: Response a -> String -> Response a
asContentType res contentType = setHeaderBS (C.pack "Content-Type") (C.pack contentType)


