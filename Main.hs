module Main where


import Control.Monad (msum)
import Happstack.Server (simpleHTTP, nullConf, ok, Conf, ServerPart)
import Text.Blaze.Html

import Heptagon.Logger
import qualified Heptagon.Pages.Index as Index



config :: Conf
config = nullConf

main :: IO ()
main = simpleHTTP nullConf $
    msum [index
         ]

index :: ServerPart Html
index = ok Index.pageHtml
