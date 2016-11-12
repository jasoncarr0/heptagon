module Heptagon.Pages.Index
( pageHtml
) where

import Data.Text.Lazy hiding (span)

import Prelude hiding (span)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Pretty


pageHtml :: String
pageHtml = renderHtml $
    docTypeHtml $
        span $
            toHtml "Please Log In"

