module Heptagon.Pages.Index
( pageHtml
) where

import Prelude hiding (span)
import Text.Blaze.Html5


pageHtml :: Html
pageHtml = docTypeHtml $
    span $ toHtml "Please Log In"

