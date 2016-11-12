module Heptagon.Pages.Template
(
) where

import Data.Text
import Data.List (foldl')

applyTemplate :: [(String, String)] -> Text -> Text
applyTemplate = foldl' (\t (old,  new) -> replace (fromString $ wrapVar old) (fromString new) t)

-- ${variable}
