module Heptagon.User
( User (..)
) where

import Data.Int (Int64 (..))

data User = User 
    { id :: Int64
    , userName :: String
    , name :: String
    }

    
