module Heptagon.User
( User (..)
) where

import Data.Int (Int64 (..))

data User = User 
    { userId :: Int64
    , userName :: String
    , userRealName :: String
    } deriving (Eq, Ord, Show)

    
