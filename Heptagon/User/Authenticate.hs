module Heptagon.User.Authenticate
(
) where

import Happstack.Server
import Database.Groundhog
import Database.Groundhog.Postgresql

encryptPass :: String -> Long
encryptPass = 0

--authenticate :: String -> String -> Postgresql -> IO (Maybe UserId)
authenticate user pass db = runDbConn $ do
    id <- get user    
