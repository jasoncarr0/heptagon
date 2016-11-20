{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Logging
import Control.Monad (msum)

import qualified Data.Text as T

import Happstack.Server 
import Happstack.Server.Routing (dir, nullDir)

import qualified Heptagon.Pages.Index as Index
import qualified Heptagon.Pages.Login as Login
import Heptagon.User 
import Heptagon.User.Authenticate 
import Heptagon.Logging 
import Heptagon.Templates

config :: Conf
config = nullConf

main :: IO ()
main = do
    user <- authenticate "jon" "password"
    --user <- makeUser (User 0 "jon" "Jon") "password"
    print user
    withStdoutLogging $ 
        simpleHTTP nullConf $
        msum [ nullDir >>  Index.indexPage
             , dir "login" Login.loginPage
             ]



indexPage :: ServerPart String
indexPage = logServerPart (ok Index.pageHtml)


