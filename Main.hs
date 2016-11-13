{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Logging
import Control.Monad (msum)

import qualified Data.Text as T

import Happstack.Server (simpleHTTP, nullConf, ok, Conf, ServerPart)

import Text.Blaze.Html

import qualified Heptagon.Pages.Index as Index
import Heptagon.Logging 


config :: Conf
config = nullConf

main :: IO ()
main = withStdoutLogging $ 
    simpleHTTP nullConf $
    msum [nullDir >>  indexPage
         ,dir "login" loginPage
         ]

indexPage :: ServerPart String
indexPage = logServerPart (ok Index.pageHtml)

