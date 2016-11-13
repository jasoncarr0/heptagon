{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Heptagon.User.Authenticate
( userDB
, authenticate
, makeUser
) where

import Data.Int (Int64 (..))
import Database.PostgreSQL.Simple
import Happstack.Server
import Heptagon.User


encryptPass :: String -> Int64
encryptPass = const 0

data AuthError = AuthenticationFailed | NoSuchUser | DuplicateUser

userDB :: ConnectInfo
userDB = ConnectInfo "localhost" 5432 "" "" "users"


authenticate :: String -> String -> IO (Maybe User)
authenticate name pass =do
    c <- connect userDB
    let pwhash = encryptPass pass
    users :: [(Int64, String, String, Int64)] <- 
        query c  "select * where pwhash = ?" (Only pwhash)
    let id = case users of [(id, username, name, _)] -> Just (User id username name)
                           _                         -> Nothing
    close c
    return id

makeUser :: User -> IO (Maybe User)
makeUser u = do
    c <- connect userDB
    close c
    return (Just u)


