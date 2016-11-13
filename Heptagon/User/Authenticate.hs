{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Heptagon.User.Authenticate
( userDB
, authenticate
, makeUser
) where

import Crypto.PBKDF2

import Data.ByteString as S (ByteString, pack, unpack)
import Data.ByteString.Char8 as C8 (pack, unpack)
import Data.Int (Int64 (..))
import Data.Word (Word8 (..))
import Data.String (IsString (..))

import Database.PostgreSQL.Simple

import Happstack.Server
import Heptagon.DB (dbSettings)
import Heptagon.User


encryptPass :: String -> String
encryptPass pass = fromHashed $ pbkdf2 
    (Password $ strToWords pass) 
    (Salt $ strToWords "hackrpi")

fromHashed :: HashedPass -> String 
fromHashed (HashedPass words) = C8.unpack (S.pack words)

strToWords :: String -> [Word8]
strToWords s = S.unpack (C8.pack s)



data AuthError = AuthenticationFailed | NoSuchUser | DuplicateUser

userDB :: ConnectInfo
userDB = dbSettings 


authenticate :: String -> String -> IO (Maybe User)
authenticate username pass =do
    c <- connect userDB
    let pwhash = encryptPass pass
    users :: [(Int64, String, String, String)] <- 
        query c  "select * from users where user_name = ? and pwhash = ?" (username, pwhash)
    let id = case users of [(id, username, name, _)] -> Just (User id username name)
                           _                         -> Nothing
    close c
    return id


changePass :: String -> String -> IO Bool
changePass username newPass = do
    c <- connect userDB
    let pwhash = encryptPass newPass
    execute c "update users set pwhash = ? where user_name = ?" (pwhash, username)
    close c
    return True
    


makeUser :: String -> String -> String -> IO (Maybe User)
makeUser username email pass = do
    c <- connect userDB
    [Only id] <- query c 
        "insert into users (user_name, email, pwhash) values (?,?,?) returning id" 
        (username, email, encryptPass pass)
    close c
    return (Just (User id username email))


