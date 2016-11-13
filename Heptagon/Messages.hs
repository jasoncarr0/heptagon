module Heptagon.Messages
(
)

where

import Data.Int (Int64 (..))
import Heptagon.DB (dbSettings)
import Database.PostgreSQL.Simple

data Conversation = Conversation 
                  { id :: Int64
                  , [Message]}

type Message = String



messageDB :: ConnectInfo
messageDB = dbSettings 

loadConvo :: Int64 -> IO Conversation
loadConvo i = query c "select * where id = ?" (Only id) >>= 
                \(id:messages) -> User id messages
