module Heptagon.User
(
) where

import Database.Groundhog
import Database.Groundhog.Core
import Data.Int

data User = User 
    { id :: Int64
    , userName :: String
    , name :: String
    }

instance PersistField User where
    persistName _ = "User"
    toPersistValue (User id name) = return (
        ([PersistInt64 id, PersistString userName, PersistString name]++)
    fromPersistvalue ((PersistInt64 id):(PersistString userName):(PersistString name):xs) 
        = return (User id name, xs)

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: User
    constructors:
      - name: User
        uniques:
          - name: id
            type: int64
          - name: userName
            type: string
          - name: name
            type: string
|]
    
