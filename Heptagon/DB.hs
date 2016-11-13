module Heptagon.DB
( dbSettings
) where

import Database.PostgreSQL.Simple

dbSettings  :: ConnectInfo
dbSettings = ConnectInfo "digitalocean" 5432 "hackrpi" "" "heptagon"
