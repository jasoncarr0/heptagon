{-# LANGUAGE OverloadedStrings #-}

module Heptagon.Logging 
( logServerPart
) where

import Data.Text as T

import Control.Logging
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader

import Happstack.Server.Internal.Monads
import Happstack.Server.Internal.Types

logServerPart :: MonadIO m => ServerPartT m String -> ServerPartT m String
logServerPart s@(ServerPartT (ReaderT f)) = s >>= \a -> ServerPartT $ ReaderT (\req -> logReq req >> logWeb (f req)) >> log' (T.pack a) >> return a

logReq :: MonadIO m => Request -> m ()
logReq = log' . T.pack . show

logWeb :: MonadIO m => WebT m a -> WebT m ()
logWeb (WebT (ErrorT e)) = return () --logWebTInternal e

logWebTInternal :: (MonadIO m, Show e) => Either e a -> m ()
logWebTInternal (Left e) = log' $ T.pack ("Error: " ++ show e)
logWebTInternal (Right a) = log' "Unknown"
