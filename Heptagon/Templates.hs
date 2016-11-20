{-# LANGUAGE FlexibleContexts #-}

module Heptagon.Templates 
( TVal (..)
, TMap (..)
, applyTemplate
, interpret
, urlsVal
) where

import Data.Map
import Data.Maybe (fromMaybe)
import Heptagon.Templates.Inject
import Heptagon.Urls (urls)


data TemplateTerm = RawHTML String | VarInj [String] | Tag String deriving Show

render :: TMap -> String -> String
render tmap str = case applyTemplate str tmap of Left er -> show er
                                                 Right s -> s


urlsVal :: TVal
urlsVal = inject urls

interpret :: [TemplateTerm] -> TMap -> String
interpret ((RawHTML str):strs) tmap = str ++ interpret strs tmap
interpret ((VarInj terms):strs) tmap = fromMaybe "null" (resolveVars terms tmap) 
    ++ interpret strs tmap
interpret [] tmap = []
interpret ((Tag str):strs) tmap = "{%TAG " ++ str ++ " TAG%}" ++ interpret strs tmap

