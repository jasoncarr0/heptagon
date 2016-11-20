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
import Heptagon.Templates.Parse
import Heptagon.Templates.Types
import Heptagon.Urls (urls)

import Text.Parsec


render :: TMap -> String -> String
render tmap str = case applyTemplate str tmap of 
    Left er -> show er
    Right s -> s


urlsVal :: TVal
urlsVal = inject urls

splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst p [] = ([], [])
splitAtFirst p (x:xs) = if (p x) then ([], xs) else mapFst (x:) $ splitAtFirst p xs where
    mapFst f (a, b) = (f a, b)


applyTemplate :: String -> TMap -> Either ParseError String
applyTemplate str tmap = flip interpret tmap <$> (parse parser "text" str)

interpret :: [TemplateTerm] -> TMap -> String
interpret ((RawHTML str):strs) tmap = str ++ interpret strs tmap
interpret ((VarInj terms):strs) tmap = fromMaybe "null" (resolveVars terms tmap) 
    ++ interpret strs tmap
interpret [] tmap = []
interpret ((Tag str):strs) tmap = doTag str 

doTag :: [String] -> String
doTag = concat
