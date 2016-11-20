{-# LANGUAGE FlexibleContexts #-}

module Heptagon.Templates 
( TVal (..)
, TMap (..)
, Inject (..)
, applyTemplate
, interpret
, fromMap
, putTMap
) where

import Data.Map
import Data.Maybe (fromMaybe)
import Heptagon.Templates.Parse
import Heptagon.Templates.Types

import Text.Parsec


render :: TMap -> String -> String
render tmap str = case applyTemplate tmap str of 
    Left er -> show er
    Right s -> s


splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst p [] = ([], [])
splitAtFirst p (x:xs) = if (p x) then ([], xs) else mapFst (x:) $ splitAtFirst p xs where
    mapFst f (a, b) = (f a, b)


applyTemplate :: TMap -> String -> Either ParseError String
applyTemplate tmap str = interpret tmap <$> (parse parser "text" str)

interpret :: TMap -> [TemplateTerm] -> String
interpret tmap ((RawHTML str):terms) = str ++ interpret tmap terms
interpret tmap ((VarInj var):terms) = fromMaybe "null" (expand <$> resolveVars var tmap) 
    ++ interpret tmap terms
interpret tmap ((Tag tag):terms) = doTag tmap tag terms
interpret tmap [] = []

splitBy :: Eq a => [a] -> a -> [[a]]
splitBy (x:xs) y = if x == y then splitBy xs y else concatHead x $ splitBy xs y where
    concatHead :: a -> [[a]] -> [[a]]
    concatHead a (b:bs) = (a:b):bs
    concatHead a [] = [[a]]
splitBy [] y = []

doTag :: TMap -> [String] -> [TemplateTerm] -> String
doTag tmap ("for":varName:"in":sourceName:[]) rest = forPart ++ (interpret tmap restPart) where
    forPart = case resolvedName of 
        Nothing -> ""
        Just var -> iterable var >>= withVar rawForPart tmap varName
    withVar :: [TemplateTerm] -> TMap -> String -> TVal -> String
    withVar terms tmap name val = interpret (putTMap name val tmap) terms
    resolvedName = resolveVars (sourceName `splitBy` '.') tmap
    (rawForPart, restPart) = splitAtFirst isEndFor rest
    isEndFor (Tag ["endfor"]) = True
    isEndFor _ = False
    

fromMap :: Map String TVal -> TMap
fromMap m = (Just . (!) m, keys m)

putTMap :: String -> TVal -> TMap -> TMap
putTMap name var (f, ls) = ((\s -> if (s == name) then Just var else f s), name : ls)
