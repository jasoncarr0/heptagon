{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Heptagon.Templates.Inject
( TVal (..)
, TMap
, Inject (..)
, resolveVars
) where

import Data.Foldable as F (toList)

data TVal = TVal 
    { expand   :: String
    , subMap   :: TMap
    , iterable :: [TVal]
    } 

type TMap = (String -> Maybe TVal, [String])

resolveVars :: [String] -> TMap -> Maybe String
resolveVars (st:sts) tmap = expand <$> ((fst tmap st) >>= (resolveVars' sts)) where
    resolveVars' :: [String] -> TVal -> Maybe TVal
    resolveVars' [] v = Just v
    resolveVars' (st:sts) v = (fst (subMap v) st) >>= resolveVars' sts

instance Show TVal where
    show (TVal str (f, keys) iter) = 
        "TVal " ++ show str ++
        " {" ++
            foldr (\(k, o) s -> s ++ (show k) ++ ":" ++ (show o) ++ ", ") 
                "" 
                ((\k -> (k, f k)) <$> keys)
        ++ "} " ++ (show iter)

justShow :: Show a => a -> TVal
justShow a = TVal (show a) noMap []

noMap :: TMap
noMap = (const Nothing, [])--const Nothing

class Inject a where
    inject :: a -> TVal


instance Inject a => Inject (String -> a) where
    inject f = TVal "FUNCTION" (Just . inject . f, []) []

instance Inject String where
    inject str = TVal str noMap []

instance {-# OVERLAPPABLE #-} (Foldable f, Inject a, Show (f a)) => Inject (f a) where
    inject xs = TVal (show xs) noMap (inject <$> F.toList xs)

instance {-# OVERLAPPABLE #-} Show a => Inject a where
    inject = justShow

    
   
