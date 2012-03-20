{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Main where


class C a b | a -> b where
    f :: a -> Int
    
instance C Int Int where
    f i = i

instance C Int Bool where
    f i = True
    
    