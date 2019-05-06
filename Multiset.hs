module Multiset(Multiset(..))
    where

class Multiset a where
    insert :: (Eq elem, Ord elem) => elem -> a -> a
    remove :: (Eq elem, Ord elem) => elem -> a -> a
    search :: (Eq elem, Ord elem) => elem -> a -> Int
    union :: a -> a -> a
    intersection :: a -> a -> a
    minus :: a -> a -> a
    inclusion :: a -> a -> Bool
    sum :: a -> a -> a
    size :: a -> Int