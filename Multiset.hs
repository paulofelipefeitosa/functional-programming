module Multiset(Multiset(..))
    where

class Multiset bag where
    insert :: (Ord elem) => elem -> bag elem -> bag elem
    remove :: (Ord elem) => elem -> bag elem -> bag elem
    search :: (Ord elem) => elem -> bag elem -> Int
    union :: (Ord elem) => bag elem -> bag elem -> bag elem
    intersection :: (Ord elem) => bag elem -> bag elem -> bag elem
    minus :: (Ord elem) => bag elem -> bag elem -> bag elem
    inclusion :: (Ord elem) => bag elem -> bag elem -> Bool
    sum :: (Ord elem) => bag elem -> bag elem -> bag elem
    size :: (Ord elem) => bag elem -> Int