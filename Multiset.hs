module Multiset(Multiset(..),
    MultisetException(..))
    where

import Control.Exception

data MultisetException = ElementoInexistente [Char] | OperacaoNaoPermitida [Char] deriving (Show)

instance Exception MultisetException

class Multiset bag where
    insert :: (Ord elem, Show elem) => elem -> bag elem -> bag elem
    remove :: (Ord elem, Show elem) => elem -> bag elem -> IO (bag elem)
    search :: (Ord elem, Show elem) => elem -> bag elem -> Int
    union :: (Ord elem, Show elem) => bag elem -> bag elem -> bag elem
    intersection :: (Ord elem, Show elem) => bag elem -> bag elem -> bag elem
    minus :: (Ord elem, Show elem) => bag elem -> bag elem -> IO (bag elem)
    inclusion :: (Ord elem, Show elem) => bag elem -> bag elem -> Bool
    sum :: (Ord elem, Show elem) => bag elem -> bag elem -> bag elem
    size :: (Ord elem, Show elem) => bag elem -> Int