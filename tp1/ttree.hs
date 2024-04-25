module TTree where

import Data.List

data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v ) | Leaf k v | E deriving (Show)


--t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)


search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search (x:xs) (Leaf k v) = if (x == k) && (length (xs) == 0) then Just v else Nothing
search (x:xs) (Node key v i c d) | x == key && (length xs == 0) = v
                                | x == key = search xs c
                                | x < key = search (x:xs) i
                                | otherwise = search (x:xs) d
