module TTree where

import Data.List

data TTree k v = Node k (Maybe v) (TTree k v ) (TTree k v ) (TTree k v ) | Leaf k v | E deriving (Show)


--Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)


search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search (x:xs) (Leaf k v) = if (x == k) && (length (xs) == 0) then Just v else Nothing
search (x:xs) (Node key v i c d) | x == key && (length xs == 0) = v
                                | x == key = search xs c
                                | x < key = search (x:xs) i
                                | otherwise = search (x:xs) d

getKey :: TTree k v -> Maybe k
getKey E = Nothing
getKey (Leaf k v) = Just k
getKey (Node k v _ _ _) = Just k

insertTTree :: Ord k => [k] -> v -> TTree k v -> TTree k v
insertTTree [] v t = t
insertTTree (x:xs) v E | length xs == 0 = Leaf x v
                        | otherwise = (Node x Nothing E (insertTTree xs v E) E)
insertTTree (x:xs) v (Leaf k v1) | x == k && length xs == 0 = Leaf x v -- un caracter y es el mismo de la hoja
                                | x == k = Node k (Just v1) E (insertTTree xs v E) E -- caracter igual al de la hoja y no es un solo caracter
                                | x < k = Node k (Just v1) (insertTTree (x:xs) v E) E E 
                                | otherwise = Node k (Just v1) E E (insertTTree (x:xs) v E)

insertTTree (x:xs) v (Node k v1 i c d) | x == k && length xs == 0 = Node x (Just v) i c d
                                        | x == k = Node k v1 i (insertTTree xs v c) d
                                        | x < k = Node k v1 (insertTTree (x:xs) v i) c d
                                        | otherwise = Node k v1 i c (insertTTree (x:xs) v d)

--Sin Rsi
--                     R (1)
--                   / |
--                  E  E 
--                     |
--                     D 
--                     |
--                     E (2)




--Node 'h' (Just 1) E (Node 'o' Nothing E (Node 'l' Nothing E (Leaf 'a' 123) (Node 'n' Nothing E (Node 'd' Nothing E (Leaf 'a' 321) E) E)) E) E

e = Node 'h' (Just 1) (Node 'd' Nothing E (Node 'o' Nothing E (Node 'l' Nothing E (Leaf 'o' 4) E) E) E) (Node 'o' Nothing E (Node 'l' (Just 2) E (Leaf 'a' 5) E) E) (Node 'o' Nothing E (Node 'l' Nothing E (Leaf 'a' 3) E) E)

--Node 'h' (Just 1) (Node 'd' Nothing E (Node 'o' Nothing E (Node 'l' Nothing E (Leaf 'o' 4) E) E) E) (Node 'o' Nothing E (Node 'l' (Just 2) E E E) E) (Node 'o' Nothing E (Node 'l' Nothing E (Leaf 'a' 3) E) E)

deleteTTree :: Ord k => [k] -> TTree k v -> TTree k v
deleteTTree [] t = t
deleteTTree (x:xs) E = E
deleteTTree (x:xs) (Leaf k v) | x == k = E
                              | otherwise = Leaf k v
deleteTTree (x:xs) (Node k v i c d) | x == k && length xs == 0 = Node k Nothing i c d
                                    | x == k = Node k v i (deleteTTree xs c) d
                                    | x < k = Node k v (deleteTTree (x:xs) i) c d
                                    | otherwise = Node k v i c (deleteTTree (x:xs) d)


keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
--keys (Node k v i E E) = 