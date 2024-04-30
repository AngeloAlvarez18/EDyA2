{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Alumnos: 
-- Álvarez, Ángelo
-- Lojo, Dolores

module TTree where

import Data.List

data TTree k v = Node k (Maybe v) (TTree k v ) (TTree k v ) (TTree k v ) | Leaf k v | E deriving (Show)

--a)
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search (x:xs) (Leaf k v) = if (x == k) && (length (xs) == 0) then Just v else Nothing
search (x:xs) (Node key v i c d)  | x == key && (length xs == 0) = v
                                  | x == key = search xs c
                                  | x < key = search (x:xs) i
                                  | otherwise = search (x:xs) d

--b)
insertTTree :: Ord k => [k] -> v -> TTree k v -> TTree k v
insertTTree [] v t = t
insertTTree (x:xs) v E  | null xs = Leaf x v
                        | otherwise = (Node x Nothing E (insertTTree xs v E) E)
insertTTree (x:xs) v (Leaf k v1)  | x == k && null xs = Leaf x v -- un caracter y es el mismo de la hoja
                                  | x == k = Node k (Just v1) E (insertTTree xs v E) E -- caracter igual al de la hoja y no es un solo caracter
                                  | x < k = Node k (Just v1) (insertTTree (x:xs) v E) E E --recursión izq
                                  | otherwise = Node k (Just v1) E E (insertTTree (x:xs) v E) --recursión derecha
insertTTree (x:xs) v (Node k v1 i c d)  | x == k && null xs = Node x (Just v) i c d -- un caracter y es el mismo del nodo
                                        | x == k = Node k v1 i (insertTTree xs v c) d -- caracter igual al del nodo y no es un solo caracter
                                        | x < k = Node k v1 (insertTTree (x:xs) v i) c d -- recursión izquierda
                                        | otherwise = Node k v1 i c (insertTTree (x:xs) v d) -- recursión derecha



--c)
deleteTTree :: Ord k => [k] -> TTree k v -> TTree k v
deleteTTree [] t = t
deleteTTree (x:xs) E = E
deleteTTree (x:xs) (Leaf k v) | x == k = E
                              | otherwise = Leaf k v                           
deleteTTree (x:xs) (Node k v i c d) | x == k && null xs = Node k Nothing i c d
                                    | x == k = Node k v i (deleteTTree xs c) d
                                    | x < k = Node k v (deleteTTree (x:xs) i) c d
                                    | otherwise = Node k v i c (deleteTTree (x:xs) d)

--d)
keysTTree :: TTree k v -> [[k]]
keysTTree E = []
keysTTree (Leaf k v) = [[k]]
keysTTree (Node k Nothing i c d) = keysTTree i ++ map (k:) (keysTTree c) ++ keysTTree d
keysTTree (Node k _ i c d) = keysTTree i ++ [[k]] ++ map (k:) (keysTTree c) ++ keysTTree d

--e)
class Dic k v d | d -> k v where
  vacioDic :: d
  insertarDic :: Ord k => k -> v -> d -> d
  buscarDic :: Ord k => k -> d -> Maybe v
  eliminarDic :: Ord k => k -> d -> d
  clavesDic :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
    vacioDic = E
    insertarDic = insertTTree
    buscarDic = search
    eliminarDic = deleteTTree
    clavesDic = keysTTree