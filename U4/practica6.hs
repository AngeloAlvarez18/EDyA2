-- Practica 6

-- 1)

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

size :: BTree a -> Int
size Empty = 0
size (Node sz _ _ _) = sz


-- nth :: BTree a → Int → a, calcula el n-esimo elemento de una secuencia

nth :: BTree a -> Int -> a
nth (Node _ l x r) n | sl - 1 >= n = nth l n
                     | sl     == n = x
                     | otherwise = nth r (n - sl - 1)
                     where sl = size l

-- cons :: a → BTree a → BTree a, la cual inserta un elemento al comienzo de la secuencia

cons :: a -> BTree a -> BTree a
cons a t = Node n Empty a t where n = (size t) + 1

-- tabulate :: (Int → a) → Int → BTree a, la cual dada una funcion f y un entero n devuelve una secuencia de
-- tamano n, donde cada elemento de la secuencia es el resultado de aplicar f al ındice del elemento

-- Devuelve una tupla con los arboles con menos de n elementos y con mas de n elementos
splitAt2 :: BTree a -> Int -> (BTree a, BTree a)
splitAt2 Empty _ = (Empty, Empty)
splitAt2 (Node n l a r) m = if n < m then let (ll, lr) = splitAt2 l m
                                        in (ll, Node n lr a r)
                                    else let (rl, rr) = splitAt2 r m
                                        in (Node n l a rl, rr)

value :: BTree a -> a
value (Node _ _ a _) = a

balance :: BTree a -> BTree a
balance Empty = Empty 
balance t@(Node _ Empty a Empty) = t
balance (Node 3 Empty a (Node 2 Empty b r)) = Node 3 (Node 1 Empty a Empty) b r
balance (Node 3 (Node 2 l b Empty) a Empty) = Node 3 (Node 1 Empty (value l) Empty) b 

iddd a = a

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty
tabulate f n = let t = tabulate f (n-1)
                   sl = size t
                in Node (sl + 1) t (f sl) Empty

