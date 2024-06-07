-- Practica 6

-- 1)

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

size :: BTree a -> Int
size Empty = 0
size (Node sz _ _ _) = sz


-- nth :: BTree a → Int → a, calcula el n-esimo elemento de una secuencia

nth :: BTree a -> Int -> a
nth (Node _ l x r) n | sl > n = nth l n
                     | sl == n = x
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
balance (Node 3 Empty a (Node 2 l b Empty)) = Node 3 l a (Node 1 Empty b Empty)
balance (Node 3 (Node 2 l b Empty) a Empty) = Node 3 (Node 1 Empty (value l) Empty) b (Node 1 Empty a Empty)
balance (Node 3 (Node 2 Empty b r) a Empty) = Node 3 (Node 1 Empty b Empty) (value r) (Node 1 Empty a Empty)


tabulateAux :: (Int -> a) -> Int -> Int -> BTree a
tabulateAux f n m | n == m = Node 1 Empty (f n) Empty
                  | m < n = Empty
                  | otherwise = let len = m - n + 1
                                    a = len `div` 2
                                    medio = m - a
                                in Node len (tabulateAux f n (medio-1)) (f medio) (tabulateAux f (medio+1) m)

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty
tabulate f n = let x = n `div` 2
                   l = tabulate f x
                   r = tabulateAux f (x+1) (n-1)
                in Node n l (f x) r

-- map :: (a → b) → BTree a → BTree b, la cual dada una funcion f y una secuencia s, devuelve el resultado de
-- aplicar f sobre cada elemento de s.

mapT :: (a -> b) -> BTree a -> BTree b
mapT f Empty = Empty
mapT f (Node n l a r) = let lr = mapT f l
                            rr = mapT f r
                        in (Node n lr (f a) rr)


-- take :: Int → BTree a → BTree a, tal que dados un entero n y una secuencia s devuelve los primeros n
-- elementos de s.

takeT :: Int -> BTree a -> BTree a
takeT n Empty = Empty
takeT n t@(Node m l a r) | n > m = t
                        | n == sl + 1 = (Node (m - sr) l a Empty)
                        | n <= sl = takeT n l
                        | otherwise = Node n l a (takeT (n - (sl + 1)) r)
                        where sl = size l 
                              sr = size r


-- drop :: Int → BTree a → BTree a, tal que dados un entero n y una secuencia s devuelve la secuencia s sin los
-- primeros n elementos.

dropT :: Int -> BTree a -> BTree a
dropT n Empty = Empty
dropT 0 t = t
dropT n (Node m l a r) | n == m = Empty
                       | n == sl = Node (m-n) Empty a r
                       | n == (sl+1) = r
                       | n < sl = Node (m-n) (dropT n l) a r
                       | otherwise = dropT (n -(sl+1)) r
                        where sl = size l

-- 2. El problema de calcular la maxima suma de una subsecuencia contigua de una secuencia dada s puede resolverse
-- con un algoritmo “Divide & Conquer” que en cada llamada recursiva calcule: la maxima suma de una subsecuencia
-- contigua de s, la maxima suma de un prefijo de s, la maxima suma de un sufijo de s y la suma de todos los elementos
-- de s. Dado el siguiente tipo de datos:

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

-- a) Definir una funcion mcss :: (Num a, Ord a) ⇒ Tree a → a, que calcule la maxima suma de una subsecuencia
-- contigua de una secuencia dada, en terminos de mapreduce.
-- Ayuda: Dado un arbol t, mcss aplica la funcion reduce sobre el arbol que se obtiene al reemplazar cada
-- elemento v por la 4-tupla (max (v, 0), max (v, 0), max (v, 0), v).

tuple :: (Num a, Ord a) => a -> (a, a, a, a)
tuple a = let a' = a `max` 0 
            in (a', a', a', a)

combineTuple :: (Num a, Ord a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
combineTuple (m, p, s, t) (m', p', s', t') = (max (s + p') (max m m'),
                                            max p (t+p'),
                                            max s' (s+t'),
                                            t + t')

mapReduce :: (Num a, Ord a) => (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapReduce f g e E = e
mapReduce f g e (Leaf a) = f a
mapReduce f g e (Join l r) = let l' = mapReduce f g e l
                                 r' = mapReduce f g e r
                            in g l' r'

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = let (a,b,c,d) = mapReduce tuple combineTuple (0,0,0,0) t
        in a
