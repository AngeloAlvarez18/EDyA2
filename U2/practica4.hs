-- parcial 1 2023

-- type Interval = (Int, Int)

-- data ITree = E | N ITree Interval ITree deriving Show

-- right :: ITree -> Int
-- right (N _ (x, y) E) = y
-- right (N _ (x, y) r)  = right r

-- darValorX :: ITree -> Int
-- darValorX (N _ (x, y) _) = x

-- darValorY :: ITree -> Int
-- darValorY (N _ (x, y) _) = y

-- checkIT :: ITree -> Bool
-- checkIT E = True
-- checkIT (N E (x, y) E) = x <= y
-- checkIT (N l (x, y) r) = (x <= y) && (darValorY l < (x -1)) && (y + 1 < darValorX r) && (checkIT l) && (checkIT r)


-- splitMax :: ITree -> (Interval, ITree)
-- splitMax (N l (x,y) E) = ((x,y), l)
-- splitMax (N l (x,y) r) = 

-- ------------------------------------------------------------------

-- Practica 4

-- 1. Si un arbol binario es dado como un nodo con dos subarboles identicos se puede aplicar la tecnica sharing para
-- que los subarboles sean representados por el mismo arbol. Definir las siguientes funciones de manera que se puedan
-- compartir la mayor cantidad posible de elementos de los arboles creados

--a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d , crea un arbol binario completo
-- de altura d con el valor x en cada nodo.

data Tree a = E | N (Tree a) a (Tree a) deriving Show

completo :: a -> Int -> Tree a
completo x 0 = E
completo x 1 = N E x E
completo x d = N (completo x (d-1)) x (completo x (d-1))

-- 2. Definir las siguientes funciones sobre arboles binarios de busqueda (bst):

data BST a = Empty | B (BST a) a (BST a) deriving Show

-- Hagamos primero una funcion para insertar
insertBST :: Ord a => a -> BST a -> BST a
insertBST x Empty = B Empty x Empty
insertBST x (B l a r) = if x > a then (B l a (insertBST x r)) else (B (insertBST x l) a r)

a = B (B (B Empty 1 Empty) 4 Empty) 9 (B (B Empty 12 Empty) 15 (B Empty 19 Empty))
arbolEjemplo = B (B (B Empty 6 Empty) 9 (B (B Empty 13 Empty) 14 Empty)) 15 (B (B Empty 17 Empty) 20 (B (B Empty 26 Empty) 64 (B Empty 72 Empty)))

-- 1. maximumT :: Ord a ⇒ BST a → a, que calcula el maximo valor en un bst.
maximumT :: Ord a => BST a -> a
maximumT Empty = error "no se aceptan arboles vacios"
maximumT (B Empty x Empty) = x
maximumT (B l x Empty) = x
maximumT (B l x r) = maximumT r

-- 2. checkBST :: Ord a ⇒ BST a → Bool, que chequea si un arbol binario es un bst

checkBST :: Ord a => BST a -> Bool
checkBST Empty = True
checkBST (B Empty x Empty) = True
checkBST (B t1@(B l1 a r1) x Empty) = x > a && checkBST t1
checkBST (B Empty x t2@(B l2 b r2)) = x < b && checkBST t2
checkBST (B t1@(B l1 a r1) x t2@(B l2 b r2)) = x > a && x < b && checkBST t1 && checkBST t2

-- 3. splitBST :: Ord a ⇒ BST a → a → (BST a, BST a), que dado un arbol bst t y un elemento x , devuelva una
-- tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x .

arbolMayores :: Ord a => BST a -> a -> BST a
arbolMayores Empty a = Empty
arbolMayores (B _ x Empty) a = Empty
arbolMayores (B _ x t2@(B l2 b r2)) a = if b > a then t2 else arbolMayores t2 a

arbolMenores :: Ord a => BST a -> a -> BST a
arbolMenores Empty a = Empty
arbolMenores (B Empty x Empty) a = if x <= a then (B Empty x Empty) else Empty
arbolMenores t1@(B l x Empty) a = if x <= a then t1 else arbolMenores l a
arbolMenores (B l x t2@(B l2 b r2)) a | x > a = arbolMenores l a
                                      | x < a && b > a = (B l x Empty)
                                      | otherwise = (B l x (arbolMenores t2 a))
                                          

splitBST ::  Ord a => BST a -> a -> (BST a, BST a)
splitBST t a = (arbolMenores t a, arbolMayores t a)
