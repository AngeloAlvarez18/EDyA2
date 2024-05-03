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
