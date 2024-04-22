module Pract3 where

import Data.List

-- 1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios. Cualquier
-- otro color se expresa en terminos de los porcentajes de cada uno estos tres colores que es necesario combinar
-- en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada color de manera biunivoca, por lo que
-- usualmente se utilizan estos valores como representacion de un color.
-- Definir un tipo Color en este modelo y una funcion mezclar que permita obtener el promedio componente a
-- componente entre dos colores.

data Colours = Red Float | Green Float | Blue Float deriving Show

type Color = (Colours, Colours, Colours)


mezclar :: Color -> Color -> (Float, Float, Float)
mezclar (Red r, Green g, Blue b) (Red r', Green g', Blue b') = ((r+r')/2, (g+g')/2, (b+b')/2)


-- 2. Consideremos un editor de lineas simple. Supongamos que una Linea es una secuencia de caracteres c1, c2, . . . , cn
-- junto con una posicion p, siendo 0 <= p <= n, llamada cursor (consideraremos al cursor a la derecha de un caracter
-- que sera borrado o insertado, es decir como el cursor de la mayoria de los editores). Se requieren las siguientes
-- operaciones sobre lineas:
-- vacia :: Linea
-- moverIzq :: Linea → Linea
-- moverDer :: Linea → Linea
-- moverIni :: Linea → Linea
-- moverFin :: Linea → Linea
-- insertar :: Char → Linea → Linea
-- borrar :: Linea → Linea
-- La descripcion informal es la siguiente: (1) la constante vacia denota la linea vacia, (2) la operacion moverIzq
-- mueve el cursor una posicion a la izquierda (siempre que ello sea posible), (3) analogamente para moverDer , (4)
-- moverIni mueve el cursor al comienzo de la linea, (5) moverFin mueve el cursor al final de la linea, (6) la operacion
-- borrar elimina el caracterer que se encuentra a la izquierda del cursor, (7) insertar agrega un caracter en el lugar
-- donde se encontraba el cursor, dejando al caracter insertado a su izquierda.
-- Definir un tipo de datos Linea e implementar las operaciones dadas

data Linea = L {caracteres :: String, posicion :: Int} | Indefinido deriving Show

vacia :: Linea
vacia = L "" 0

moverIzq :: Linea -> Linea
moverIzq (L s 0) = L s 0
moverIzq (L s p) = if (length s) < p || (length s) < 0 then Indefinido else L s (p - 1) 

moverDer :: Linea -> Linea
moverDer (L s p) | (length s) < p || (length s) < 0 = Indefinido
                 | (p + 1) > (length s) = L s p 
                 | otherwise = L s (p+1)


moverIni :: Linea -> Linea
moverIni (L s p) | (length s) < p || (length s) < 0 = Indefinido
                 | otherwise = L s 0


moverFin :: Linea -> Linea
moverFin (L s p) | (length s) < p || (length s) < 0 = Indefinido
                 | otherwise = L s (length s)



insertar :: Char -> Linea -> Linea
insertar c (L (x:xs) 0) = L (c : (x:xs)) 0
insertar c (L (x:xs) p) = L (x : (caracteres (insertar c (L xs (p-1))))) p

-- insertar 'z' "hola" 2 = L ('h' : (caracteres insertar 'z' "ola" 1)) 2
-- L ('h' : caracteres (L ('o' : (caracteres (insertar 'z' "la" 0))) 1)) 2
-- L ('h' : caracteres (L ('o' : (caracteres (L "zla" 0))) 1)) 2
-- L ('h' : caracteres (L ('o' : ("zla")) 1)) 2
-- L ('h' : caracteres (L ("ozla")) 1) 2
-- L ('h' : "ozla") 2
-- L "hozla" 2

borrar :: Linea -> Linea
borrar (L s 0) = L s 0
borrar (L (x:xs) 1) = L xs 0
borrar (L (x:xs) p) = L (x : caracteres (borrar (L xs (p-1)))) (p-1) 


-- 3. Dado el tipo de datos
-- data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
-- a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
-- Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.
-- headCL y tailCL no estan definidos para una lista vacia.
-- headCL toma una CList y devuelve el primer elemento de la misma (el de mas a la izquierda).
-- tailCL toma una CList y devuelve la misma sin el primer elemento.
-- isEmptyCL aplicado a una CList devuelve True si la CList es vacia (EmptyCL) o False en caso contrario.
-- isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso
-- contrario.
-- b) Definir una funcion reverseCL que toma una CList y devuelve su inversa.
-- c) Definir una funcion inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.
-- d) Definir una funcion lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la
-- CList.
-- e) Definir una funcion concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL :: CList a -> Maybe a
headCL EmptyCL = Nothing
headCL (CUnit a) = Just (CUnit a)
headCL (Consnoc a _ _) = Just (CUnit a)

-- tailCL :: CList a -> Maybe a
-- tailCL EmptyCL = Nothing
-- tailCL (CUnit a) = Just (CUnit a)
-- tailCL (Consnoc _ _ a) = Just (Consnoc )