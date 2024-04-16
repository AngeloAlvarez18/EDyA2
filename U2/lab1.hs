module Lab01 where

import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
nott :: Bool -> Bool
nott b = case b of
    True -> False
    False -> True

-- -- b)
ins :: [a] -> [a]
ins [x]         =  []
ins (x:xs)      =  x : ins xs
ins []          =  error "empty list"

-- -- c)
lengthh :: [a] -> Int
lengthh []        =  0
lengthh (_:l)     =  1 + lengthh l

-- -- d)
list123 = 1 : (2 : (3 : ([])))

-- -- e)
-- Concatena la primer lista al principio de la segunda
(++!) :: [a] -> [a] -> [a]
[] ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- -- f)
addToTail :: Int -> [Int] -> [Int] -- EN vez de Int podria ser Float o Integer
addToTail x xs = map (+x) (tail xs)

-- -- g)
--listmin :: [a] -> a
listmin xs = (head . sort) xs

-- -- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs



-- 2. Definir las siguientes funciones y determinar su tipo:
-- a) five, que dado cualquier valor, devuelve 5

five :: a -> Int
five x = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de
-- aplicar la función al valor dado

--apply :: (a -> a) -> a -> a
apply f x = f x

-- c) ident, la función identidad
ident :: a -> a
ident a = a

-- d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a, a) -> a
first (a, b) = a

-- e) derive, que aproxima la derivada de una función dada en un punto dado
--derive :: (a -> a) -> a -> a
derive f a = (f (a + 0.0001) - f a) / 0.0001

-- f) sign, la función signo
--sign :: Num a => a -> a
--sign :: Num a => a -> a
sign a | a < 0 = -1
       | a == 0 = 0
       | a > 0 = 1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
--vabs :: Num a => a -> a
--vabs a = a * (sign a) -- con sign
vabs a = if a >= 0 then a else - a


-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero
--pot :: Int -> Num -> Int
pot x y = y^x


-- i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3 :: Int -> Int -> Int -> Int
max3 x y z | (x >= y) && (x >= z) = x
           | (y >= x) && (y >= z) = y
           | otherwise = z

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
--swap :: (a, b) -> (a, b)
swap (a, b) = (b, a)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
bisiesto :: Int -> Bool
bisiesto x | vabs(2024 - x) % 4 == 0 = True

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}

-- v = [1, 2, 3] *$ 2 *$ 4


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

-- unique :: [Int] -> [Int]
-- unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}