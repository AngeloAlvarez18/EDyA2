module Lab01 where

import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
not1 b = case b of
    True  -> False
    False -> True

-- b)
ins [x]         =  []
ins (x:xs)      =  x : ins xs
ins []          =  error "empty list"

-- c)
lengthh []        =  0
lengthh (_:l)     =  1 + lengthh l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys -- funciona por el orden de precedencia (primero hace el ++!)

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
listmin xs = (head . sort) xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap (smap f) xs


-- 2. Definir las siguientes funciones y determinar su tipo:

-- a) five, que dado cualquier valor, devuelve 5
five a = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de 
-- aplicar la función al valor dado
apply f a = f a


-- c) ident, la función identidad
ident a = a

-- d) first, que toma un par ordenado, y devuelve su primera componente
first (a,b) = a

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive f a = (f (a + 0.0001) - f a) / 0.0001

-- f) sign, la función signo
sign a 
    |a > 0 = 1
    |a < 0 = -1
    |otherwise = 0

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs n
    | n < 0 = (-n)
    |otherwise = n

-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero
pot a b = a^b

-- i) xor, el operador de disyunción exclusiva
xor False False = False
xor False True  = True
xor True False = True
xor True True = False


-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3 a b c = max (max a b) c
-- deberia funcionar con numeros iguaes?

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap (a,b) = (b,a)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

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

infixl 6 *$
ns *$ n = map (*n) (ns)  

v = [1, 2, 3] *$ 2 *$ 4



{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors n = [x | x <- [1..n], n `mod` x == 0]

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

matches n ns = [x | x <- ns, x /= n]

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

como funciona:

1- zip xs [0..]: Esta expresión crea una lista de tuplas donde cada elemento de xs se combina con su índice correspondiente. Por ejemplo, si xs = [3,1,2], entonces zip xs [0..] produce [(3,0),(1,1),(2,2)].

2- (x,i) <- zip xs [0..]: Utiliza una lista por comprensión para recorrer la lista de tuplas generada por zip. En cada iteración, x es el elemento de xs y i es su índice correspondiente.

3- take i xs: Toma los primeros i elementos de xs. Esto significa que en cada iteración estamos tomando los elementos anteriores a x en la lista original.

4- not (elem x (take i xs)): Verifica si x no está presente en los elementos anteriores a él en la lista original. En otras palabras, estamos verificando si x es único hasta ese punto en la lista original.

5- [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]: Esta es la parte principal de la función. Estamos utilizando una lista por comprensión para iterar sobre las tuplas generadas por zip. En cada iteración, si x es único hasta ese punto en la lista original, se agrega a la lista resultante.
-}

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. 

productoLista lista = foldr (*) 1 lista
scalarProduct xs ys = sum (map productoLista [[x,y] | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..], i==j])

-}

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
