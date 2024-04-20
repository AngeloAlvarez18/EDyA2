module Lab01 where

import Data.List

-- 1. Dar el tipo completo de las siguientes funciones:
-- a) test donde test f x = f x ≡ x + 2

-- test :: Num a => (a -> a) -> a -> Bool

-- b) esMenor donde esMenor y z = y < z

-- esMenor :: Ord a => a -> a -> Bool

-- c) eq donde eq a b = a ≡ b

-- eq :: Eq a => a -> a -> Bool

-- d) showVal donde showVal x = "Valor:" ++ show x

-- showVal :: Show a => a -> [Char]


-- 2. Dar el tipo de las siguientes operaciones y explicar su proposito:
-- a) (+5)

-- (+5) :: Num a => a -> a
-- Toma un numero y le suma cinco

-- b) (0<)

-- (0<) :: Num a => a -> a
-- Chequea si un numero es mayor a 0

-- c) (’a’:)

-- ('a':) :: [Char] -> [Char]
-- Agrega el caracter 'a' al principio de un string

-- d) (++"\n")

-- (++"\n") :: [Char] -> [Char]
-- Agrega un salto de linea al final de un string

-- e) filter (≡ 7)

-- filter (==7) :: Num a => [a] -> [a]
-- Toma una lista y devuelve los elementos q son iguales a 7

-- f) map (++[1])

-- map (++[1]) :: [Int] -> [Int]
-- Toma una lista de listas y le concatena la lista [1] al principio del primer elemento de la lista

-- 3. Dar al menos dos ejemplos de funciones que tengan el tipo indicado en cada caso:
-- a) (Int → Int) → Int

applyf :: (Int -> Int) -> Int
applyf f = f 1

applyg :: (Int -> Int) -> Int
applyg g = g 2

-- b) Int → (Int → Int)

sumax :: Int -> (Int -> Int)
sumax x = (+) x

multiplicax :: Int -> (Int -> Int)
multiplicax x = (*) x

-- c) (Int → Int) → (Int → Int)

componer :: (Int -> Int) -> (Int -> Int)
componer f = f . (+5)

componer2 :: (Int -> Int) -> (Int -> Int)
componer2 g = g . (*5)

-- d) Int → Bool

mayora0 :: Int -> Bool
mayora0 x = x > 0

espar :: Int -> Bool
espar x = x `mod` 2 == 0

-- e) Bool → (Bool → Bool )

andFunc :: Bool -> (Bool -> Bool)
andFunc x = (&&) x

orFunc :: Bool -> (Bool -> Bool)
orFunc x = (||) x

-- f) (Int, Char) → Bool

representaCaracter :: (Int, Char) -> Bool
representaCaracter (x, y) = fromEnum y == x

representaNum :: (Int, Char) -> Bool
representaNum (x, y) = toEnum x == y

-- g) (Int, Int) → Int

applyFuncTo2 :: (Int -> Int) -> Int
applyFuncTo2 f = f 2

applyFuncTo0 :: (Int -> Int) -> Int
applyFuncTo0 f = f 0

-- h) Int → (Int, Int)

pointInxAxis :: Int -> (Int, Int)
pointInxAxis x = (x, 0)

pointInyAxis :: Int -> (Int, Int)
pointInyAxis y = (0, y)

-- i) a → Bool

devuelveTrue :: a -> Bool
devuelveTrue x = True

devuelveFalse :: a -> Bool
devuelveFalse x = False

-- j) a → a

identidad2 :: a -> a
identidad2 x = x


-- 4. Indicar si cada una de las siguientes expresiones esta o no bien formada. En caso de que lo este determinar el
-- valor que denota, en caso contrario especificar si el error es sintactico o de tipos:

-- a) if true then false else true where false = True; true = False

-- Error de sintaxis

-- b) if if then then else else

-- Error de sintaxis

-- c) False ≡ (5 > 4)

-- Esta bien

-- d) 1 < 2 < 3

-- Error de tipo

-- e) 1 + if (’a’ < ’z’) then − 1 else 0

-- Esta bien

-- f) if fst p then fst p else snd p where p = (True, 2)

-- Error de sintaxis

-- g) if fst p then fst p else snd p where p = (True, False)

-- Error de sintaxis


-- 5. Reescribir cada una de las siguientes definiciones sin usar let, where o if :
-- a) f x = let (y, z ) = (x , x ) in y

-- f x = let (y, z) = (x , x) -- Declara la tupla (y,z) que es igual a (x, x)
--               in y  -- Devuelve el primer argumento de la tupla creada, o sea y = x

f x = x

-- b) greater (x , y) = if x > y then True else False

greater :: (Int, Int) -> Bool
greater (x, y) = x > y

-- c) f (x , y) = let z = x + y in g (z , y) where g (a, b) = a − b

f2 (x , y) = x


-- 6. Pasar de notacion Haskell a notacion de funciones anonimas (llamada notacion lambda),
-- a) smallest, definida por
-- smallest (x , y, z ) | x <= y && x <= z = x
--                      | y <= x && y <= z = y
--                      | z <= x && z <= y = z

smallest :: Int -> Int -> Int -> Int
smallest = \x y z -> if ((x <= y) && (x <= z)) then x else if ((y <= x) && (y <= z)) then y else z


-- b) second x = λx → x

second :: Int -> Int
second = \x -> x

-- c) andThen, definida por
-- andThen True y = y
-- andThen False y = False

--andThen :: Bool -> a -> a
andThen = \x y -> if x then y else x -- No se porque no FUNCA

-- d) twice f x = f (f x )

twice :: (Int -> Int) -> Int -> Int
twice = \f x -> f (f x)

-- e) flip f x y = f y x

flip = \f x y -> f y x

--f) inc = (+1)

inc = \x -> x + 1


-- 7. Pasar de notacion lambda a notacion Haskell,
-- a) iff = λx → λy → if x then not y else y
iff :: Bool -> Bool -> Bool
iff x y = if x then not y else y


-- 8. Suponiendo que f y g tienen los siguientes tipos
-- f :: c → d
-- g :: a → b → c
-- y sea h definida como
-- h x y = f (g x y)
-- Determinar el tipo de h e indicar cuales de las siguientes definiciones de h son equivalentes a la dada:
-- h = f ◦ g
-- h x = f ◦ (g x )
-- h x y = (f ◦ g) x y    (Esta es la correcta)
-- Dar el tipo de la funcion (◦)

-- h :: a -> b -> d

-- (.) :: (b -> c) -> (a- > b) -> (a -> c)

-- 9. La funcion zipp3 zipea 3 listas. Dar una definicion recursiva de la funcion y 
-- otra definicion con el mismo tipo que utilice la funcion zip. 
-- ¿Que ventajas y desventajas tiene cada definicion?

-- Aca entendi mal e hice una funcion q concatena 3 listas xD

-- zipp3 :: [a] -> [a] -> [a] -> [a]
-- zipp3 [] [] z = z
-- zipp3 [] (y:ys) z = y : zipp3 [] ys z
-- zipp3 (x:xs) y z = x : zipp3 xs y z

-- zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- zipp3 

zip_hecha :: [a] -> [b] -> [(a,b)]
zip_hecha [] y = []
zip_hecha x [] = []
zip_hecha (x:xs) (y:ys) = (x, y) : zip_hecha xs ys

zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3 [] [] z = []
zipp3 x [] [] = []
zipp3 [] y [] = []
zipp3 [] y z = []
zipp3 x [] z = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

zipp3_ :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3_ x y z = f x d 
    where 
        d = zip y z
        f [] b = []
        f a [] = []
        f (a:as) ((o,p):bs) = (a,o,p) : f as bs

-- La ventajas de usar zip es que tenemos menos casos base para la definicion de la funcion

-- 10. Indicar bajo que suposiciones tienen sentido las siguientes ecuaciones. Para aquellas que tengan sentido, indicar
-- si son verdaderas y en caso de no serlo modificar su lado derecho para que resulten verdaderas:
-- a) [[]] ++ xs = xs
-- Tiene sentido solo si xs = [[a]] o xs = [], [[]] ++ xs = [] : xs


-- b) [[]] ++ xs = [xs]
-- Tiene sentido solo si xs = [[a]] o xs = [], [[]] ++ xs = [] : xs

-- c) [[]] ++ xs = [] : xs
-- Tiene sentido solo si xs = [[a]] o xs = [], el resultado es verdadero

-- d) [[]] ++ xs = [[], xs]
-- Tiene sentido solo si xs = [[a]] o xs = [], [[]] ++ xs = [] : xs

-- e) [[]] ++ [xs] = [[], xs]
-- Tiene sentido, el resultado esta bien

-- f) [[]] ++ [xs] = [xs]
-- Tiene sentido, el resultado es [[], xs]

-- g) [] ++ xs = [] : xs
-- Tiene sentido solo si xs = [] o xs = [[a]], el resultado es [xs]

-- h) [] ++ xs = xs
-- Tiene sentido solo si xs = [] o xs = [[a]], el resultado ta bien

-- i) [xs] ++ [] = [xs]
-- Tiene sentido solo si xs = [] o xs = [[a]], el resultado ta bien

-- j) [xs] ++ [xs] = [xs, xs]
-- Tiene sentido solo si xs = [] o xs = [[a]], el resultado ta bien


-- 11. Inferir, de ser posible, los tipos de las siguientes funciones:
-- (puede suponer que sqrt :: Float → Float)


-- a) modulus = sqrt ◦ sum ◦ map (↑2)
modulus = sqrt . sum . map (^2)

-- b) vmod [ ] = [ ]
-- vmod (v : vs) = modulus v : vmod vs

-- ni idea

-- 12. Dado el siguiente tipo para representar numeros binarios:
-- type NumBin = [Bool]
-- donde el valor False representa el numero 0 y True el 1. Definir las siguientes operaciones tomando como convencion
-- una representacion Little-Endian (i.e. el primer elemento de las lista de dıgitos es el dıgito menos significativo del
-- numero representado).
-- a) suma binaria

-- type NumBin = [Bool]
-- bin_sum :: NumBin -> NumBin -> NumBin
-- bin_sum x y = if lengthh x >= y then f x y else 


-- (True, False, True) + (False, True)


-- b) producto binario
-- c) cociente y resto de la division por dos


-- 13. Definir las siguientes funciones usando listas por comprension:
-- a) divisors, que dado un entero positivo x devuelve la lista de los divisores de x (y la lista vacıa si el entero no es
-- positivo).

divisors n = [x | x <- [1..n], n `mod` x == 0]

-- b) matches, que dados un entero x y una lista de enteros descarta de la lista los elementos distintos a x .

matches n m = [x | x <- m, x == n]

-- c) cuadruplas, que dado un natural n, devuelve las cuadruplas (a, b, c, d ) con 0 < a, b, c, d , 6 n que cumplen
-- a ↑ 2 + b ↑ 2 = c ↑ 2 + d ↑ 2.

cuadruplas n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a*a + b*b == c*c + d*d]


-- d) unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
-- Por ejemplo, unique [1, 4, 2, 1, 3] = [1, 4, 2, 3].

-- 14. El producto escalar de dos listas de enteros de igual longitud es la suma de los productos de los elementos
-- sucesivos (misma posicion) de ambas listas. Usando listas por comprension defina una funcion scalarproduct que
-- devuelva el producto escalar de dos listas.

-- hecho en lab1
