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



-- b) greater (x , y) = if x > y then True else False

-- c) f (x , y) = let z = x + y in g (z , y) where g (a, b) = a − b