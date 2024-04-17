module Lab01 where

import Data.List
import Prelude.hs

-- 1. Dar el tipo completo de las siguientes funciones:
-- a) test donde test f x = f x ≡ x + 2

-- test :: Num a => (a -> a) -> a -> Bool

-- b) esMenor donde esMenor y z = y < z

-- esMenos :: Num a => a -> a -> Bool

-- c) eq donde eq a b = a ≡ b

-- eq :: a -> a -> Bool

-- d) showVal donde showVal x = "Valor:" ++ show x

-- showVal :: a -> [Char]


-- 2. Dar el tipo de las siguientes operaciones y explicar su proposito:
-- a) (+5)
-- b) (0<)
-- c) (’a’:)
-- d) (++"\n")
-- e) filter (≡ 7)
-- f) map (++[1])