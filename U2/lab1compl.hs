module Lab1C where

import Data.List
import Data.Ord

type Texto = String

{-
   Definir una función que dado un caracter y un texto calcule la frecuencia 
   con la que ocurre el caracter en el texto
   Por ejemplo: frecuency 'a' "casa" = 0.5 
-}

lengthh :: [a] -> Float
lengthh [] =  0
lengthh (_:l) =  1 + lengthh l

--frecuency :: Char -> Texto -> Float
frecuency c s = (lengthh x) / (lengthh s) where x = [a | a <- s, a == c]

{-                      
  Definir una función frecuencyMap que dado un texto calcule la frecuencia 
  con la que ocurre cada caracter del texto en éste.
  La lista resultado debe estar ordenada respecto a la frecuencia con la que ocurre 
  cada caracter, de menor a mayor frecuencia. 
    
  Por ejemplo: frecuencyMap "casa" = [('c',0.25),('s',0.25),('a',0.5)]

-}

merge ([], ys) = ys
merge (xs, []) = xs
merge ((a,b):xs, (c,d):ys) = if b <= d then (a,b):merge(xs, (c,d):ys) else (c,d):merge((a,b):xs,ys)

split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs,ys) = split zs in (x:xs, y:ys)

mergeSortTuple [] = []
mergeSortTuple [x] = [x]
mergeSortTuple xs = let (ls, rs) = split xs
                        ls' = mergeSortTuple ls
                        rs' = mergeSortTuple rs
                    in merge (ls', rs')


unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

frecuencyMap :: Texto -> [(Char, Float)]
frecuencyMap s =  mergeSortTuple (unique ([(a,frecuency a s) | a <- s]))

{-
  Definir una función subconjuntos, que dada una lista xs devuelva una lista 
  con las listas que pueden generarse con los elementos de xs.

  Por ejemplo: subconjuntos [2,3,4] = [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
-}

subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = map (x:) (subconjuntos xs) ++ subconjuntos xs

-- map 2: (map 3: subconjuntos [4])
-- map 2: (map 3: (map 4: subconjuntos []))
-- map 2: (map 3: (map 4: [[]]))
-- map 2: (map 3: [[4]])
-- map 2: [[3,4]]
-- [[2,3,4]]


{-
 Definir una función intercala :: a -> [a] -> [[a]]
 tal que (intercala x ys) contiene las listas que se obtienen
 intercalando x entre los elementos de ys. 
 
 Por ejemplo: intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
-}

--ponerEnIndice :: Int -> [Int] -> Int -> [Int]
ponerEnIndice a xs 0 = a : xs
ponerEnIndice a (x:xs) i = x : ponerEnIndice a xs (i - 1)

-- ponerEnIndice 1 [2,3] 2 = 2 : ponerEnIndice 1 [3] 1
-- 2 : 3 : ponerEnIndice 1 [] 0
-- 2 : 3 : 1 : []


--intercala :: a -> [a] -> [[a]]
intercala x ys = [ponerEnIndice x ys i | i <- [0.. (length ys)]]


{- 
  Definir una función permutaciones que dada una lista calcule todas las permutaciones
  posibles de sus elementos. Ayuda: la función anterior puede ser útil. 

  Por ejemplo: permutaciones "abc" = ["abc","bac","cba","bca","cab","acb"]
-}                  

"abc" "bac" "bca" "acb" "cab"

"abc" -> "ba"

sacarElemento 0 (x:xs) = xs
sacarElemento i (x:xs) = x : sacarElemento (i-1) xs

tomarElemento 0 (x:xs) = x
tomarElemento i (x:xs) = tomarElemento (i-1) xs

listaSinElemento xs = [sacarElemento i xs | i <- [0..(length xs)-1]]

permutacionesRepetidas [] ys = []
permutacionesRepetidas (x:xs) (y:ys) = intercala x y : permutacionesRepetidas xs ys

permutacionesAux [] = []
permutacionesAux (x:xs) = x ++ permutacionesAux xs

permutaciones xs = unique (permutacionesAux (permutacionesRepetidas xs (listaSinElemento xs)))