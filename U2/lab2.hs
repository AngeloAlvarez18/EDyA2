data BTree a = E | Leaf a | Node (BTree a) (BTree a) deriving Show

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = 0
altura (Leaf a) = 0
altura (Node t1 t2) = 1 + (altura t1 `max` altura t2)

--Node (Btree (Node (Leaf 1) (Leaf 2))) (Leaf 3)

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = True
perfecto (Leaf a) = True
perfecto (Node (Leaf a) E) = False
perfecto (Node E (Leaf a)) = False
perfecto (Node t1 t2) = if altura t1 == altura t2 then True && (perfecto t1) && (perfecto t2) else False

--Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Node (Leaf 5) (Node E (Leaf 4)))
--Node (Node (Node (Node E E) E) E) E
--Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
--Node (Node (Leaf 1) E) (Node E (Leaf 2))

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf a) = a : []
inorder (Node t1 t2) = inorder t1 ++ inorder t2

-- a = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Node (Leaf 5) (Node E (Leaf 4)))

-- inorder (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) : inorder (Node (Leaf 5) (Node E (Leaf 4)))
-- [inorder (Leaf 1) : inorder (Node (Leaf 2) (Leaf 3)) ] : [inorder Leaf 5 : inorder (Node E (Leaf 4))]
-- [1] : (inorder Leaf 2 : Leaf 3) : [[5] : inorder E : inorder Leaf 4]
-- [1] : [2] : [3] : [5] : [] : [4]


-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):



{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

data GTree a = EG | NodeG a [GTree a] deriving Show

data BinTree a = EB | NodeB (BinTree a) a (BinTree a) deriving Show

g2btAux :: [GTree a] -> BinTree a
g2btAux [] = EB
g2btAux [NodeG a xs] = NodeB (g2btAux xs) a EB
g2btAux ((NodeG a xs) : ts) = NodeB (g2btAux xs) a (g2btAux ts)

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt (NodeG a []) = NodeB EB a EB
g2bt (NodeG a xs) = NodeB (g2btAux xs) a EB

--NodeB (NodeB (NodeB EB 4 EB) 2 (NodeB EB 3 EB)) 1 EB

-- NodeG 1 [NodeG 2 [NodeG 6 [NodeG 11 [],NodeG 12 []],NodeG 7 [],NodeG 8 []],NodeG 3 [],NodeG 4 [],NodeG 5 [NodeG 9 [NodeG 13 []],NodeG 10 []]]

-- NodeB (NodeB (NodeB (NodeB EB 11 (NodeB EB 12 EB)) 6 (NodeB EB 7 (NodeB EB 8 EB))) 2 (NodeB EB 3 (NodeB EB 4 (NodeB (NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)) 5 EB)))) 1 EB
-- NodeB (NodeB (NodeB EB 11 (NodeB EB 12 EB)) 6 (NodeB EB 7 (NodeB EB 8 EB))) 2 (NodeB EB 3 (NodeB EB 4 (NodeB (NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)) 5 EB)))
-- NodeB (NodeB EB 11 (NodeB EB 12 EB)) 6 (NodeB EB 7 (NodeB EB 8 EB))

-- NodeB EB 3 (NodeB EB 4 (NodeB (NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)) 5 EB))
-- NodeB EB 4 (NodeB (NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)) 5 EB)
-- NodeB (NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)) 5 EB
-- NodeB (NodeB EB 13 EB) 9 (NodeB EB 10 EB)




-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

dcn :: BinTree a -> [a]
dcn = undefined

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2   -}

maxn :: BinTree a -> Int
maxn = undefined

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar = undefined

