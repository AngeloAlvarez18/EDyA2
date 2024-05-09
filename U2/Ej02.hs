module Ej02 where

-- 2)

data BST a = Hoja | BSTNodo (BST a) a (BST a) deriving Show

maximumBST :: Ord a => BST a -> a
maximumBST (BSTNodo l a Hoja) = a 
maximumBST (BSTNodo l a r) = maximumBST r

minimumBST :: Ord a => BST a -> a
minimumBST (BSTNodo Hoja a r) = a 
--minimumBST (BSTNodo l a Hoja) = a 
minimumBST (BSTNodo l a r) = minimumBST l

bst1 = BSTNodo (BSTNodo (BSTNodo Hoja 1 Hoja) 2 (BSTNodo Hoja 3 Hoja)) 4 (BSTNodo (BSTNodo Hoja 6 Hoja) 7 (BSTNodo Hoja 8 (BSTNodo Hoja 9 Hoja)))

bst2 = BSTNodo (BSTNodo Hoja 7 Hoja) 6 (BSTNodo Hoja 8 (BSTNodo Hoja 9 Hoja))

bst3 = BSTNodo Hoja 3 Hoja


checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True 
checkBST (BSTNodo Hoja a Hoja) = True
checkBST (BSTNodo Hoja a r) = checkBST r && (minimumBST r > a)
checkBST (BSTNodo l a Hoja) = checkBST l && (maximumBST l <= a)
checkBST (BSTNodo l a r) = checkBST l && checkBST r && (maximumBST l <= a) && (minimumBST r > a)


insertBST :: Ord a => BST a -> a -> BST a 
insertBST Hoja new = BSTNodo Hoja new Hoja
insertBST (BSTNodo l a r) new
  | new == a = BSTNodo l a r
  | new < a  = BSTNodo (insertBST l new) a r
  | new > a  = BSTNodo l a (insertBST r new)


borrarBST :: Ord a => BST a -> a -> BST a 
borrarBST Hoja _ = Hoja
borrarBST (BSTNodo l a r) b
--  | b == a = BSTNodo l (minimumBST r) (borrarBST r (minimumBST r))
  | b == a = borrarBSTAux (BSTNodo l a r)
  | b < a = BSTNodo (borrarBST l b) a r
  | b > a = BSTNodo l a (borrarBST r b)

borrarBSTAux :: Ord a => BST a -> BST a 
borrarBSTAux (BSTNodo Hoja a Hoja) = Hoja 
borrarBSTAux (BSTNodo Hoja a r) = r 
borrarBSTAux (BSTNodo l a Hoja) = l 
borrarBSTAux (BSTNodo l a r) = BSTNodo l (minimumBST r) (borrarBST r (minimumBST r))

eliminarMenorIgual :: Ord a => BST a -> a -> BST a
eliminarMenorIgual Hoja _ = Hoja
eliminarMenorIgual (BSTNodo l raiz r) b
  | b == raiz = r
  | b < raiz  = BSTNodo (eliminarMenorIgual l b) raiz r
  | b > raiz  = eliminarMenorIgual r b

eliminarMayor :: Ord a => BST a -> a -> BST a
eliminarMayor Hoja _ = Hoja
eliminarMayor (BSTNodo l raiz r) b
  | b == raiz = BSTNodo l raiz Hoja
  | b < raiz = eliminarMayor l b
  | b > raiz = BSTNodo l raiz (eliminarMayor r b)

splitBST:: Ord a => BST a -> a -> (BST a, BST a)
splitBST arbol num = (eliminarMayor arbol num, eliminarMenorIgual arbol num)

splitBST2 :: Ord a => BST a -> a -> (BST a, BST a) 
splitBST2 Hoja _ = (Hoja, Hoja)
splitBST2 (BSTNodo l raiz r) a 
  | raiz == a = (BSTNodo l raiz Hoja, r)
  | raiz > a  = let (x,y) = splitBST2 l a in (x, BSTNodo y raiz r) 
  | raiz < a  = let (x,y) = splitBST2 r a in (BSTNodo l raiz x, y)


join :: Ord a => BST a -> BST a -> BST a
join Hoja arbol = arbol
join arbol Hoja = arbol
join arbol1 (BSTNodo l2 a2 r2) = let (x,y) = splitBST2 arbol1 a2 in BSTNodo (join x l2) a2 (join y r2)


bst10 = BSTNodo Hoja 1 (BSTNodo Hoja 2 Hoja)
bst11 = BSTNodo (BSTNodo Hoja 3 Hoja) 4 (BSTNodo Hoja 6 Hoja)

