-- 4)

data T a = E | N (T a) a (T a) deriving Show

altura :: T a -> Int
altura E = 0
altura (N l x r ) = 1 + max (altura l) (altura r)

-- a)

combinar :: T a -> T a -> T a
combinar E t = t
combinar t E = t
combinar (N l a r) (N (N lr b' rr) b E) = N (combinar l (N lr b' E)) a (combinar r (N rr b E))
combinar (N l a r) (N l' b r') = N (combinar l (N l' b E)) a (combinar r r')


a = N (N (N E 4 E) 2 (N E 5 E)) 1 (N (N E 6 E) 3 (N E 7 E))
b = N (N (N E 11 E) 9 (N E 12 E)) 8 (N (N E 13 E) 10 (N E 14 E))

N (N (N (N E 11 E) 4 (N E 9 E)) 2 (N (N E 12 E) 5 (N E 8 E))) 1 (N (N (N E 13 E) 6 (N E 10 E)) 3 (N (N E 14 E) 7 E))