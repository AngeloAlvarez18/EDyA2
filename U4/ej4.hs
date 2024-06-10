-- 4)

data T a = E | N (T a) a (T a) deriving Show

altura :: T a -> Int
altura E = 0
altura (N l x r ) = 1 + max (altura l) (altura r)

-- a)

-- combinar :: T a -> T a -> T a
-- combinar E t = t
-- combinar t E = t
-- combinar (N l a r) (N (N lr b' rr) b E) = N (combinar l (N lr b' E)) a (combinar r (N rr b E))
-- combinar (N l a r) (N l' b r') = N (combinar l (N l' b E)) a (combinar r r')


a = N (N (N E 4 E) 2 (N E 5 E)) 1 (N (N E 6 E) 3 (N E 7 E))
b = N (N (N E 11 E) 9 (N E 12 E)) 8 (N (N E 13 E) 10 (N E 14 E))

--tomarMenor :: T Int -> (Int, T Int)
tomarMenor (N E a r) = (a, r)
tomarMenor (N (N E b rr) a r) = (b, N rr a r)
tomarMenor (N l a r) = let (x, tl) = tomarMenor l
                        in (x, (N tl a r))

--combinar :: T Int -> T Int -> T Int
combinar E t = t
combinar t1@(N l a r) t = let (x, t') = tomarMenor t1
                        in (N t' x t)



--filterT :: (Int -> Bool) -> T Int -> T Int
filterT f E = E
filterT f (N l a r) | (f a) = N (filterT f l) a (filterT f r)
                   | otherwise = combinar (filterT f l) (filterT f r)

--par :: Int -> Bool
par n = (mod n 2) == 0

quickSort :: T Int -> T Int
quickSort E = E
quickSort t@(N l a r) = let lr = filterT (<a) t 
                            rr = filterT (>a) t
                    in N (quickSort lr) a (quickSort rr)
