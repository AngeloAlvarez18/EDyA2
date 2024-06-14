-- Tomaremos a las secuencias como una lista

nth :: Int -> [a] -> a
nth n s = s !! n

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [0..n-1]

data Paren = Open | Close

-- a)

matchParen :: [Paren] -> Bool
matchParen s = matchP s == (0,0)

data TreeView a = EMPTY | ELT a | NODE [a] [a]

showT :: [a] -> TreeView a
showT s | length s == 0 = EMPTY
        | length s == 1 = ELT (head s) 
        | length s > 1 = NODE (take (div (length s) 2) s) (drop (div (length s) 2) s)

matchP :: [Paren] -> (Int, Int)
matchP s = case (showT s) of 
            EMPTY -> (0,0)
            ELT v -> toTuple v
            NODE l r -> let l' = matchP l 
                            r' = matchP r
                        in sumTuple l' r'

toTuple :: Paren -> (Int, Int)
toTuple Open = (1,0)
toTuple Close = (0,1)

sumTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuple (a,b) (c,d) | a < b = (a+c, b+d)
                     | otherwise = let 
                                    (x,y) = (a+c, b+d)
                                    m = min x y
                                    in (x - m, y - m)

-- b)

matchParen2 :: [Paren] -> Bool
matchParen2 s = let ls = scanl sumTuple2 (0, 0, True) (map toTuple2 s)
                    (a,b,c) = nth ((length ls) - 1) ls
                in (a == b) && c


toTuple2 :: Paren -> (Int, Int, Bool)
toTuple2 Open = (1,0, True)
toTuple2 Close = (0,1,True)

sumTuple2 :: (Int, Int, Bool) -> (Int, Int, Bool) -> (Int, Int, Bool)
sumTuple2 (a,b,c) (x,y,z) | a < y = (a+x, b+y, False)
                          | otherwise = (a+x, b+y, c && z && True)