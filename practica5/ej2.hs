-- Tomaremos a las secuencias como una lista

nth :: Int -> [a] -> a
nth n s = s !! n

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [0..n-1]


fibSeq :: Int -> [Int]
fibSeq n = let s = tabulate matriz n
               s' = scanl aux [0,1,0,1] s
            in drop 1 (map (nth 1) s')


matriz :: Int -> [Int]
matriz n = [1,1,1,0]

aux :: [Int] -> [Int] -> [Int]
aux a b = multMatriz a b

multMatriz :: [Int] -> [Int] -> [Int]
multMatriz a b = let x = ((nth 0 a) * (nth 0 b)) + ((nth 1 a) * (nth 2 b))
                     y = ((nth 0 a) * (nth 1 b)) + ((nth 1 a) * (nth 3 b))
                     z = ((nth 2 a) * (nth 0 b)) + ((nth 3 a) * (nth 2 b))
                     w = ((nth 2 a) * (nth 1 b)) + ((nth 3 a) * (nth 3 b))
                 in [x,y,z,w]