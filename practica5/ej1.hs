-- Tomaremos a las secuencias como una lista

nth :: Int -> [a] -> a
nth n s = s !! n

tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [0..n-1]


promedios :: [Int] -> [Float]
promedios s = tabulate f ((length s))
                    where lr = scanl (+) 0 s
                          s' = drop 1 lr
                          f i = (fromIntegral (nth i s')) / (fromIntegral (i+1))


    