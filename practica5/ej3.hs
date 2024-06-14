-- Tomaremos a las secuencias como una lista

--nth :: Int -> [a] -> a
nth n s = s !! n

--tabulate :: (Int -> a) -> Int -> [a]
tabulate f n = map f [0..n-1]

--aguaHist :: [Int] -> Int
aguaHist s | length s < 3 = 0
            | otherwise = let maxLL = scanl max 0 s
                              reverseList = reverse s
                              maxRR = scanl max 0 reverseList
                              list = tabulate (aguaArriba maxLL maxRR s) (length s)
                          in foldl (+) 0 list

--iesimoElem :: [Int] -> Int -> Int -> Int
iesimoElem s n i = nth (n - 1 - i) s

--aguaArriba :: [Int] -> [Int] -> [Int] -> Int -> [Int]
aguaArriba a b s i = let maxL = nth i a
                         maxR = nth ((length s) - 1 - i) b
                         altura = nth i s
                    in max 0 ((min maxL maxR) - altura)

s = [2,3,4,7,5,2,3,2,6,4,3,5,2,1]