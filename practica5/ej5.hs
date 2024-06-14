-- Tomaremos a las secuencias como una lista

data TreeView a = EMPTY | ELT a | NODE [a] [a]

showT :: [a] -> TreeView a
showT s | length s == 0 = EMPTY
        | length s == 1 = ELT (head s) 
        | length s > 1 = NODE (take (div (length s) 2) s) (drop (div (length s) 2) s)


scmmlAux :: [Int] -> (Int, Int, Int, Int)
scmmlAux s = case (showT s) of
            EMPTY -> (0,0,0,0)
            ELT v -> toTuple v
            NODE l r -> let l' = scmmlAux l
                            r' = scmmlAux r
                        in combineTuple l' r'

toTuple :: Int -> (Int, Int, Int, Int)
toTuple n = (n, n, 1, 1)

combineTuple :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
combineTuple (a,b,c,d) (x,y,z,w) | x > b = let actual = c + z
                                               historico = max d w
                                               now = max actual historico
                                            in (a, y, actual, now)
                                 | otherwise = (a, y, 1, max d w)


scmml :: [Int] -> Int
scmml s = let 
            (a,b,c,d) = scmmlAux s
            result = (d-1)
        in result