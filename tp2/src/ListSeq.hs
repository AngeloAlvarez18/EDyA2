import Par
import Seq

instance Seq [] where
    emptyS = []

    singletonS a = [a]

    lengthS = length

    nthS (x:xs) n = if n == 0 then x else nthS (n-1) 0

    tabulateS f n = map f [1..n]

    mapS f [] = []
    mapS f (x:xs) = (f x) : mapS f xs

    filterS f [] = []
    filterS f (x:xs) = if (f x) then x : filterS f xs else filterS f xs

    appendS a b = a ++ b

    takeS s i = take i s

    dropS s i = drop i s

    showtS s | lengthS s == 0 = EMPTY
             | lengthS s == 1 = ELT (nthS s 0)
             | lengthS s > 1 = NODE (take (div (length s) 2) s) (drop (div (length s) 2) s)

    showlS s | lengthS s == 0 = NIL
             | lengthS s > 1 = CONS (nthS s 0) (dropS s 1)

    joinS [] = []
    joinS (x:xs) = appendS x (joinS xs)

    reduceS f e s = case showtS of
        EMPTY -> e
        ELT a -> a
        NODE l r -> let (l', r') = reduceS f e l ||| reduceS f e r
                    in f l' r'



-- Wfilter(n) = W(n-1) + W(f)  ---> W(n) = Sum(W(f x), x pertenece xs)

-- Wreduce(n) = 2W(n/2) + W(take) + W(drop) + W(f) 

-- SI f pertenece O(1) entonces Wreduce(n) = O(n log n)