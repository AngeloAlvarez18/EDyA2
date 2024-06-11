data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

size :: BTree a -> Int
size Empty = 0
size (Node n _ _ _) = n

altura :: BTree a -> Int
altura Empty = 0
altura (Node n l a r) = 1 + (max (altura l) (altura r))

toList :: BTree a -> [a]
toList Empty = []
toList (Node n l a r) = (toList l) ++ [a] ++ (toList r)

splitAtt :: BTree a -> Int -> (BTree a, BTree a)
splitAtt Empty n = (Empty, Empty)
splitAtt t@(Node m l a r) n | n == m = (t, Empty)
                        | n == sl = (l, (Node (m - sl) Empty a r))
                        | n == (sl+1) = ((Node (sl+1) l a Empty), r)
                        | n < sl = let (lr, rr) = splitAtt l n
                                    in (lr, (Node (m-n) rr a r))
                        | otherwise = let (lr, rr) = splitAtt r (n - (sl+1))
                                        in ((Node n l a lr), rr)
                            where sl = size l


a = Node 7 (Node 3 (Node 1 Empty 1 Empty) 2 (Node 1 Empty 3 Empty)) 4 (Node 3 (Node 1 Empty 5 Empty) 6 (Node 1 Empty 7 Empty))
d = Node 8 (Node 5 (Node 1 Empty 1 Empty) 2 (Node 3 (Node 2 Empty 3 (Node 1 Empty 4 Empty)) 5 Empty)) 6 (Node 2 Empty 7 (Node 1 Empty 8 Empty))