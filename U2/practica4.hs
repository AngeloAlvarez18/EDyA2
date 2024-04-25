-- parcial 1 2023

type Interval = (Int, Int)

data ITree = E | N ITree Interval ITree deriving Show

right :: ITree -> Int
right (N _ (x, y) E) = y
right (N _ (x, y) r)  = right r

darValorX :: ITree -> Int
darValorX (N _ (x, y) _) = x

darValorY :: ITree -> Int
darValorY (N _ (x, y) _) = y

checkIT :: ITree -> Bool
checkIT E = True
checkIT (N E (x, y) E) = x <= y
checkIT (N l (x, y) r) = (x <= y) && (darValorY l < (x -1)) && (y + 1 < darValorX r) && (checkIT l) && (checkIT r)


splitMax :: ITree -> (Interval, ITree)
-- splitMax (N l (x,y) E) = ((x,y), l)
-- splitMax (N l (x,y) r) = 
