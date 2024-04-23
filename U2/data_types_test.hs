module DataTypes where

import Data.List

data Dia = Dom | Lun | Mar | Mie | Jue | Vie | Sab deriving (Eq, Ord, Enum, Show)

data Eitherr a b = Leftt a | Rightt b deriving Show

testt :: Int -> Eitherr Int Float
testt 0 = Leftt 2
testt x = Rightt 2.5

data Shape = Circle Float | Rect Float Float deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

data Maybee a = Nothingg | Justt a deriving Show

safehead :: [a] -> Maybee a 
safehead [] = Nothingg
safehead xs = Justt (head xs)


data T1 a = Tip a | Bin (T1 a) (T1 a) deriving Show
data T2 b = Empty | Branch (T2 b) b (T2 b) deriving Show
data T3 a b = Leaf a | Node (T3 a b) b (T3 a b) deriving Show
data T4 a = E | N2 a (T4 a) (T4 a) | N3 a (T4 a) (T4 a) (T4 a) deriving Show
data T5 a = Rose a [T5 a] deriving Show

size :: T1 a -> Int
size (Tip _) = 1
size (Bin t1 t2) = size t1 + size t2

depth :: T1 a -> Int
depth (Tip _) = 0
depth (Bin t1 t2) = 1 + (depth t1) `max` (depth t2)

-- b = Bin (Tip 2) (Bin (Tip 3) (Tip 4))

