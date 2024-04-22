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