module Recursividade
  (
    inserir,
    ordenar,
    fatorialFix,
    somar,
    pow
  )
where

inserir :: Ord t => t -> [t] -> [t]
inserir x [] = [x]
inserir x (y:ys)
 | x <= y = x : y : ys
 | otherwise = y : inserir x ys

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)

--Q1
fatorialFix :: (Ord t, Num t) => t -> t
fatorialFix x 
  | x < 0 = error "Não existe fatorial para números negativos!"
  | x == 0 = 1
  | otherwise = x * fatorialFix (x-1)

--Q2

somar :: Int -> Int
somar 0 = 0
somar x = x + somar (x-1)

--Q3
pow :: (Num a, Eq a) => a -> a -> a
pow m n
  | n == 0 = 1
  | otherwise = m * pow m (n -1)

--Q4

