module Recursividade
  (
    inserir,
    ordenar,
    fatorialFix,
    somar,
    pow,
    euclides,
    e,
    concatenar,
    replicar,
    selecionar,
    existe,
    merge
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

euclides :: Int -> Int -> Int
euclides x y
  | x < 0 || y < 0 = error "Permitido apenas números interiros não-negativos!"
  | x == y = x
  | x < y = euclides (y - x) x
  | otherwise = euclides (x - y) y

--Q5
--a)

e :: [Bool] -> Bool
e [] = False
e [x, y] = x == y
e (x:xs) = x == head xs && e xs

--b)

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

--c)

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n x = x : replicar (n-1) x

--d)

selecionar :: [a] -> Int -> a
selecionar [] _ = error "Lista vazia."
selecionar x n = head $ drop n x

--e)

existe :: Eq a => a -> [a] -> Bool
existe _ [] = False
existe x (y:ys)
  | x == y = True
  | otherwise = existe x ys

--Q6

merge :: Ord a => [a] -> [a] -> [a]
merge x y = ordenar(x ++ y)