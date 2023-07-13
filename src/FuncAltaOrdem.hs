module FuncAltaOrdem
  (
    impares,
  posicao,
  replicar,
  fibonacci)
where
import Data.List (sort)


--Q1
impares :: Integral a => [a] -> [a]
impares l = sort $ filter odd l

--Q2
posicao :: Int -> [a] -> a
posicao x l = head $ drop x l

--Q3

replicar :: a -> Int -> [a]
replicar x n = take n (repeat x)


--Q4

--Q5

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)