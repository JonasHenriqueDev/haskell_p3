module Testes
  (
    pares,
    ordenada,

  )
where

pares :: [a] -> [(a, a)]
pares xs = zip xs (tail xs)

ordenada :: Ord a => [a] -> Bool
ordenada xs = and [x <= y | (x, y) <- pares xs]
