module ProvaUm
  (
  funcionarios,
  prefixos,
  fatores,
  primo,
  primos,
  tailCondicional,
  tailGuardas,
  )
where


--Q1
funcionarios :: [(String, [String])] -> [String]
funcionarios = foldr (\(x, y) n ->
    if length y >= 2
        then x : n
    else n
    ) []

--Q2
prefixos :: [a] -> [[a]]
prefixos = foldr (\x n -> [x] : concatMap (\prefixo -> [x:prefixo]) n) []

--Q3
fatores :: Integral a => a -> [a]
fatores n = [i | i<-[1..n], n `mod` i == 0]

primo :: Integral a => a -> Bool
primo n 
    | n < 1 = False
    | otherwise = fatores n == [1, n]

primos :: Integral a => a -> a -> [a]
primos x y = filter primo [x..y]

--Q4
tailCondicional :: [a] -> [a]
tailCondicional xs = if null xs 
                        then error "Lista vazia" 
                    else 
                        drop 1 xs

tailGuardas :: [a] -> [a]
tailGuardas xs
    | null xs = error "Lista vazia"
    | otherwise = drop 1 xs


--Q5
