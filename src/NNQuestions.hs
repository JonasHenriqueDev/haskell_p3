module NNQuestions
  (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    pares,
    compress,
    pack
  )
where

--Q1
myLast :: [a] -> a
myLast [] = error "Lista vazia"
myLast [x] = x
myLast x = x !! (length x - 1)

--Q2
myButLast :: [a] -> a
myButLast [] = error "Lista vazia"
myButLast [x] = x
myButLast x = x !! (length x - 2)

--Q3
elementAt :: [a] -> Int -> a
elementAt [] y = error "Lista vazia"
elementAt x y = x !! (y - 1)

--Q4
myLength :: [a] -> Int
myLength = length

--Q5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--Q6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = error "Lista vazia"
isPalindrome x = x == myReverse x

--Q7

--Q8
pares :: [a] -> [(a, a)]
pares a = zip (init a) (tail a)

compress :: Eq b => [b] -> [b]
compress a = [fst a | a <- pares a, uncurry (/=) a] ++ [last a]

--Q9

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)