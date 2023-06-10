{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module ListaUm
    (
        quantosIguais,
        mediaMaiores,
        potencia_2,
        potencia_4,
        ouExclusivo,
        x_maior,
        x_menor,

    )
where

--Q1
quantosIguais :: Eq a => a -> a -> a -> Int
quantosIguais a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | a /= b && b /= c = 0

--Q2
mediaMaiores :: (Ord a, Fractional a) => a -> a -> a -> Int
mediaMaiores a b c =
    let media = (a + b + c)/3
        maiores = length (filter (>media) [a, b, c]) in maiores

--Q3
potencia_2 :: Int -> Int
potencia_2 x = x * x

--Q4
potencia_4 :: Int -> Int
potencia_4 x = potencia_2 (potencia_2 x)

--Q5
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo False False = False
ouExclusivo False True = True
ouExclusivo True False = True
ouExclusivo True True = False

--Q6
x_maior :: Float -> Float -> Float -> Float
x_maior a b c = (-b + sqrt (b * b  - 4 * a * c)) / (2 * a)

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = (-b - sqrt (b * b  - 4 * a * c)) / (2 * a)

--Q7