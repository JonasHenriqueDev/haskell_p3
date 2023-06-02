module Lib
  ( someFunc,
    converteFC,
    converteCF,
    par,
    par2,
    menor,
    menor3,
    fatorial,
    fib,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Forneça uma temperatura em graus Fahrenheit a partir de uma temperatura em graus Celsius. --

-- F para C --
converteFC :: Float -> Float
converteFC f = (f - 32) / 1.8

-- C para F --
converteCF :: Float -> Float
converteCF c = c * 1.8 + 32

-- Faça uma função que recebe um numero e retorna verdadeiro se o numero for par. --
par :: Int -> Bool
par = even -- jeito mais fácil e curto --

par2:: Int -> Bool
par2 x = if mod x 2 == 0 then True else False -- jeito mais explicito --

-- Faça uma função que recebe dois valores e retorna o menor. --
menor :: Int -> Int -> Int
menor x y = if x <= y then x else y

-- Faça uma função que recebe três valores e retorna o menor. --
menor3 :: Int -> Int -> Int -> Int
menor3 x y z = if menor x y <= z then menor x y else z

-- Escreva uma função recursiva para calcular o fatorial de um numero natural. --
fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

-- Fibonacci --
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)