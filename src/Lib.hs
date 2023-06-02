module Lib
  ( someFunc,
    converteFC,
    converteCF,
    par,
    par2,

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