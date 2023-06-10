module ListaUm
    (   
        media3,
        questao1,
        questao2,
    )
where

media3 :: Int -> Int -> Int
media3 a b c = (a + b + c) / 3

questao1 :: Eq a => a -> a -> a -> Int
questao1 a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | a /= b && b /= c = 0

questao2 :: 