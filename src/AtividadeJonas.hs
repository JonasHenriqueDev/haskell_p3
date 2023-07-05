module AtividadeJonas
  ( mapaRecursivo,
    mapaCompreensao,
    cortaString,
    split,
  )
where

-- Q2
mapaRecursivo :: (a1 -> a2) -> [a1] -> [a2]
mapaRecursivo _ [] = []
mapaRecursivo funcao (x : xs) = funcao x : mapaRecursivo funcao xs

{-
input: mapaRecursivo (++ "-Pernambuco") ["Universidade", "Garanhuns", "Engenharia de Software"]
output: ["Universidade-Pernambuco","Garanhuns-Pernambuco","Engenharia de Software-Pernambuco"]
-}
mapaCompreensao :: (t -> a) -> [t] -> [a]
mapaCompreensao _ [] = []
mapaCompreensao funcao (x : xs) = [funcao x | x <- xs]

{-
input: mapaCompreensao (++ "-Pernambuco") ["Universidade", "Garanhuns", "Engenharia de Software"]
output: ["Universidade-Pernambuco","Garanhuns-Pernambuco","Engenharia de Software-Pernambuco"]
-}

-- Q3
cortaString :: String -> Int -> Int -> String
cortaString s i f = drop (i - 1) (take (f + 1) s)

-- Q4

split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split s ch = x : split (drop 1 y) ch
  where
    (x, y) = span (/= ch) s
