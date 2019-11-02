--1 

import Data.Sequence

-- primeira versão
separa :: [Int] -> [Int]
separa (p:s:r) = (s:r)


-- Esta função está errada
--separab :: [Int] -> [Int]
--separab (p:s:r) = (r:s:p)


separac :: [Int] -> [Int]
separac (p:r) = r


-- 2

-- a 
comprimento::[Int]->Int
comprimento [] = 0
comprimento(x:xs) = 1 + comprimento xs

-- b
somatorio::[Int]->Int
somatorio [] = 0
somatorio(a:b) = a + somatorio b


-- c
somatorio_impares::[Int]->Int
somatorio_impares [] = 0
somatorio_impares(a:b) 
 |mod a 2 /= 0 = a + somatorio_impares b
 |otherwise = somatorio_impares b


-- d
soma_quadrados::[Int]->Int
soma_quadrados [] = 0
soma_quadrados(a:b) = a ^ 2 + soma_quadrados b


-- e 
soma_mult_3::[Int]->Int
soma_mult_3 [] = 0
soma_mult_3(a:b)
 |mod a 3 == 0 = a + soma_mult_3 b
 |otherwise = soma_mult_3 b


-- f
produtorio::[Int]->Int
produtorio [] = 1
produtorio(x:xs) = x * produtorio xs


-- g
n_esimo::Int->[Int]->Int
n_esimo 0 (x:xs) = x
n_esimo n xs = xs !! n 


-- h
ultimo::[Int]->Int
ultimo [x] = x
ultimo(x:xs) = ultimo xs


-- i
duplica::[Int]->[Int]
duplica [] = []
duplica(a:b) = [a] ++ [a] ++ duplica b



-- j
reverso::[Int]->[Int]
reverso[] = []
reverso(a:b) = reverso b ++ [a]


-- k
substituir_no_indice::Int->Int->[Int]->[Int]
substituir_no_indice _ _ [] = []
substituir_no_indice a b (x:xs)
 |a == 0 = b:xs
 |otherwise = x : substituir_no_indice(a-1) b xs



