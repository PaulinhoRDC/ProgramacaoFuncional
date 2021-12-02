module Ficha4 where

import  Data.Char

{-

1. Defina a função digitAlpha :: String -> (String,String), que dada uma string,
devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
apenas com os números presentes na string. Implemente a função de modo a fazer uma
única travessia da string. Relembre que as funções isDigit, isAlpha :: Char -> Bool
estão já definidas no módulo Data.Char.

-}

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs)
        | isAlpha x = (x:a, b)
        | isDigit x = (a, x:b)
        | otherwise = (a,b)
    where (a,b) = digitAlpha xs

{-

2. Defina a função nzp :: [Int] -> (Int,Int,Int) que, dada uma lista de inteiros,
conta o número de valores nagativos, o número de zeros e o número de valores positivos,
devolvendo um triplo com essa informação. Certifique-se que a função que definiu
percorre a lista apenas uma vez.

-}

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs)
      | x<0    = (1+a,b,c)
      | x == 0 = (a,1+b,c)
      | x>0    = (a,b,1+c)
      | otherwise = (a,b,c)
    where (a,b,c) = nzp xs

{-

3. Defina a função divMod :: Integral a => a -> a -> (a, a)
que calcula simultaneamente a divisão e o resto da divisão inteira por subtrações sucessivas.

-}

divMod' :: Integral a => a -> a -> (a, a)
divMod' x y
      | (y<=x)    = (1+q, r)
      | otherwise = (0, x)
    where (q,r) = divMod' (x-y) y

-- Sabendo que :

-- div x y | (x>=y) = 1 + div (x-y) y
--         | otherwise = 0

-- mod x y | (x>=y) = mod (x-y) y
--         | otherwise = x

{-

4. Utilizando uma função auxiliar com um acumulador, optimize seguinte definição recursiva
que determina qual o número que corresponde a uma lista de digitos.
                        fromDigits :: [Int] -> Int
                        fromDigits [] = 0
                        fromDigits (h:t) = h*10^(length t) + fromDigits t
Note que:
                fromDigits [1,2,3,4] = 1 × 103 + 2 × 102 + 3 × 101 + 4 × 100
                                     = 4 + 10 × (3 + 10 × (2 + 10 × (1 + 10 × 0)))

-}

fromDigitsOpt :: [Int] -> Int
fromDigitsOpt l = fromDigitsAC l 0

fromDigitsAC :: [Int] -> Int -> Int
fromDigitsAC [] ac = ac
fromDigitsAC (h:hs) ac = fromDigitsAC hs (h+10*ac)

{-

5. Utilizando uma função auxiliar com acumuladores, optimize seguinte definição que
determina a soma do segmento inicial de uma lista com soma máxima.
                    maxSumInit :: (Num a, Ord a) => [a] -> a
                    maxSumInit l = maximum [sum m | m <- inits l]

-}

maxSumInit :: (Num a, Ord a) => [a] -> a     -- maxSumInit [3,2,-7,1,10,-3,2] == 9
maxSumInit (x:xs) = maxSI xs x x

maxSI :: (Num a, Ord a) => [a] -> a -> a -> a
maxSI [] m s = m
maxSI (x:xs) m s
        | (m >= s+x) = maxSI xs m (s+x)
        | otherwise  = maxSI xs (s+x) (s+x)

{-

6. Optimize a seguinte definição recursiva da função que calcula o n-ésimo número da
sequência de Fibonacci, usando uma função auxiliar com 2 acumuladores que representam,
respectivamente, o n-ésimo e o n+1-ésimo números dessa sequência.
                          fib :: Int -> Int
                          fib 0 = 0
                          fib 1 = 1
                          fib n = fib (n-1) + fib (n-2)

-}

fib :: Integer -> Integer
fib n = fibAC n (0,1)

fibAC 0 (a,b) = a
fibAC 1 (a,b) = b
fibAC n (a,b) = fibAC (n-1) (b, a+b)
