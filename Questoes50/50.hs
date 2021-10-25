module Questoes50 where

import Data.Char

{-
1. Apresente uma definição recursiva da função (pré-definida) enumFromTo :: Int -> Int -> [Int]
que constrói a lista dos números inteiros compreendidos entre dois limites.
Por exemplo, enumFromTo 1 5 corresponde à lista [1,2,3,4,5]
-}

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end |(start > end)  = []
                     |(start == end) = [start]
                     |otherwise      = start: enumFromTo' (start+1) end
