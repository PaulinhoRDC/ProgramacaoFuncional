module Ficha5 where

import  Data.Char

{-
1. Apresente definições das seguintes funções de ordem superior, já pré-definidas no Prelude
ou no Data.List:
-}

--  (a) any :: (a -> Bool) -> [a] -> Bool que testa se um predicado é verdade para
-- algum elemento de uma lista; por exemplo:
-- any odd [1..10] == True

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = if (p x) then True
                        else any' p xs

-- OU

any2 p l = elem True (map p l)

-- OU

any3 p l = or (map p l)

{-
(e) span :: (a -> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os
dois resultados anteriores. Note que apesar de poder ser definida à custa das
outras duas, usando a definição
                  span p l = (takeWhile p l, dropWhile p l)
nessa definição há trabalho redundante que pode ser evitado.
Apresente uma definição alternativa onde não haja duplicação de trabalho.
-}

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p [] = ([] , [])
span' p (x:xs)
      | (p x)     = (x:l1, l2)
      | otherwise = ([], x:xs)
    where (l1,l2) = span' p xs

{-

2. Relembre a questão sobre polinómios introduzida na Ficha 3, onde um polinómio era
representado por uma lista de monómios representados por pares (coeficiente, expoente)
              type Polinomio = [Monomio]
              type Monomio = (Float,Int)
Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinómio 2 x^3 + 3 x 4 + 5 x^3 + 4 x^5
Redefina as funções pedidas nessa ficha, usando agora funções de ordem
superior (definidas no Prelude ou no Data.List) em vez de recursividade explícita:
-}

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- (a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os monómios com um dado grau de um polinómio.

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l = filter aux l
    where aux :: Monomio -> Bool
          aux (c,e) = e==n

-- OU

selgrau1 :: Int -> Polinomio -> Polinomio
selgrau1 n l = filter (\(c,e) -> e==n) l

-- (b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quantos monómios de grau n existem em p.

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((c,e): t) = if (e==n) then 1+ conta n t
                               else conta n t

-- OU

conta1 n l = length (filter (\(c,e) -> e==n) l )

--OU

conta2 n l = foldr (\(c,e) r -> if e==n then 1+r else r) 0 l

--OU

conta3 n l = foldr aux 0 l
    where aux :: Monomio -> Int -> Int
          aux (c,e) r = if (e==n) then 1+r
                                  else r

-- (c) grau :: Polinomio -> Int que indica o grau de um polinómio.

grau :: Polinomio -> Int
grau [] = 0
grau ((c,e): t) = max e (grau t)

-- OU

grau1 l = foldr (\(c,e) r -> max e r) 0 l

-- (d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polinómio.

deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c* (fromIntegral e), e-1)) p

-- (e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polinómio para uma dado valor de x.

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((c,e): t) = c * x^e + calcula x t

-- OU

calcula1 x p = foldr (\(c,e) r -> (c* x^e) + r) 0 p

{-

3. Considere a sequinte definição para representar matrizes:
              type Mat a = [[a]]
Por exemplo, a matriz (triangular superior)

1 2
3 4
5 6     ,seria representada por:     [[1,2,3], [0,4,5], [0,0,6]]

Defina as seguintes funcões sobre matrizes (use, sempre que achar apropriado, funções
de ordem superior).
-}

type Mat a = [[a]]

-- (a) dimOK :: Mat a -> Bool que testa se uma matriz está bem construída (i.e., se todas as linhas têm a mesma dimensão).

dimOK :: Mat a -> Bool
dimOK (l:ls) = let n = length l
               in all (\a -> length a==n) ls

{-
(d) transpose :: Mat a -> Mat a que calcula a transposta de uma matriz.

1 2
3 4
5 6   , tem como transposta: 1 3 5
                             2 4 6
-}

transpose :: Mat a -> Mat a
transpose ([]: _) = []
transpose m = (map head m) : transpose (map tail m)
