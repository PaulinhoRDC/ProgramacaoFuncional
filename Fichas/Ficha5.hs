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
b) zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de duas listas usando uma função específica;
 por exemplo: zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].
-}

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []                                -- caso em que não se deve fazer a função (podia fazer separados casos)

{-
c) takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos da lista que satisfazem um dado predicado;
 por exemplo: takeWhile odd [1,3,4,5,6,6] == [1,3].
-}

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f (x:xs) | (f x) = x : takeWhile' f xs
                    | otherwise = []

{-
d) dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da lista que satisfazem um dado predicado;
por exemplo: dropWhile odd [1,3,4,5,6,6] == [4,5,6,6].
-}

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f (x:xs) | (f x) = dropWhile' f xs
                    | otherwise = xs

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
f) deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
que apaga o primeiro elemento de uma lista que é “igual” a um dado elemento de acordo com a função de comparação
que é passada como parâmetro.
 Por exemplo: deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)].
-}

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x (h:t) | f x h = t
                   | otherwise = h: deleteBy' f x t

{-
g) sortOn :: Ord b => (a -> b) -> [a] -> [a]
que ordena uma lista comparando os resultados de aplicar uma função de extração de uma chave a cada elemento de uma lista.
 Por exemplo: sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].
-}

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = insere h (sortOn' f t)
      where insere x [] = [x]
            insere x (a:b) = if f x > f a then a: insere x b
                                          else x:a:b

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

-- (f) simp :: Polinomio -> Polinomio que retira de um polinómio os monómios de coeficiente zero.

simp :: Polinomio -> Polinomio
simp l = filter (\(c,e) -> c /= 0) l            -- Poderia fazer a simplificação de retirar os l's

-- (g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplicação de um monómio por um polinómio.

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) l = map (\(b,e) -> (b*x,y+e)) l

-- (h) ordena :: Polinomio -> Polinomio que ordena um polonómio por ordem crescente dos graus dos seus monómios.

ordena :: Polinomio -> Polinomio
ordena l = sortOn' snd l

-- (i) normaliza :: Polinomio -> Polinomio
--que dado um polinómio constrói um polinómio equivalente em que não podem aparecer varios monómios com o mesmo grau.

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):xs) = (sum [bs | (bs,es) <- selgrau e xs] + b,e): normaliza [(bo,eo) | (bo,eo) <- xs, eo /= e]

-- j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polinómios,
--de forma que, se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado.

soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r

-- k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de dois polinómios.

produto :: Polinomio -> Polinomio -> Polinomio
produto' ((c,e): t) ((c2,e2): t2) = (c*c2, e+e2): produto t t2
produto' l [] = l
produto' [] l = l

--OU
produto p1 p2 = foldl (\ac x -> soma (mult x p2) ac) [] p1

-- (l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polinómios são equivalentes.

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)


{-
3. Considere a sequinte definição para representar matrizes:
              type Mat a = [[a]]
Por exemplo, a matriz (triangular superior)

1 2 3
0 4 5
0 0 6        ,seria representada por:     [[1,2,3], [0,4,5], [0,0,6]]

Defina as seguintes funcões sobre matrizes (use, sempre que achar apropriado, funções
de ordem superior).
-}

type Mat a = [[a]]

-- (a) dimOK :: Mat a -> Bool que testa se uma matriz está bem construída (i.e., se todas as linhas têm a mesma dimensão).

dimOK :: Mat a -> Bool
dimOK (l:ls) = let n = length l
               in all (\a -> length a==n) ls

--OU
dimOK' (h:t) = all (\x -> length h == length x) t

-- (b) dimMat :: Mat a -> (Int,Int) que calcula a dimensão de uma matriz.

dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, length (head m))     -- número de linahs, número colunas

--(c) addMat :: Num a => Mat a -> Mat a -> Mat a ,que adiciona duas matrizes.

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (zipWith (+)) m1 m2

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

-- (e) multMat :: Num a => Mat a -> Mat a -> Mat a ,que calcula o produto de duas matrizes.

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [ [ sum (zipWith (*) (m1 !! j) [ x !! i | x <- m2 ]) | i <- [0..c-1] ] | j <- [0..l-1] ]
    where (l,_) = dimMat m1
          (_,c) = dimMat m2

-- f) zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
--,que, à semelhança do que acontece com a função zipWith, combina duas matrizes.
-- Use essa função para definir uma função que adiciona duas matrizes.

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat m1 m2 = (zipWith . zipWith) m1 m2

addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' m1 m2 = zipWMat (+) m1 m2

-- (g) triSup :: Num a => Mat a -> Bool
--que testa se uma matriz quadrada é triangular superior (i.e., todos os elementos abaixo da diagonal são nulos).

triSup :: Real a => Mat a -> Bool
triSup = snd . foldl (\(ac1,ac2) line -> (ac1+1, all (== 0) (take ac1 line) && ac2)) (0,True)

-- (h) rotateLeft :: Mat a -> Mat a ,que roda uma matriz 90º para a esquerda.
--Por exemplo, o resultado de rodar a matriz acima apresentada deve corresponder à matriz:
      {-
                                                                  | 3 5 6 |
                                                                  | 2 4 0 |
                                                                  | 1 0 0 |
      -}

rotateLeft :: Mat a -> Mat a
rotateLeft m = [ [ map (!! i) m !! j | j <- [0..l-1] ] | i <- [c-1,c-2..0]]
    where (l,c) = dimMat m
