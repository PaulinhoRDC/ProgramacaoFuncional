module Ficha6 where

import  Data.Char

{-
1. Considere o seguinte tipo para representar árvores binárias.
           data BTree a = Empty
                        | Node a (BTree a) (BTree a)
                    deriving Show
Defina as seguintes funções:
-}

data BTree a = Empty
             | Node a (BTree a) (BTree a)
         deriving Show

-- (c) folhas :: BTree a -> Int, que calcula o número de folhas (i.e., nodos sem descendentes) da árvore.

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d) = (folhas e) + (folhas d)

-- (d) prune :: Int -> BTree a -> BTree a, que remove de uma árvore todos os elementos a partir de uma determinada profundidade.

prune :: Int -> BTree a -> BTree a
prune 0 t = Empty
prune x (Node r e d) = if (x>0) then Node r (prune (x-1) e) (prune (x-1) d)
                                else error "profundidade inválida"
prune x Empty = Empty

-- (e) path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde a esquerda e True a direita)
-- e uma árvore, dá a lista com a informação dos nodos por onde esse caminho passa.

path :: [Bool] -> BTree a -> [a]
path [] (Node r e d) = [r]
path (x:xs) Empty = []
path (x:xs) (Node r e d) | (x==True) = r: path xs d
                         | otherwise = r: path xs e

-- (f)

-- (g)

-- (h)


{-
2. Defina as seguintes funções, assumindo agora que as árvores são binárias de procura:
-}

--(a) Defina uma função minimo :: Ord a => BTree a -> a que determina o menor elemento de uma árvore binária de procura não vazia

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _ ) = r
minimo (Node r e _) = minimo e

-- (b) Defina uma função semMinimo :: Ord a => BTree a -> BTree a que remove o menor elemento de uma árvore bin´aria de procura não vazia.

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = (Node r (semMinimo e) d)


-- (c) Defina uma função minSmin :: Ord a => BTree a -> (a,BTree a) que calcula,
-- com uma única travessia da árvore o resultado das duas funções anteriores.

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = let (m, e') = minSmin e
                       in (m, Node r e' d)

-- (d) Defina uma função remove :: Ord a => a -> BTree a -> BTree a que remove
-- um elemento de uma árvore binária de procura, usando a função anterior.
