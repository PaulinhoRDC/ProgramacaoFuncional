module Ficha7 where

import  Data.Char

-- OUTROS TIPOS DE ÁRVORES

{-

1. Considere o seguinte tipo para representar expressões inteiras.
              data ExpInt = Const Int
                          | Simetrico ExpInt
                          | Mais ExpInt ExpInt
                          | Menos ExpInt ExpInt
                          | Mult ExpInt ExpInt
Os termos deste tipo ExpInt podem ser vistos como árvores cujas folhas são inteiros e
cujos nodos (não folhas) são operadores.
-}

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- (a) Defina uma função calcula :: ExpInt -> Int que, dada uma destas expressões
-- calcula o seu valor.

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

-- (b) Defina uma função infixa :: ExpInt -> String ,de forma a que
-- infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dê como resultado "(3 + (2 - 5))".

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "( " ++ (infixa e1) ++ "+" ++ (infixa e2) ++ " )"
infixa (Menos e1 e2) = "( " ++ (infixa e1) ++ "-" ++ (infixa e2) ++ " )"
infixa (Mult e1 e2) = "( " ++ (infixa e1) ++ "*" ++ (infixa e2) ++ " )"

-- (c) Defina uma outra função de conversão para strings posfixa :: ExpInt -> String
-- de forma a que quando aplicada à expressão acima dê como resultado "3 2 5 - +".

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e) ++ " ~"
posfixa (Mais e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " +"
posfixa (Menos e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " -"
posfixa (Mult e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ " *"

{-

2. Considere o seguinte tipo para representar árvores irregulares (rose trees).
                  data RTree a = R a [RTree a]

                  Árvore de exemplo para testar as funções:
                  arvore = R 6 [R 4 [R 7 [R 1 [],
                                          R 3 []],
                                     R 9 []],
                                R 3 [R 12 []],
                                R 6 [],
                                R 11 []]

Defina as seguintes funções sobre estas árvores:
-}

data RTree a = R a [RTree a]

arvore = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

-- (a) soma :: Num a => RTree a -> a que soma os elementos da árvore.

soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)

-- (b) altura :: RTree a -> Int que calcula a altura da árvore.

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)

-- (c) prune :: Int -> RTree a -> RTree a ,que remove de uma árvore todos os elementos a partir de uma determinada profundidade.

prune :: Int -> RTree a -> RTree a
prune 1 (R x l) = (R x [])
prune n (R x l)
          |(n>1) = ( R x (map (prune (n-1)) l) )

-- (d) mirror :: RTree a -> RTree a ,que gera a árvore simétrica.

mirror :: RTree a -> RTree a
mirror (R x l) = R x (reverse (map mirror l))

-- (e) postorder :: RTree a -> [a] ,que corresponde à travessia postorder da árvore.

postorder :: RTree a -> [a]
postorder (R x l) = concat (map postorder l) ++ [x]

{-

3. Relembre a definição de árvores binárias apresentada na ficha anterior:

                data BTree a = Empty | Node a (BTree a) (BTree a)

Nestas árvores a informação está nos nodos (as extermidades da árvore têm apenas
uma marca – Empty). E também habitual definirem-se árvores em que a informação
está apenas nas extermidades (leaf trees):

                data LTree a = Tip a | Fork (LTree a) (LTree a)

Defina sobre este tipo as seguintes funções:
-}

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)


-- (a) ltSum :: Num a => LTree a -> a ,que soma as folhas de uma árvore.

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a1 a2) = (ltSum a1) + (ltSum a2)

-- (b) listaLT :: LTree a -> [a] ,que lista as folhas de uma árvore (da esquerda para a direita).

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork a b) = listaLT a ++ listaLT b

-- (c) ltHeight :: LTree a -> Int ,que calcula a altura de uma árvore.

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a1 a2) = 1 + max (ltHeight a1)(ltHeight a2)

{-
4. Estes dois conceitos podem ser agrupados num só, definindo o seguinte tipo:

            data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

São as chamadas full trees onde a informação está não só nos nodos, como também nas
folhas (note que o tipo da informação nos nodos e nas folhas não tem que ser o mesmo).
-}

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


-- (a) Defina a função splitFTree :: FTree a b -> (BTree a, LTree b) ,que separa
-- uma árvore com informação nos nodos e nas folhas em duas árvores de tipos
-- diferentes.

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x e d) = (Node x e1 d1, Fork e2 d2)
        where (e1,e2) = splitFTree e
              (d1,d2) = splitFTree d

-- (b) Defina ainda a função joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
-- que sempre que as árvores sejam compatíveis as junta numa só.

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees _ _ = Nothing
