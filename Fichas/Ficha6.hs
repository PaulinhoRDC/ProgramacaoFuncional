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

-- (a) altura :: BTree a -> Int que calcula a altura da árvore.

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ arv1 arv2) = max (1 + altura arv1) (1 + altura arv2)

-- (b) contaNodos :: BTree a -> Int que calcula o número de nodos da árvore.

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ arv1 arv2) = 1 + contaNodos arv1 + contaNodos arv2

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

-- (f) mirror :: BTree a -> BTree a ,que dá a árvore simétrica.

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

-- (g) zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c ,que generaliza a função zipWith para árvores binárias.

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r e d) (Node r2 e2 d2) = Node (f r r2) (zipWithBT f e e2) (zipWithBT f d d2)
zipWithBT _ _ _ = Empty

-- (h) unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
--,que generaliza a função unzip (neste caso de triplos) para árvores binárias.

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1, Node b unzipL2 unzipR2, Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

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

-- (d) Defina uma função remove :: Ord a => a -> BTree a -> BTree a
--,que remove um elemento de uma árvore binária de procura, usando a função anterior.

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r)
    | x < e = Node e (remove x l) r
    | x > e = Node e l (remove x r)
    | otherwise = case l of Empty -> r
                            _ -> case r of Empty -> l
                                           _ -> Node g l h
    where (g,h) = minSmin r                                     --VERIFICAR MÉTODO USADO AULA

{-
 Nesta função, depois de remover o elemento, temos de formar uma nova árvore, pois não podemos ter um nodo vazio.
 Para isso, removemos o menor elemento do ramo da direita e colocamos esse elemento onde estava o elemento removido.
 Desta forma, a árvore mantém a sua ordem, já que todos os elementos à esquerda continuam a ser mais pequenos
e todos os elementos à direita continuam a ser maiores do que o elemento no nodo.
-}

{-
3) Considere agora que guardamos a informação sobre uma turma de alunos na seguinte estrutura de dados:

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno          -- árvore binária de procura (ordenada por número)

Turma de exemplo para testar as funções:

turma :: Turma
turma = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))

Defina as seguintes funções:
-}

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno

turma :: Turma
turma =(Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty))

-- (a) inscNum :: Numero -> Turma -> Bool que verifica se um aluno, com um dado número, está inscrito.

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) l r) = n == num || inscNum n (if n < num then l else r)

-- (b)inscNome :: Nome -> Turma -> Bool que verifica se um aluno, com um dado nome, está inscrito.

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) l r) = n == nom || inscNome n l || inscNome n r

-- (c) trabEst :: Turma -> [(Numero,Nome)] que lista o número e nome dos alunos trabalhadores-estudantes (ordenados por número).

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,_) l r) = (case reg of
                                           TE -> [(num,nom)]
                                           otherwise -> []
                                     ) ++ trabEst l ++ trabEst r

-- (d) nota :: Numero -> Turma -> Maybe Classificacao
--,que calcula a classificação de um aluno (se o aluno não estiver inscrito a função deve retornar Nothing).

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) l r) | n == num = Just clas
                                 | n < num = nota n l
                                 | otherwise = nota n r
nota _ _ = Nothing

-- (e) percFaltas :: Turma -> Float ,que calcula a percentagem de alunos que faltaram à avaliação.

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = (sumFaltas turma / numAlunos turma) * 100
    where sumFaltas Empty = 0
          sumFaltas (Node (_,_,_,clas) l r) = (case clas of
                                                      Faltou -> 1
                                                      otherwise -> 0
                                              ) + sumFaltas l + sumFaltas r

          numAlunos Empty = 0
          numAlunos (Node e l r) = 1 + numAlunos l + numAlunos r

-- (f) mediaAprov :: Turma -> Float ,que calcula a média das notas dos alunos que passaram.

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = (sumNotas turma / numNotas turma)
    where sumNotas :: Turma -> Float
          sumNotas Empty = 0
          sumNotas (Node (_,_,_,Aprov nota) l r) = fromIntegral nota + sumNotas l + sumNotas r
          sumNotas (Node e l r) = sumNotas l + sumNotas r

          numNotas :: Turma -> Float
          numNotas (Node (_,_,_,clas) l r) = (case clas of
                                                    Aprov nota -> 1
                                                    otherwise -> 0
                                             ) + numNotas l + numNotas r
          numNotas _ = 0

-- (g) aprovAv :: Turma -> Float ,que calcula o rácio de alunos aprovados por avaliados.
-- Implemente esta função fazendo apenas uma travessia da árvore.

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = (a / b)
    where (a,b) = aux turma
          aux Empty = (0,0)
          aux (Node (_,_,_,clas) l r) = case clas of
                                                Aprov nota -> (x+1,y)
                                                Rep -> (x,y+1)
                                                otherwise -> (x,y)
            where (x,y) = (c+e,d+f)
                  (c,d) = aux l
                  (e,f) = aux r
