module Teste1819 where

import Data.Char

import System.Random
import System.IO
import System.IO.Error

{-
1. Apresente uma definição recursiva das seguintes funções (pré-definidas) sobre listas:

(a) elemIndices :: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um elemento
ocorre numa lista. Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6].

(b) isSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.
Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.
-}

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x [] = []
elemIndices x l = posIndice 0 x l

posIndice :: Eq a => Int -> a -> [a] -> [Int]
posIndice i x [] = []
posIndice i x (h:t) |(x==h) = i : posIndice (i+1) x t
                    |otherwise = posIndice (i+1) x t

-- ----------------

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf (x:xs) (y:ys) = if x==y then isSubsequenceOf xs ys
                                        else isSubsequenceOf (x:xs) ys
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False


{-
2. Considere o seguinte tipo para representar árvores binárias.
                      data BTree a = Empty | Node a (BTree a) (BTree a)

(a)Defina a função  lookupAP :: Ord a => a-> BTree (a,b)-> Maybe b ,que generaliza a função
lookup para árvores binárias de procura.

(b) Defina a função zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c que generaliza a função zipWith para árvores binárias.
-}

data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAp x Empty = Nothing
lookupAP x (Node (a,b) esq dir) |(x==a) = Just b
                                |(x<a) = lookupAP x esq
                                |otherwise = lookupAP x dir


-- ----------------

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a esq dir) (Node a2 esq2 dir2) = Node (f a a2) (zipWithBT f esq esq2) (zipWithBT f dir dir2)
zipWithBT f _ _ = Empty

{-
3. Defina a função digitAlpha :: String -> (String,String), que dada uma string,
devolve um par de strings: uma apenas com os números presentes nessa string, e a outra apenas com as letras presentes na string.
Implemente a função de modo a fazer uma única travessia da string.
Sugestão: pode usar as funções isDigit, isAlpha :: Char -> Bool.
-}

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | (isDigit x) = (x:num , letr)
                  | (isAlpha x) = (num , x:letr)
                  | otherwise = (num , letr)

            where (num,letr) = digitAlpha xs


digitAlpha' :: String -> (String,String)
digitAlpha' l = foldr (\x (ds,as) -> if isDigit x then (x:ds,as) else if isAlpha x then (ds,x:as) else (ds,as)) ([],[]) l

digitAlpha'' :: String -> (String,String)
digitAlpha'' l = foldr (\x acc -> if isDigit x then (x:(fst(acc)),(snd(acc))) else if isAlpha x then ((fst(acc)),x:(snd(acc))) else ((fst(acc)),(snd(acc)))) ([],[]) l

{-
4. Considere o seguinte tipo de dados para representar uma sequência em que os elementos podem ser acrecentados à esquerda (Cons)
ou por concatenação de duas sequências (App).
                                                data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

(a) Defina a função firstSeq :: Seq a -> a ,que recebe uma sequência não vazia e devolve o seu primeiro elemento.

(b) Defina a função dropSeq :: Int -> Seq a -> Seq a, tal que dropSeq n s elimina os n primeiros elementos da sequência s.
 A função deve manter a estrutura da sequência.
 Por exemplo: dropSeq 2 (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil)) == App (Cons 3 Nil) (Cons 1 Nil)

(c) Declare (Seq a) como instância da classe Show de forma a obter o seguinte comportamento no interpretador:
       > App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil))
       <<1,7,5,3>>
-}

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a
firstSeq (Cons x s) = x
firstSeq (App (Nil) s) = firstSeq s
firstSeq (App s _) = firstSeq s

-- ----------------

dropSeq :: Int -> Seq a -> Seq a
dropSeq _ (Nil) = Nil
dropSeq n (Cons a s) = dropSeq (n - 1) s
dropSeq n (App s1 s2) | n > nx = dropSeq (n-nx) s2
                      | n == nx = s2
                      | otherwise = (App (dropSeq n s1) s2)
     where nx = contaCons s1

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ s) = 1 + contaCons s
contaCons (App s1 s2) = contaCons s1 + contaCons s2

-- ----------------  App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil))   ->   <<1,7,5,3>>

instance Show a => Show (Seq a) where
  show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons x Nil) = show x
mostra (Cons x s)   = show x ++ "," ++ mostra s
mostra (App s1 s2)  = mostra s1 ++ "," ++ mostra s2


{-
5. Considere a sequinte definição para representar matrizes: type Mat a = [[a]]

Por exemplo, a matriz 6 7 2
                      1 5 9
                      8 3 4  , seria representada por [[6,7,2], [1,5,9], [8,3,4]].

(a) Defina a função getElem :: Mat a -> IO a, que selecciona aleatoriamente um elemento da matriz.
Sugestão: use a função randomRIO :: Random a => (a,a) -> IO a.

(b) Um quadrado mágico é uma matriz quadrada de inteiros em que a soma de qualquer linha, coluna e das duas diagonais é uma constante.
O exemplo acima é um quadrado mágico com constante 15.
Defina a função magic :: Mat Int -> Bool que verifica se uma matriz quadrada é um quadradro mágico.
Será valorizada a utilização de funções de ordem superior.
-}

type Mat a = [[a]]

m1 :: Mat Int
m1 = [[6,7,2], [1,5,9], [8,3,4]]

-- --------a)

getElem :: Mat a -> IO a
getElem mat = do let (linhas,colunas) = (length mat, length (head mat))
                 randLine <- randomRIO (0,linhas - 1)
                 randRow <- randomRIO (0,colunas - 1)
                 return ( (mat !! randLine) !! randRow )         -- return $ (mat !! randLine) !! randRow


-- --------b)

magic :: Mat Int -> Bool
magic m = let somaLinhas = map sum m
              somaColunas = map sum (transpose m)
              somaDiagonal1 = sum (diagonal1 m)
              somaDiagonal2 = sum (diagonal2 m)
          in length(nub' ([somaDiagonal2,somaDiagonal1] ++ somaLinhas ++ somaColunas)) == 1

diagonal1 :: Mat Int -> [Int]
diagonal1 m = diaA 0 m
    where diaA i [] = []
          diaA i (h:t) = (h!!i): diaA (i+1) t

-- diagonal1 m = zipWith (!!) m [0..]

diagonal2 :: Mat Int -> [Int]
diagonal2 m = diaA ((length m)-1) m
    where diaA i [] = []
          diaA i (h:t) = (h!!i): diaA (i-1) t

-- diagonal2 m = diagonal1 (reverse m)

nub' :: Eq a => [a] -> [a]    -- (mantém última ocorrência)
nub' [] = []
nub' (h:t) = if (elem h t)
  then nub' t
  else h: nub' t

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)
