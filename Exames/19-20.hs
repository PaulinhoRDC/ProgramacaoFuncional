module Exame1920 where

import  Data.Char
import System.Random
import System.IO
import System.IO.Error

{-
1. Apresente uma definição recursiva das seguintes funções (pré-definidas) sobre listas:

(a) inits :: [a] -> [[a]] que calcula a lista dos prefixos de uma lista.
Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].

(b) isPrefixOf:: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra.
Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf [10,30] [10,20,30] corresponde a False.
-}

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

isPrefixOf:: Eq a => [a] -> [a] -> Bool
isPrefixOf (x:xs) (y:ys) = (x==y) && isPrefixOf xs ys
isPrefixOf [] _ = True
isPrefixOf _ [] = False

{-
2. Considere o seguinte tipo para representar árvores binárias.

                                      data BTree a = Empty | Node a (BTree a) (BTree a)
                                               deriving Show

 (a) Defina a função folhas :: BTree a -> Int, que calcula o número de folhas (i.e., nodos sem descendentes) da árvore.

 (b) Defina a função path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde a esquerda e True a direita)
 e uma árvore, dá a lista com a informação dos nodos por onde esse caminho passa.
-}

data BTree a = Empty | Node a (BTree a) (BTree a)
         deriving Show

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a esq dir) = (folhas esq) + (folhas dir)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = [a]
path (x:xs) (Node a esq dir) = if x
  then a: path xs dir
  else a: path xs esq

{-
3. Uma representação possével de polimómios é pela sequência dos coeficientes - têm que se armazenar também os coeficientes nulos
pois será a posição do coeficiente na lista que dará o grau do monómio.

        type Polinomio = [Coeficiente]
        type Coeficiente = Float

   A representação do polinómio 2 x^5 − 5 x^3 será então [0,0,0,-5,0,2], que corresponde ao polinómio
0x^0 + 0x^1 + 0x^2 − 5x^3 + 0x^4 + 2x^5. Nas questões que se seguem, use sempre que possível, funções de ordem superior.

(a) Defina a operação valor :: Polinomio -> Float -> Float ,que calcula o valor do polinómio para um dado x.

(b) Defina a operação deriv :: Polinomio -> Polinomio que calcula a derivada de um polinómio

(c) Defina a operação soma :: Polinomio -> Polinomio -> Polinomio de adição de polinómios.
-}

type Polinomio = [Coeficiente]
type Coeficiente = Float


-- ---------------- a)

valor :: Polinomio -> Float -> Float
valor [] _ = 0
valor (h:t) x = contaGrau (h:t) x 0

contaGrau :: Polinomio -> Float -> Int -> Float
contaGrau [] _ _ = 0
contaGrau (h:t) x g = (h*(x^ fromIntegral (g))) + contaGrau t x (g+1)

-- OUUUUUUU

valor' :: Polinomio -> Float -> Float
valor' p x = sum (zipWith (\a b -> b * x ^ a) [0..] p)

-- ---------------- b)     [0,0,0,-5,0,2] , que é  2 x^5 − 5 x^3 , ficava , [0,0,-15,0,10]

deriv :: Polinomio -> Polinomio
deriv p = tail ( (zipWith (*) [0..] p) )

-- ---------------- c)    [0,0,0,-5,0,2] [0,0,0,-5,0,2] -> [0,0,0,-10,0,4] , que é  4 x^5 − 10 x^3

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = zipWith (+) p1 p2

soma' :: Polinomio -> Polinomio -> Polinomio
soma' [] [] = []
soma' p1 p2 = (head p1 + head p2) : (soma' (tail p1) (tail p2))

{-
4. Considere a seguinte definição para representar matrizes: type Mat a = [[a]].

ex = [[1,4,3,2,5], [6,7,8,9,0], [3,5,4,9,1]] representa a matriz abaixo desenhada.

(a) Defina a função quebraLinha :: [Int] -> [a] -> [[a]]
que recebe uma lista de inteiros s e uma linha l, e produz a lista de segmentos contíguos de l de comprimento indicado em s.
    Por exemplo, quebraLinha [2,3] [1,4,3,2,5] == [[1,4],[3,2,5]].

(b) Defina a função fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a] ,que recebe duas lista de inteiros
(com a partição das linhas e das colunas) e uma matriz, e produz a lista de (sub)-matrizes de acordo com essa partição.
    Por exemplo, fragmenta [2,1] [2,3] ex == [ [[1,4],[6,7]], [[3,2,5],[8,9,0]], [[3,5]], [[4,9,1]] ].

(c) Defina a função geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int) ,tal que,
 geraMat (x,y) (a,b) gera aleatoriamente uma matriz com x linhas e y colunas, cujos valores estão compreendidos entre a e b.
    Sugestão: use a função randomRIO :: Random a => (a,a) -> IO a.
-}

type Mat a = [[a]]

-- ---------------- a)

quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha (h:t) l = take h l : quebraLinha t (drop h l)

-- ---------------- b)

fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) pc m = quebraLinhas pc (take h m) ++ fragmenta t pc (drop h m)

quebraLinhas :: [Int] -> Mat a -> [Mat a]
quebraLinhas [] _ = []
quebraLinhas (h:t) m = map (take h) m : quebraLinhas t (map (drop h) m)

-- ---------------- c)

geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (x,y) r |(x==0) = return []
                |otherwise = do l <- geraLinha y r
                                m <- geraMat (x-1,y) r
                                return ( l : m )

geraLinha :: Int -> (Int, Int) -> IO [Int]
geraLinha x r |(x==0) = return []
              |otherwise = do n <- randomRIO r
                              t <- geraLinha (x-1) r
                              return (n : t)
