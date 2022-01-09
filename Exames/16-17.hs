module Exame1617 where

import  Data.Char
import System.Random
import System.IO
import System.IO.Error

-- ------------1)

-- ----a)

unlines' ::  [String] -> String
unlines' [] = ""
unlines' [x] = x
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

unlines'' ::  [String] -> String
unlines'' l@(h:t) = foldr (\x acc -> x ++ if(null t) then "" else "\n" ++ acc) "" l

-- ----b)

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) (x:t) l = if elem x l
  then (\\) t (delete x l)
  else x: (\\) t l

delete :: (Eq a) => a -> [a] -> [a]
delete x [] = []
delete x (h:t) = if (x==h)
  then t
  else h: delete x t

-- ------------2)

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

-- ----a)

primeiro :: Seq a -> a
primeiro (Inicio a s) = a
primeiro (Fim Nil a)  = a
primeiro (Fim s a)    = primeiro s

-- ----b)

semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a s)   = Inicio a (semUltimo s)
semUltimo (Fim s a)      = s


-- ------------3)

data BTree a = Empty | Node a (BTree a) (BTree a)
      deriving Show

-- ----a)

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node a esq dir) = Node a (prune (n-1) esq) (prune (n-1) esq)

-- ----b)

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node a Empty dir) = dir
semMinimo (Node a esq dir) = Node a (semMinimo esq) dir


-- ------------4)

type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

-- ----a)

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes [] = []
posicoes tab = [(x,y) | x <- [0..n], y <- [0..n], retira (x,y) tab  == 'R']
    where n = (length tab) - 1

retira :: (Int,Int) -> Tabuleiro -> Char
retira (x,y) tab = ((tab !! x) !! y)

-- ----b)

-- ----c)
