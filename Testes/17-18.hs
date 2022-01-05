module Teste1718 where

import  Data.Char

import System.Random
import System.IO
import System.IO.Error

-- ------------1)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(h:t) = if (x<=h)
  then x:l
  else h: insert x t

-- ------------2)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t
                            Just a -> a: catMaybes t

-- ------------3)

data Exp a = Const a | Var String | Mais (Exp a) (Exp a) | Mult (Exp a) (Exp a)

instance (Show a) => Show (Exp a) where
  show (Const a) = show a
  show (Var s) = s
  show (Mais e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

-- ------------4)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insert' f x (sortOn f xs)

insert' :: Ord b => (a -> b) -> a -> [a] -> [a]
insert' f n [] = [n]
insert' f n (x:xs) = if (f n <= f x)
  then n:x:xs
  else x: insert' f n xs

-- ------------5)

-- ------a)

amplitude ::  [Int] -> Int
amplitude [] = 0
amplitude l = maior - menor
  where (maior,menor) = foldl (\(a,b) x -> (if x>a then x else a , if x<b then x else b)) (head l, head l) l

-- ---  OUUUUUU

amplitude' :: [Int] -> Int
amplitude' [] = 0                    --  min max l
amplitude' l@(h:t) = m2 - m1
    where (m1,m2) = ampaux h h l

-- devolve o min e o max da lista
ampaux :: Int -> Int -> [Int] -> (Int, Int)
ampaux m1 m2 [] = (m1, m2)
ampaux m1 m2 (h:t) = ampaux (min m1 h ) (max m2 h) t

-- ------b)

{-
parte :: [Int] -> ([Int],[Int])
parte l = foldl1 (\(acc1,acc2) (a,b) -> if amplitude acc1 + amplitude acc2 < amplitude a + amplitude b then (acc1,acc2) else (a,b)) combinacoes
    where combinacoes = foldl (\acc n -> splitAt n sl : acc) [] [1..(length l - 1)]
          sl = iSort l
-}

-- ------------6)

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
      deriving (Show)

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

-- ------a)

conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover par img) = conta img
conta (Juntar imgs) = sum (map conta imgs)
-- ------b)

apaga :: Imagem -> IO Imagem
apaga i = do
    let k = conta i
    x <- randomRIO (1,k)
    return (snd (apagaux x i))

apagaux :: Int -> Imagem -> (Int,Imagem)
apagaux 1 (Quadrado _ ) = (0,Juntar [])
apagaux x (Quadrado v ) = (x-1, Quadrado v )

apagaux x (Mover v i) = (n, Mover v rq)
    where (n,rq) = apagaux x i

apagaux x (Juntar l ) = ( c, Juntar im)
    where (c,im) = apagaux2 x l
          apagaux2 0 l      = (0,l)
          apagaux2 n (x:xs) =(c2 , ima: imat )
              where (c,ima) = apagaux n x
                    (c2,imat) = apagaux2 c xs
