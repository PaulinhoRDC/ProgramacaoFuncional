module Especial2021 where

import Data.Char
import System.Random
import System.IO
import System.IO.Error

-- -------------1)

-- -------a)

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h:h2:t) = h2 : posImpares t

-- -------b)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] l = True
isPrefixOf l [] = False
isPrefixOf (h:t) (h2:t2) = (h==h2) && isPrefixOf t t2

-- -------------2)

type Mat a = [[a]]

ma :: Mat Int
ma = [[1,2,3], [0,4,5], [0,0,6]]

ma2 :: Mat Int
ma2 = [[1,1,1], [1,1,1], [1,1,1]]

{-
| 1 2 3 |
| 0 4 5 |
| 0 0 6 |
-}

-- -------a)

zeros :: Eq a => Num a => Mat a -> Int
zeros [] = 0
zeros (h:t) = quantos (filter (==0) h) + zeros t

quantos :: Eq a => Num a => [a] -> Int
quantos [] = 0
quantos (h:t) = if (h==0)
  then 1 + quantos t
  else quantos t

-- -------b)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (zipWith (+)) m1 m2

-- -------c)

{-
| 1 2 3 |      ------>      | 1 0 0 |
| 0 4 5 |                   | 2 4 0 |
| 0 0 6 |                   | 3 5 6 |
-}

transpose :: Mat a -> Mat a
transpose ([]: _) = []
transpose m = (map head m) : transpose (map tail m)

-- -------------3)

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving Show

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

turma :: BTree (Int,String)
turma = Node (5,"Hugo") (Node (4,"Joao") Empty Empty)
                        (Node (6,"Pedro") Empty (Node (9,"Ana") Empty Empty))

-- -------a)

replace :: Eq a => BTree a -> a -> a -> BTree a
replace (Empty) _ _ = Empty
replace (Node a esq dir) x y =  if (a==x)
  then (Node y (replace esq x y) (replace dir x y))
  else (Node a (replace esq x y) (replace dir x y))

-- -------b)

insere :: Integer -> String -> BTree (Int,String) -> BTree (Int,String)
insere num nome (Empty) = (Node ((fromInteger num),nome) Empty Empty)
insere num nome (Node (n,no) esq dir) = if ((fromInteger num)==n)
  then (Node (n,nome) esq dir)
  else if ((fromInteger num)<n)
    then (Node (n,no) (insere num nome esq) dir)
    else (Node (n,no) esq (insere num nome dir))

-- -------------4)

data RTree a = R a [RTree a]
  deriving Show

type Dictionary = [ RTree (Char, Maybe String) ]

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
          R ('r',Nothing) [
            R ('a',Just "...")[
              R ('s', Just "...")[]
              ],
            R ('o', Just "...") [],
            R ('r',Nothing) [
              R ('o',Just "...") []
            ]]  ]   ]   ]

-- -------a)

consulta :: String -> Dictionary -> Maybe String
consulta [] ((R (c,s) []) : mais) = s
consulta (h:t) ((R (c,s) resto) : mais) |(h==c) = consulta t resto
                                        |otherwise = consulta (h:t) mais


-- -------b)

{-
palavras :: Dictionary -> [String]
palavras [] = []
palavras ((R (c,s) resto) : mais) = concat( (c : palavras resto) ) : palavras mais
-}

-- -------c)

--apresenta :: Dictionary -> IO ()
