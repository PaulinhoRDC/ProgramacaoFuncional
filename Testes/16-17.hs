module Teste1617 where

import  Data.Char
import System.Random
import System.IO
import System.IO.Error
import Data.Maybe (fromMaybe)


-- ------------1)

type MSet a = [(a,Int)]

ex1 :: MSet Char
ex1 = [('b',4),('a',4),('c',1)]

-- ----a)

cardMSet ::  MSet a -> Int
cardMSet [] = 0
cardMSet ((_,x):t) = x + cardMSet t

-- ----b)

moda :: Eq a => MSet a -> [a]
moda [] = []
moda [(x,_)] = [x]
moda ((x,n):(y,n2):t) = if n==n2
  then x: moda ((y,n2):t)
  else [x]


-- ----c)

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((c,x):t) = if (x==1)
  then c: converteMSet t
  else c: converteMSet ((c,x-1):t)

-- ----d)

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] x v        = [(x,v)]
addNcopies l@((c,n):t) x v | (x==c) = ((c,n+v):t)
                           | otherwise = if (n>=m) then (c,n):(b,m):cauda
                                                   else (b,m):(c,n):cauda
                    where (b,m):cauda = addNcopies t x v



-- ------------2)

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

ex2 :: SReais
ex2 = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)    --   --> ((]4.2,5.5[ U ]3.1,7.0]) U [-12.3,30.0])


-- ----a)

instance Show SReais where
  show (Uniao s1 s2) = "(" ++ show s1 ++ " U " ++ show s2 ++ ")"
  show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
  show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
  show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
  show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"


-- ----b)

pertence :: Double -> SReais -> Bool
pertence n (Uniao s1 s2) = (pertence n s1) || (pertence n s2)
pertence n (AA x y) = n>x && n<y
pertence n (AF x y) = n>x && n<=y
pertence n (FF x y) = n>=x && n<=y
pertence n (FA x y) = n>=x && n<y

-- ----c)

tira ::  Double -> SReais -> SReais
tira n (Uniao a b) | pertence n a = Uniao (tira n a) b
                   | pertence n b = Uniao a (tira n b)
                   | otherwise = (Uniao a b)

tira n interv = case interv of (AA a b) -> if (n>a && n<b) then (Uniao (AF a n) (FA n b)) else interv
                               (AF a b) -> if (n==b) then (AA a b) else if (n>a && n<=b) then (Uniao (AF a n) (FF n b)) else interv
                               (FF a b) -> if (n==a) then (AF a b) else if (n==b) then (FA a b) else if (n>=a && n<=b) then (Uniao (AF a n) (FA n b)) else interv
                               (FA a b) -> if (n==a) then (AA a b) else if (n>=a && n<b) then (Uniao (AF a n) (FA n b)) else interv
-- ------------3)

data RTree a = R a [RTree a]

-- ----a)


f :: a -> Maybe [a] -> Maybe [a]
f _ Nothing = Nothing
f x (Just l) = Just (x : l)

percorre :: [Int]-> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre _ (R a []) = Nothing
percorre (x:xs) (R a l) = if length l< x then Nothing else f a (percorre xs (l !! (x-1)))

-- ----b)

procura :: Eq a => a -> RTree a -> Maybe [Int]
procura n (R a r) | n == a = Just []
                  | null r = Nothing
                  | otherwise = foldl (\acc num -> if procura n (r !! (num - 1)) == Nothing then acc else Just (num:fromMaybe [] (procura n (r !! (num - 1))))) Nothing [1..length r]
