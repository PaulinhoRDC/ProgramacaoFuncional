module Exame1718 where

import  Data.Char
import System.Random
import System.IO
import System.IO.Error

----- 1

(??) :: [a] -> Int -> a
(??) (h:t) n |(n==0)    = h
             |otherwise = (??) t (n-1)

----- 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of Norte -> posicao (x,y+1) t
                                Este  -> posicao (x+1,y) t
                                Oeste -> posicao (x-1,y) t
                                Sul   -> posicao (x,y-1) t

----- 3

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = (f h) || any' f t

any'' f l = foldl (\acc x -> if (f x) then True else acc) False l

any''' f l = foldr (\x acc -> if (f x) then True else acc) False l

----- 4

type Mat a = [[a]]

m1 :: Mat Int
m1 =[[1,2,3], [0,4,5], [0,0,6]]

triSup :: (Num a,Eq a) => Mat a -> Bool
triSup m = snd ( foldl (\(ac1,ac2) line -> (ac1+1, all (== 0) (take ac1 line) && ac2)) (0,True) m )

triSup' :: (Num a,Eq a) => Mat a -> Bool
triSup' m = snd ( foldr (\line (ac1,ac2) -> (ac1+1, all (== 0) (take ac1 line) && ac2)) (0,True) m )

----- 5

movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
    dir <- getChar
    case dir of 'n' -> moveFrom (x,y+1)
                's' -> moveFrom (x,y-1)
                'e' -> moveFrom (x+1,y)
                'o' -> moveFrom (x-1,y)
                otherwise -> return (x,y)

----- 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
      deriving Show

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

ex2 = Juntar [Mover (5,5) (Quadrado 4),
              Mover (5,6) (Quadrado 5),
              Mover (9,8) (Quadrado 2)]

ex3 = Juntar []

-- ----------- a)

vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover (_,_) img) = vazia img
vazia (Juntar imgs) | null imgs = True
                    | otherwise = elem True (map vazia imgs)    -- any (== True) (map vazia imgs)     -- or (map vazia imgs)


-- ----------- b)

maior :: Imagem -> Maybe Int
maior (Quadrado n) = Just n
maior (Mover _ img) = maior img
maior (Juntar imgs) | null imgs = Nothing
                    | otherwise = maximum' (filter (/= Nothing) (map maior imgs))
    where maximum' [] = Nothing
          maximum' l = maximum l

-- ----------- c)
{-

instance Eq Imagem where
    img1 == img2 = null $ (quadPos img1 (0,0)) `\\` (quadPos img2 (0,0))

quadPos :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quadPos (Quadrado n) pos = [(n,pos)]
quadPos (Mover (a,b) img) (x,y) = quadPos img (x+a,y+b)
quadPos (Juntar imgs) pos = concatMap (\x -> quadPos x (pos)) imgs

-}
