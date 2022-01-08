module Especial1920 where

import  Data.Char
import System.Random
import System.IO
import System.IO.Error

-- -------------1)

-- -------a)

subst :: Eq a => (a,a) -> [a] -> [a]    -- subst (3,0) [1,2,3,4,3,2,3,4,5] = [1,2,0,4,0,2,0,4,5]
subst _ [] = []
subst (x,y) (h:t) |(x==h) = y: subst (x,y) t
                  |otherwise = h: subst (x,y) t

subst' :: Eq a => (a,a) -> [a] -> [a]
subst' (x,y) l = foldr(\h acc -> if (h==x) then y:acc else h:acc) [] l


-- -------b)

posicoes :: [a] -> [Int] -> [a]    -- posicoes [7,4,9,1,2,3,4,5,1] [1,5,2] = [7,2,4]
posicoes [] _ = []
posicoes _ [] = []
posicoes l (h:t) = (l !! (h-1)): posicoes l t

-- -------------2)

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)

-- -------a)

folhas :: Tree a b -> [b]
folhas (Leaf b) = [b]
folhas (Node a esq dir) = (folhas esq) ++ (folhas dir)

-- -------b)

somas :: Tree Float Int -> (Float, Int)
somas (Leaf b) = (0,b)
somas (Node a esq dir) = (a+x,y)
        where (x,y) = (f+f2,i+i2)
              (f,i) = somas esq
              (f2,i2) = somas dir

-- -------------3)

type Mat a = [[a]]

{-
   | 1 2 3 |
   | 0 4 5 |
   | 0 0 6 |     ,seria representada por: [[1,2,3], [0,4,5], [0,0,6]]
-}

ma :: Mat Int
ma = [[1,2,3], [0,4,5], [0,0,6]]


rotateLeft :: Mat a -> Mat a
rotateLeft [] = [[]]
rotateLeft l@(h:t) = let l2 = map last l
                         aUsar = ((length h)-1)
                     in if (aUsar == 0)
                       then [l2]
                       else l2: rotateLeft (map (take ((length h)-1)) l)

rotateLeft' :: Mat a -> Mat a
rotateLeft' m = reverse (transpose m)

transpose :: Mat a -> Mat a
transpose ([]: _) = []
transpose m = (map head m) : transpose (map tail m)

-- -------------4)

type Filme = (Titulo,Realizador,[Actor],Genero,Ano)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int
data Genero = Comedia | Drama | Ficcao | Accao | Animacao | Documentario
        deriving (Eq,Show)
type Filmes = [ Filme ]

lista :: Filmes
lista = [("Boas", "Paulo", ["cuestz","lafamiglia"], Accao, 2022),("Boas2", "Maria", ["cuestz","maryjojo"], Comedia, 1922),("Boas3", "Paulo", [], Documentario, 1000)]

-- -------a)

doRealizador :: Filmes -> Realizador -> [Titulo]
doRealizador [] _ = []
doRealizador ((t,n,_,_,_):resto) nome = if (nome == n)
  then t: doRealizador resto nome
  else doRealizador resto nome

-- -------b)

doActor :: Filmes -> Actor -> [Titulo]
doActor [] _ = []
doActor ((t,_,atores,_,_):resto) nome = if (elem nome atores)
  then t: doActor resto nome
  else doActor resto nome

-- -------c)

consulta :: Filmes  -> Genero -> Realizador -> [(Ano, Titulo)]
consulta bd gen rea = map aux (filter (teste gen rea) bd)
    where teste :: Genero -> Realizador -> Filme -> Bool
          teste g r (_,x,_,y,_) = g==y && r==x

---- PERGUNTA -> Qual a função aux e qual o seu tipo?

aux :: Filme -> (Ano,Titulo)
aux (t,_,_,_,a) = (a,t)

-- -------------5)

data Avaliacao = NaoVi | Pontos Int deriving (Eq,Show)

type FilmesAval = [(Filme,[Avaliacao])]

lalala :: Filmes
lalala = [ ("eque"    , "gay", ["gay1"  , "gay2", "gay3"], Comedia, 2000),
           ("nada"    , "gay", ["lesb1", "lesb2", "gay3"], Comedia, 3000),
           ("queia"   , "les", ["gay1"  ,"lesb1", "les2"], Ficcao , 1000),
           ("manteiga", "gay", ["lesb1"   ,"gay2","gay3"], Comedia, 4000)
         ]

fa :: FilmesAval
fa = [ (("eque", "gay", ["gay1"  , "gay2", "gay3"], Comedia, 2000),[Pontos 3]),
       (("nada", "gay", ["lesb1", "lesb2", "gay3"], Comedia, 3000),[Pontos 4])
         ]

-- -------a)

avalia :: FilmesAval -> IO FilmesAval
avalia lista = do putStrLn "Nome do filme: "
                  nomeFilme <- getLine
                  putStrLn "Sua avaliação:  (Entre 1 e 5)"
                  avaliacao <- getLine

                  let k = read avaliacao     -- Passar de String para Int
                      filme = [ ((nome,m,l,n,b),(Pontos k) : av) | ((nome,m,l,n,b),av) <- lista , nome == nomeFilme]

                  if null filme
                    then return lista
                    else return (filme ++  [ ((n,m,l,b,v),av) | ((n,m,l,b,v),av) <- lista , n/= nomeFilme ] )

-- -------b)

listaPorGeneros :: FilmesAval -> [(Genero,[(Titulo,Avaliacao)])]
listaPorGeneros l = nub [ t b | ((n,m,l,b,v),av) <- l]
    where t genero = (genero , [(n,mediaf av) | ((n,m,l,b,v),av) <- l , b == genero] )

mediaf :: [Avaliacao] -> Avaliacao
mediaf l = if null m
           then NaoVi
           else Pontos $ div (sum m) (length l)
    where m = media l

media :: [Avaliacao] -> [Int]
media [] = []
media ((Pontos x):t) = x : media t
media (NaoVi:t) = media t
