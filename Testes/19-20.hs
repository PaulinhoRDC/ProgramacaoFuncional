module Teste1920 where

import  Data.Char

import System.Random
import System.IO
import System.IO.Error

{-
1. Apresente uma definição recursiva de cada uma das seguintes funções (pré-definidas) sobre listas:

(a) intersect :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.
 Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].

(b) tails :: [a] -> [[a]] que calcula a lista dos sufixos de uma lista.
 Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].
-}

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) l = if (x `elem` l) then x: (intersect xs l)
                                     else intersect xs l

-- OU

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l l2 = foldr (\x acc -> if x `elem` l2 then x:acc else acc) [] l

-- --------------

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = [l] ++ tails(tail l)

{-
2. Para armazenar conjuntos de números inteiros, optou-se pelo uso de sequências de intervalos.
Assim, por exemplo, o conjunto {1,2,3,4,7,8,19,21,22,23} poderia ser representado por [(1,4),(7,8),(19,19),(21,23)].

                    type ConjInt = [Intervalo]
                    type Intervalo = (Int,Int)

(a) Defina uma função elems :: ConjInt -> [Int] que, dado um conjunto, dá como resultado a lista dos elementos desse conjunto.

(b) Defina uma função geraconj :: [Int] -> ConjInt que recebe uma lista de inteiros,
ordenada por ordem crescente e sem repetições, e gera um conjunto.
    Por exemplo, geraconj [1,2,3,4,7,8,19,21,22,23] = [(1,4),(7,8),(19,19),(21,23)].
-}

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []
elems ((x,x2):t) | (x<x2)    = x: elems ((x+1,x2):t)
                 | otherwise = x: elems t

-- --------------

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = enquantoCrescente l (head l) (head l)

enquantoCrescente :: [Int] -> Int -> Int  -> ConjInt
enquantoCrescente [] x y       = []
enquantoCrescente [n] x y      = [(x,y)]
enquantoCrescente (n:n2:t) x y |(n==(n2-1)) = enquantoCrescente (n2:t) x n2
                               |otherwise   = (x,y) : enquantoCrescente (n2:t) n2 n2

-- OU

geraconj' :: [Int] -> ConjInt
geraconj' [] = []
geraconj' (h : t) = (h, d) : geraconj (dropWhile (<= d) t)
    where d = foldl (\acc x -> if x == succ acc then x else acc) h t


{-
3. Para armazenar uma agenda de contactos telefónicos e de correio electrónico definiram-se os seguintes tipos de dados.
   Não existem nomes repetidos na agenda e para cada nome existe uma lista de contactos.
                        data Contacto = Casa Integer
                                      | Trab Integer
                                      | Tlm Integer
                                      | Email String
                              deriving (Show)

                       type Nome = String
                       type Agenda = [(Nome, [Contacto])]

(a) Defina a função acrescEmail :: Nome -> String -> Agenda -> Agenda que,dado um nome, um email e uma agenda,
acrescenta essa informação à agenda.

(b) Defina a função verEmails :: Nome -> Agenda -> Maybe [String] que, dado um nome e uma agenda,
retorna a lista dos emails associados a esse nome. Se esse nome não existir na agenda a função deve retornar Nothing.

(c) Defina a função consulta :: [Contacto] -> ([Integer],[String]) que, dada lista de contactos,
retorna o par com a lista de números de telefone (tanto telefones fixos como telemóveis) e a lista de emails, dessa lista.
Implemente a função de modo a fazer uma única travessia da lista de contactos.

(d) Defina a função consultaIO :: Agenda -> IO () que, dada uma agenda,
lê do teclado o nome que pretende consultar e apresenta no ecrã os contactos associados a esse nome na agenda.
-}

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
      deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]


agenda1 :: Agenda
agenda1 = [("Sofia", [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]),("Luís", [Tlm 69420]),("Rita", [Trab 58008])]

contacto1 :: [Contacto]
contacto1 = [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz", Tlm 69420, Trab 58008]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome e [] = [(nome, [Email e])]
acrescEmail nome e agenda@((nomeX, contactos) : t)
                          | (nome == nomeX) = (nome, (Email e): contactos) : t
                          | otherwise       = head agenda : acrescEmail nome e t

-- -----------------

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):xs) | (n==x) = Just (daListaStr l)
                       | otherwise = verEmails n xs

daListaStr :: [Contacto] -> [String]
daListaStr [] = []
daListaStr (h:t) = case h of Email s -> s: daListaStr t
                             _ -> daListaStr t

-- -----------------

consulta :: [Contacto] -> ([Integer],[String])
consulta = foldr (\x (i,s) -> case x of Email email -> (i,email:s); otherwise -> (n x:i,s)) ([],[])
    where n x = case x of Casa num -> num
                          Trab num -> num
                          Tlm num -> num

-- OU

consTelefs :: [Contacto] -> ([Integer],[String])
consTelefs [] = ([],[])
consTelefs (h:t) = case h of Casa s  -> (s :a , b)
                             Trab s  -> (s :a , b)
                             Tlm s   -> (s :a , b)
                             Email s -> (a , s :b)
              where (a,b) = consTelefs t

-- -----------------

consultaIO :: Agenda -> IO ()
consultaIO agenda = do nome <- getLine
                       let contactos = aux nome agenda
                       putStr (concat [show x ++ "\n" | x <- contactos])

              where aux _ [] = []
                    aux nome ((name,contactos):t) = if (name == nome) then contactos else aux nome t



{-
4. Relembre o tipo RTree a definido nas aulas.
                                                data RTree a = R a [RTree a] deriving (Show, Eq)

(a) Defina a função paths :: RTree a -> [[a]] que dada uma destas árvores calcula todos os caminhos desde a raíz até às folhas.
Por exemplo, paths (R 1 [R 2 [], R 3 [R 4 [R 5 [], R 6 []]], R 7 []]) deve corresponder à lista [[1,2],[1,3,4,5],[1,3,4,6],[1,7]].

(b) Defina a função unpaths :: Eq a => [[a]] -> RTree a inversa da anterior, i.e.,
tal que, unpaths (paths t) == t, para qualquer árvore t :: Eq a => RTree a.
-}

data RTree a = R a [RTree a]
      deriving (Show, Eq)

{-

paths :: RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a outras) = [a: x | x <- concat [paths branch | branch <- outras]]

-- ---------------

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths l@((x:_):_) = R x (map unpaths l'')
     where l'  = map tail l                           -- Para ir buscar o elemento que é cabeça de todas as listas
           l'' = agrupa (\(x:_) (y:_) -> x==y) l'     -- Agrupar quais das subárvores começam pelo mesmo valor

agrupa :: Eq a => [[a]] -> [[[a]]]
agrupa [] = []
agrupa ((x:xs):t) = ((x:xs):l1) : agrupa l2
    where (l1,l2) = span (\(y:_)->x==y) t

-}
