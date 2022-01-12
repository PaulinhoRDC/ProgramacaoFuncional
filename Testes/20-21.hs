module Teste2021 where

import  Data.Char

{-
1. Apresente uma definicção recursiva da função (\\) :: Eq a => [a] -> [a] -> [a]
que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira.
Por exemplo, (\\) [1,2,3,4,5,1,2] [2,3,4,1,2] == [5,1].
-}

remPrimOcorr :: Eq a => [a] -> [a] -> [a]
remPrimOcorr l [] = l
remPrimOcorr [] _ = []
remPrimOcorr l (x:xs) = remPrimOcorr (delete' x l) xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) = if (n==x)
  then xs
  else x: delete' n xs

{-
2. Considere o tipo MSet a para representar multi-conjuntos de elementos de a: type MSet a = [(a,Int)].
   Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero.

(a) Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um elemento a um multi-conjunto.
    Se o elemento não existir, deve ser retornado o multi-conjunto recebido.
    Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] == [(’b’,2), (’a’,4)].

(b) Usando uma função de ordem superior, defina a função calcula :: MSet a -> ([a],Int)
que, numa única travessia do multi-conjunto, calcula simulanemente a lista (sem repetidos) de elementos do multi-conjunto
e o número total de elementos.
    Por exemplo, calcula [(’b’,2), (’a’,4), (’c’,1)] == ([’b’,’a’,’c’],7).
-}

type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((n,i):t) = if (x==n)
  then if (i==1)
    then t
    else ((n,(i-1)):t)
  else (n,i): removeMSet x t

-- -----------------

calcula' :: MSet a -> ([a],Int)
calcula' [] = ([], 0)
calcula' ((n,i):t) = ( (n:outras) , i+outros )
     where (outras,outros) = calcula t

calcula  l = foldr (\(x,y) a ->  (x: fst a , y + snd a) ) ([] , 0) l

calcula2 l = foldl (\a (x,y) ->  (x: fst a , y + snd a) ) ([] , 0) l

calcula3 l = foldr auxfc ([] , 0) l

auxfc :: (a,Int) -> ([a],Int) -> ([a],Int)
auxfc (x,y) (lx,sy) = (x:lx, y+sy)

calcula4  l = foldr (\(x,y) (outras,outros) ->  (x: outras , y + outros) ) ([] , 0) l



{-
3. Defina a funcção partes :: String -> Char -> [String], que parte uma string pelos pontos onde um dado caracter ocorre.
Por exemplo, partes "um;bom;exemplo;" ’;’ == ["um","bom","exemplo"] e partes "um;exemplo;qualquer" ’;’ == ["um","exemplo","qualquer"].
-}

partes' :: String -> Char -> [String]
partes' "" _ = []
partes' s c = let (p,r) = span (/= c) s
              in case r of
                [] -> [p]
                x:xs -> p : partes' xs c

partes :: String -> Char -> [String]
partes [] _ = []
partes (x:l) c
    | x == c = partes l c
partes l c = let (left, right) = span (/=c) l
             in left : partes right c

{-
4. Considere o seguinte tipo para representar árvores binárias de procura.

                      data BTree a = Empty | Node a (BTree a) (BTree a)

                      a1 = Node 5 (Node 3 Empty Empty)
                                  (Node 7 Empty (Node 9 Empty Empty))

(a)Defina a função remove :: Ord a => a -> BTree a -> BTree a, que remove um elemento de uma árvore binária de procura.

(b) Defina BTree a como uma instância da classe Show, de forma a que, show a1 produza a string "((* <-3-> *) <-5-> (* <-7-> (* <-9-> *)))"
-}

data BTree a = Empty | Node a (BTree a) (BTree a)

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node a esq dir) |(n<a) = Node a (remove n esq) dir
                          |(n>a) = Node a esq (remove n dir)
                          |otherwise = (Node minarvdir esq dir2)
                where minarvdir = minA dir
                      dir2 = arvSemMin dir

minA :: Ord a => BTree a -> a
minA (Node a Empty _) = a
minA (Node a esq _) = minA esq

arvSemMin :: Ord a => BTree a -> BTree a
arvSemMin Empty = Empty
arvSemMin (Node a Empty dir) = dir
arvSemMin (Node a esq dir) = (Node a (arvSemMin esq) dir)

-- ----------------

instance Show a => Show ( BTree a ) where
    show Empty = "*"
    show (Node x e d) = "(" ++ show e ++ " <-" ++ (show x) ++ "-> " ++ show d ++ ")"

{-
5. Apresente uma definição da função sortOn :: Ord b => (a -> b) -> [a] -> [a]
que ordena uma lista comparando os resultados de aplicar uma função de extração de uma chave a cada elemento de uma lista.
   Por exemplo: sortOn snd [(3,1),(2,5),(1,2)] == [(3,1),(1,2),(2,5)].
-}

mysortOn :: Ord b => (a->b) ->[a] -> [a]
mysortOn f []    = []
mysortOn f (h:t) = insere f h (mysortOn f t)


insere :: Ord b => (a -> b) -> a -> [a] -> [a]
insere f x [] = [x]
insere f x (h:t) = if f x > f h
                   then h: insere f x t
                   else x:h:t

{-
6. Considere o seguinte tipo para representar um sistema hierárquico de ficheiros
                  data FileSystem = File Nome | Dir Nome [FileSystem]
                  type Nome = String

                  fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
                                   Dir "yyy" [],  Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

(a) Defina a função fichs :: FileSystem -> [Nome], que lista o nome de todos os ficheiros de um file system.

(b) Defina a função dirFiles:: FileSystem -> [Nome] -> Maybe [Nome]
que lista o nome dos ficheiros de um file system que estão numa determinada path.
Se a path não for válida, a função deve devolver Nothing.
Por exemplo, dirFiles fs1 ["usr","xxx"] == Just ["abc.txt","readme"]

(c) Defina a função listaFich :: FileSystem -> IO ()
que lê uma path do teclado e imprime no ecrã os nomes dos ficheiros que estão na diretoria indicada pela path.
A path deve ser lida como uma string com o formato usual (por exemplo: ”usr/xxx/PF”).
Se a path não for válida, deve ser escrita a mensagem ”Não é uma directoria.”
-}

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String

fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs",File "exemplo2.hs"]],
                 Dir "yyy" [],  Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

-- ------------- a)

fichs :: FileSystem -> [Nome]
fichs (File n) = [n]
fichs (Dir n l) = (concat (map fichs l)) -- OU -- concat $ map fich l

-- ------------- b)

--dirFiles fs1 ["usr","xxx"] == Just ["abc.txt","readme"]

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles fs [] = Just (fichs' fs)
dirFiles (File _) (h:t) = Nothing
dirFiles (Dir x l) (h:t) |(x==h) = dirFilesAux l t
                         |otherwise = Nothing

dirFilesAux :: [FileSystem] -> [Nome] -> Maybe [Nome]
dirFilesAux [] (_:_) = Nothing
dirFilesAux lfs [] = Just (concat (map fichs' lfs))
dirFilesAux ((File _): fs) (n:ns) = dirFilesAux fs (n:ns)
dirFilesAux ((Dir d l): fs) (n:ns) |(n==d)    = dirFilesAux l ns
                                   |otherwise = dirFilesAux fs (n:ns)

fichs' :: FileSystem -> [Nome]
fichs' (File n) = [n]
fichs' _ = []


-- ------------------c)

listaFich :: FileSystem -> IO ()
listaFich fs = do
    putStrLn "dá me uma diretoria"        -- ”usr/xxx/PF”
    path <- getLine
    if dirFiles fs (partes' path '/') == Nothing
        then do putStr "Não é diretoria"
        else putStrLn ( unwords ( f (dirFiles fs (partes' path '/') )))

f (Just x) = x
