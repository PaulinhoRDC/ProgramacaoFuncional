module Questoes50 where

import Data.Char

{-
1. Apresente uma definição recursiva da função (pré-definida) enumFromTo :: Int -> Int -> [Int]
que constrói a lista dos números inteiros compreendidos entre dois limites.
Por exemplo, enumFromTo 1 5 corresponde à lista [1,2,3,4,5]
-}

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start end |(start > end)  = []
                      |(start == end) = [start]
                      |otherwise      = start: enumFromTo' (start+1) end

{-
2. Apresente uma definição recursiva da função (pré-definida) enumFromThenTo :: Int -> Int -> Int -> [Int]
que constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante.
Por exemplo, enumFromThenTo 1 3 10 corresponde à lista [1,3,5,7,9].
-}

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end | start > end && next > start || start < end && next < start || start == next && start > end = []
                               | otherwise = start: enumFromThenTo' next (next + (next-start)) end

{-
3.Apresente uma definiçã recursiva da função (pré-definida) (++) :: [a] -> [a] -> [a]
que concatena duas listas.
Por exemplo, (++) [1,2,3] [10,20,30] corresponde à lista [1,2,3,10,20,30].
-}

mm :: [a] -> [a] -> [a]
mm [] l = l
mm l [] = l
mm (h:t) l2 = h: mm t l2

{-
4. Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a
que dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição
(assume- se que o primeiro elemento se encontra na posição 0).
Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
-}

qualelem :: [a] -> Int -> a
qualelem [] _ = error "Lista Vazia || Inteiro negativo"
qualelem (h:_) 0 = h
qualelem (h:t) x = qualelem t (x-1)

{-
5. Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a]
que dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
Por exemplo, reverse [10,20,30] corresponde a [30,20,10].
-}

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse t ++ [h]

{-
6. Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a]
que dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l.
A lista resultado só terá menos de que n elementos se a lista l tiver menos do que n elementos.
Nesse caso a lista calculada é igual à lista fornecida.
Por exemplo, take 2 [10,20,30] corresponde a [10,20].
-}

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 l = []
take' n (h:t) = h: take' (n-1) t

{-
7. Apresente uma definição recursiva da função (pré-definida) drop :: Int -> [a] -> [a]
que dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.
Se a lista fornecida tiver n elementos ou menos, a lista resultante será vazia.
Por exemplo, drop 2 [10,20,30] corresponde a [30].
-}

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' n (h:t) = drop' (n-1) t

{-
8. Apresente uma definição recursiva da função (pré-definida) zip :: [a] -> [b] -> [(a,b)]
constrói uma lista de pares a partir de duas listas.
Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].
-}

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' l [] = []
zip' [] l = []
zip' (h:t) (h2:t2) = (h,h2) : zip' t t2

{-
9. Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a -> [a]
que dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais a x.
Por exemplo, replicate 3 10 corresponde a [10,10,10].
-}

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = if n<0
  then []
  else x: replicate' (n-1) x

{-
10. Apresente uma definição recursiva da função (pré-definida) intersperse :: a -> [a] -> [a]
que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é intercalado entre os elementos da lista fornecida.
Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].
-}

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' n (h:t) = h:n: intersperse' n t

{-
11. Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]]
que agrupa elementos iguais e consecutivos de uma lista.
Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].
-}

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [h] = [[h]]
group' (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group' t

--OU

group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' (h:t) = insere'' h (group'' t)

insere'' :: Eq a => a -> [[a]] -> [[a]]
insere'' x [] = [[x]]
insere'' x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)

{-
12. Apresente uma definição recursiva da função (pré-definida) concat :: [[a]] -> [a]
que concatena as listas de uma lista.
Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].
-}

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat t

{-
13. Apresente uma definição recursiva da função (pré-definida) inits :: [a] -> [[a]]
que calcula a lista dos prefixos de uma lista.
Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].
-}

inits' :: [a]->[[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]  -- init retira último elemento da lista      // OU //   inits' (init l) : l

{-
14. Apresente uma definição recursiva da função (pré-definida) tails :: [a] -> [[a]]
que calcula a lista dos sufixos de uma lista.
Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].
-}

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)  -- tail retira primeiro elemento da lista

{-
15. Defina a função heads :: [[a]] -> [a]
que recebe uma lista de listas e produz a lista com o primeiro elemento de cada lista.
Por exemplo, heads [[2,3,4],[1,7],[],[8,5,3]] corresponde a [2,1,8].
-}

heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (h:t) = head h : heads' t

{-
16. Defina a função total :: [[a]] -> Int
que recebe uma lista de listas e conta o total de elementos (de todas as listas)
Por exemplo, total [[2,3,4],[1,7],[],[8,5,3]] corresponde a 8.
-}

total' :: [[a]] -> Int
total' [] = 0
total' (h:t) = length h + total' t

{-
17. Defina a função fun :: [(a,b,c)] -> [(a,c)]
que recebe uma lista de triplos e produz a lista de pares com o primeiro e o terceiro elemento de cada triplo.
Por exemplo, fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a
[("rui",2), ("maria",2), ("ana",7)].
-}

fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((a,b,c):t) = (a,c): fun' t

{-
18. Defina a função cola :: [(String,b,c)] -> String
que recebe uma lista de triplos e concatena as strings que estão na primeira componente dos triplos.
Por exemplo, cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a "ruimariaana".
-}

cola' :: [(String,b,c)] -> String
cola' [] = ""
cola' ((a,b,c):t) = a ++ cola' t

{-
19. Defina a função idade :: Int -> Int -> [(String,Int)] -> [String]
que recebe o ano, a idade e uma lista de pares com o nome e o ano de nascimento de cada pessoa,
e devolve a listas de nomes das pessoas que nesse ano atingirão ou já ultrapassaram a idade indicada.
Por exemplo, idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] corresponde a ["rui","ana"].
-}

idade' :: Int -> Int -> [(String,Int)] -> [String]
idade' _ _ [] = []
idade' ano idade ((nome,ano2):t) = if ( (ano-ano2) >= idade)
  then nome : idade' ano idade t
  else idade' ano idade t

{-
20. Apresente uma definição recursiva da função, powerEnumFrom :: Int -> Int -> [Int]
que dado um valor n e um valor m constrói a lista [n^0, . . . , n^m−1].
-}

powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n 1 = [1]
powerEnumFrom' n m | m > 1 = powerEnumFrom' n (m-1) ++ [n^(m-1)]
                   | otherwise = []

{-
21. Apresente uma definiçãoo recursiva da função, isPrime ::  Int -> Bool
que dado um número inteiro maior ou igual a 2 determina se esse número é primo.
Para determinar se um número n é primo, descubra se existe algum número inteiro m tal que 2 ≤ m ≤ √n e mod n m = 0.
Se um tal número não existir então n é primo,e se existir então n não é primo.
-}
