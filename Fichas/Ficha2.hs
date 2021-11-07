module Ficha1 where

import  Data.Char

{-
1. Indique como é que o interpretador de Haskell avalia as expressões das alíneas que se seguem,
 apresentando a cadeia de redução de cada uma dessas expressães (i.e., os vários passos intermádios atá se chegar ao valor final).
-}

-- a) Considere a seguinte definição:

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- qual é o valor de funA [2,3,5,1] ?
{-
  funA [2,3,5,1]
  funA 2:3:5:1:[]
  4.0 + funA 3:5:1:[]
  4.0 + 9.0 + funA 5:1:[]
  4.0 + 9.0 + 25.0 funA 1:[]
  4.0 + 9.0 + 25.0 + 1.0 + funA []
  4.0 + 9.0 + 25.0 + 1.0 + 0
  39.0
-}

-- b) Considere seguinte definição:

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
                            else (funB t)

-- qual é o valor de funB [8,5,12]?
{-
  funB [8,5,12]
  funB 8:5:12:[]
  8: funB 5:12:[]
  8: funB 12:[]
  8:12: funB []
  8:12
  [8,12]
-}

-- c) Considere a seguinte definição:

funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- qual é o valor de funC [1,2,3,4,5]?
{-
  funC [1,2,3,4,5]
  funC 1:2:3:4:5:[]
  funC 3:4:5:[]
  funC 5:[]
  [5]

  funC :: [a] -> [a]
  funC x
    | even (length x) = []
    | otherwise = [last x]

  IGUAL
  -}

-- d) Considere a seguinte definição:

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- qual é o valor de funD "otrec"?
{-
  funD "otrec"
  g [] "otrec"
  g [o] "trec"
  g [to] "rec"
  g [rto] "ec"
  g [erto] "c"
  g [certo] ""
  "certo"
-}

{-
2. Defina recursivamente as seguintes funções sobre listas:
-}

-- a) dobros :: [Float] -> [Float] que recebe uma lista e produz a lista em que cada elemento é o dobro do valor correspondente na lista de entrada.

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

-- b) numOcorre :: Char -> String -> Int que calcula o número de vezes que um caracter ocorre numa string.

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (x:xs)
  | (c==x) = 1 + numOcorre c xs
  | otherwise = numOcorre c xs

-- c) positivos :: [Int] -> Bool que testa se uma lista só tem elementos positivos.

positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) = if (x>0)
  then positivos xs
  else False

-- d) soPos :: [Int] -> [Int] que retira todos os elementos não positivos de uma lista de inteiros.

soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if (x>0)
  then x: soPos xs
  else soPos xs

-- e) somaNeg :: [Int] -> Int que soma todos os números negativos da lista de entrada.

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if x<0
  then x + somaNeg xs
  else somaNeg xs

-- f) tresUlt :: [a] -> [a] devolve os últimos três elementos de uma lista.
--    Se a lista de entrada tiver menos de três elementos, devolve a própria lista.

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs)
  | (length (x:xs)<= 3) = (x:xs)
  | otherwise = tresUlt xs

-- Esta lternatica gasta muita memória, façamos outra que não o faça

tresUlt' :: [a] -> [a]
tresUlt' [] = []
tresUlt' [x] = [x]
tresUlt' [x,y] =[x,y]
tresUlt' [x,y,z] = [x,y,z]
tresUlt' (x:xs) = tresUlt' xs

-- g) segundos :: [(a,b)] -> [b] que calcula a lista das segundas componentes dos pares.

segundos :: [(a,b)] -> [b]
segundos []=[]
segundos ((x,xs):t) = xs: segundos t

-- h) nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool que testa se um elemento aparece na lista como primeira componente de algum dos pares.

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,xs):t)
  |(a==x) = True
  |otherwise = nosPrimeiros a t

-- i) sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) soma uma lista de triplos componente a componente.

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):t) = (x+x2,y+y2,z+z2)
  where (x2, y2, z2) = sumTriplos t

{-
3. Recorrendo a funções do módulo Data.Char, defina recursivamente as seguintes funções sobre strings:
-}

-- a) soDigitos :: [Char] -> [Char] que recebe uma lista de caracteres, e selecciona dessa lista os caracteres que são algarismos.

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if (isDigit x)
  then x: soDigitos xs
  else soDigitos xs

-- b) minusculas :: [Char] -> Int que recebe uma lista de caracteres, e conta quantos desses caracteres são letras minúsculas.

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if (isLower x)
  then 1 + minusculas xs
  else minusculas xs

-- c) nums :: String -> [Int] que recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem.

nums :: String -> [Int]
nums [] = []
nums (x:xs)
  |(isDigit x) = digitToInt x: nums xs
  |otherwise = nums xs

{-
4. Uma forma de representar polinómios de uma variével é usar listas de monómios representados por pares (coeficiente, expoente)
  type Polinomio = [Monomio]
  type Monomio = (Float,Int)
Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinómio 2 x^3 + 3 x4 + 5 x3 + 4 x5.
-}

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quantos monómios de grau n existem em p.

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,xs):t) = if (n == xs)
  then 1 + conta n t
  else conta n t

-- b) grau :: Polinomio -> Int que indica o grau de um polinómio.

grau :: Polinomio -> Int
grau [] = 0
grau [(n,e)] = e
grau ((n,e):(n2,e2):t) = if e>e2
  then grau ((n,e):t)
  else grau ((n2,e2):t)

-- c) selgrau :: Int -> Polinomio -> Polinomio que selecciona os monómios com um dado grau de um polinómio.

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau e ((n1,e1):t)
  |(e==e1) = (n1,e1): selgrau e t
  |otherwise = selgrau e t

-- d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polinómio.

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((n,e):xs) = if n > 0 then (n*fromIntegral e, e-1) : deriv xs
                            else deriv xs
-- a função `fromIntegral` converte um valor inteiro num valor de vírgula flutuante
-- em haskell, é necessário que ambos os fatores de uma multiplicação sejam do mesmo tipo, ele não converte automaticamente

-- e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polinómio para uma dado valor de x.

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((n1,e1):t) = n1*(x^e1)+ calcula x t

-- f) simp :: Polinomio -> Polinomio que retira de um polinómio os monómios de coeficiente zero.

simp :: Polinomio -> Polinomio
simp [] = []
simp ((n1,e1):t) = if (n1==0)
  then simp t
  else (n1,e1): simp t

-- g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplicação de um monómio por um polinómio.

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (n,e) ((n1,e1):t) = (n*n1,e+e1) : mult (n,e) t

-- h) normaliza :: Polinomio -> Polinomio que dado um polinómio constrói um polinómio
--   equivalente em que não podem aparecer varios monómios com o mesmo grau.

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,e):t) = insere (c,e) (normaliza t)

insere :: Monomio -> Polinomio -> Polinomio
insere (c,e) [] = [(c,e)]
insere (c,e) ((a,b):t) | (e==b) = (c+a,b):t
                       | otherwise = (c,e): insere (c,e) t

-- i) soma :: Polinomio -> Polinomio -> Polinomio que soma dois polinómios de forma a que
-- se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado.

--soma :: Polinomio -> Polinomio -> Polinomio


-- j) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de dois polinómios

--produto :: Polinomio -> Polinomio -> Polinomio
--produto [] _ = []
--produto ((n1,e1):t) ((n2,e2):t2) =

-- k) ordena :: Polinomio -> Polinomio que ordena um polinómio por ordem crescente dos graus dos seus monómios.

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena [(b,e)] = [(b,e)]
ordena ((n1,e1):(n2,e2):t)=if ( (e1>e2) || ((e1==e2) && (n1>=n2)) )
  then (n2,e2) : ordena ((n1,e1):t)
  else (n1,e1) : ordena ((n2,e2):t)

-- l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polinómios são equivalentes.

--equiv :: Polinomio -> Polinomio -> Bool
