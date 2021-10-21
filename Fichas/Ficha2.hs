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
