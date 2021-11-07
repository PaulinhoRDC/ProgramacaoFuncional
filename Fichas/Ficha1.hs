module Ficha1 where

import Data.Char  --Para a (8)

{-
1. Usando as seguintes funções pré-definidas do Haskell:
    • length l: o número de elementos da lista l
    • head l: a cabeça da lista (não vazia) l
    • tail l: a cauda da lista (não vazia) l
    • last l: o último elemento da lista (não vazia) l
    • sqrt x: a raiz quadrada de x
    • div x y: a divisão inteira de x por y
    • mod x y: o resto da divisão inteira de x por y
-}

-- a) perimetro – que calcula o perímetro de uma circunferência, dado o comprimento do seu raio.

perimetro :: Double -> Double
perimetro x = 2 * pi * x

-- b) dist – que calcula a distância entre dois pontos no plano Cartesiano. Cada ponto é um par de valores do tipo Double.

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2 )

-- c) primUlt – que recebe uma lista e devolve um par com o primeiro e o último elemento dessa lista.

primUlt :: [a] -> (a,a)
primUlt [] = error "Não tem nem primeiro nem último elemento"
primUlt l = (head l, last l)

-- d) multiplo – tal que multiplo m n testa se o número inteiro m é múltiplo de n.

multiplo :: Int -> Int -> Bool
multiplo x y
  | mod x y  == 0 = True
  | otherwise = False

-- e) truncaImpar – que recebe uma lista e, se o comprimento da lista for ímpar retira-lhe o primeiro elemento,
-- caso contrário devolve a própria lista.

truncaImpar :: [a] -> [a]
truncaImpar l = if even(length l)
  then l
  else tail l

-- f) max2 – que calcula o maior de dois números inteiros.

max2 :: Int -> Int -> Int
max2 x y = if (x>y) then x else y

-- g) max3 – que calcula o maior de três números inteiros, usando a função max2.

max3 :: Int -> Int -> Int -> Int
max3 x y z = (max2 (max2 x y) z)

{-
2. Defina as seguintes funções sobre polinómios de 2º grau:
-}

-- a) A função nRaizes que recebe os (3) coeficientes de um polinómio de 2º grau e que
-- calcula o número de raízes (reais) desse polinómio.

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c
  | res > 0 = 2
  | res == 0 = 1
  | res < 0 = 0
  where res= (b^2) - 4*a*c

-- b) A função raizes que, usando a função anterior, recebe os coeficientes do polinómio
-- e calcula a lista das suas raízes reais.

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
  | n == 2 = [x1, x2]
  | n == 1 = [x1] -- Neste caso x1 e x2 são iguais, por isso podemos devolver apenas um dos valores
  | n == 0 = []
  where n = nRaizes a b c
        delta = b^2 - 4*a*c
        (x1,x2) = (((-b) + sqrt delta)/ (2*a), ((-b) - sqrt delta)/ (2*a))

{-
3. Vamos representar horas por um par de nu ́meros inteiros:
type Hora = (Int,Int)
Assim o par (0,15) significa meia noite e um quarto e (13,45) duas menos um quarto.
-}

type Hora = (Int,Int)

-- a) testar se um par de inteiros representa uma hora do dia válida;

horaValida :: Hora -> Bool
horaValida (x,y) = x<=24 && x>=0 && y<60 && y>=0

-- b) testar se uma hora é ou não depois de outra (comparação);

horaPosterior :: Hora -> Hora -> Bool
horaPosterior (x1,y1) (x2,y2) = if ( (x1>x2) || (x1==x2 && y1>y2) ) then True else False

-- c) converter um valor em horas (par de inteiros) para minutos (inteiro);

horaPMin :: Hora -> Int
horaPMin (x,y) = 60 * x + y

-- d) converter um valor em minutos para horas;

minPHora :: Int -> Hora
minPHora x = (div x 60 , mod x 60)

-- e) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);

difHora :: Hora -> Hora -> Int
difHora (x1,y1) (x2,y2)
  | res < 0 = -res
  | otherwise = res
  where res = abs ( horaPMin(x1,y1) - horaPMin(x2,y2) )

-- f) adicionar um determinado número de minutos a uma dada hora.

acresHora :: Hora -> Int -> Hora
acresHora (x1,y1) min = minPHora ( horaPMin(x1,y1) + min )

{-
4. Repita o exercício anterior assumindo agora que as horas são representadas por um novo tipo de dados:

data Hora = H Int Int deriving (Show,Eq)

Com este novo tipo a hora meia noite e um quarto é representada por H 0 15 e a hora
duas menos um quarto por H 13 45.
-}

data Hora1 = H Int Int deriving (Show,Eq)

-- a) testar se um par de inteiros representa uma hora do dia válida;

horaValida2 :: Hora1 -> Bool
horaValida2 (H x y) = x<=24 && x>=0 && y<60 && y>=0

-- b) testar se uma hora é ou não depois de outra (comparação);

horaPosterior2 :: Hora1 -> Hora1 -> Bool
horaPosterior2 (H x1 y1) (H x2 y2) = if ( (x1>x2) || (x1==x2 && y1>y2) ) then True else False

-- c) converter um valor em horas (par de inteiros) para minutos (inteiro);

horaPMin2 :: Hora1 -> Int
horaPMin2 (H x y) = 60 * x + y

-- d) converter um valor em minutos para horas;

minPHora2 :: Int -> Hora1
minPHora2 x = (H (div x 60)  (mod x 60))

-- e) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);

difHora2 :: Hora1 -> Hora1 -> Int
difHora2 (H x1 y1) (H x2 y2)
  | res < 0 = -res
  | otherwise = res
  where res = abs ( horaPMin2(H x1 y1) - horaPMin2(H x2 y2) )

-- f) adicionar um determinado número de minutos a uma dada hora.

acresHora2 :: Hora1 -> Int -> Hora1
acresHora2 (H x1 y1) min = minPHora2 ( horaPMin2(H x1 y1) + min )

{-
5. Considere o seguinte tipo de dados para representar os possíveis estados de um semáforo:
  data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
-}

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) Defina a função next :: Semaforo -> Semaforo que calcula o próximo estado de um semáforo.

next :: Semaforo -> Semaforo
next sem
  | sem == Verde = Amarelo
  | sem == Amarelo = Vermelho
  | sem == Vermelho = Verde

{-
next :: Semaforo -> Semaforo
next s = case s of Verde -> Amarelo
                   Amarelo -> Vermelho
                   Vermelho -> Verde
-}

-- b) Defina a função stop :: Semaforo -> Bool que determina se é obrigatório parar num semáforo.

stop :: Semaforo -> Bool
stop sem
  | sem == Vermelho = True
  | otherwise = False

-- c) Defina a função safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois semáforos num cruzamento é seguro.

safe :: Semaforo -> Semaforo -> Bool
safe sem1 sem2
  | ( ( (sem1==Verde || sem1==Amarelo) && sem2==Vermelho ) && ( (sem2==Verde || sem2==Amarelo) && sem1==Vermelho) ) = True
  | otherwise = False


  {-
  6. Um ponto num plano pode ser representado por um sistema de coordenadas
  Cartesiano (distâncias aos eixos vertical e horizontal) ou por um sistema de coordenadas
  Polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
          data Ponto = Cartesiano Double Double | Polar Double Double
                      deriving (Show,Eq)
  Com este tipo o ponto Cartesiano (-1) 0 pode alternativamente ser representado por Polar 1 pi.
  -}

-- a) posx :: Ponto -> Double que calcula a distância de um ponto ao eixo vertical.

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a) = d * (cos a)


-- b) posy :: Ponto -> Double que calcula a distância de um ponto ao eixo horizontal.

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d * (sin a)

-- c) raio :: Ponto -> Double que calcula a distância de um ponto à origem.

raio :: Ponto -> Double
raio (Polar d a) = d
raio (Cartesiano x y) = sqrt (x^2 + y^2)

-- d) angulo :: Ponto -> Double que calcula o ângulo entre o vector que liga a origem ao ponto e o eixo horizontal.

angulo :: Ponto -> Double
angulo (Polar d a) = a
angulo (Cartesiano x y) = atan (x/y)

-- e) dist :: Ponto -> Ponto -> Double que calcula a distância entre dois pontos.

distpontos :: Ponto -> Ponto -> Double
distpontos p1 p2 = sqrt ((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

{-
7. Considere o seguinte tipo de dados para representar figuras geométricas num plano.

  data Figura = Circulo Ponto Double
              | Rectangulo Ponto Ponto
              | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

O tipo de dados diz que uma figura pode ser um círculo centrado num determinado ponto
e com um determinado raio, um rectângulo paralelo aos eixos representado por dois pontos
que são vértices da sua diagonal, ou um triângulo representado pelos três pontos dos seus vértices.
-}

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- a) Defina a função poligono :: Figura -> Bool que testa se uma figura é um polígono.

poligono :: Figura -> Bool
poligono (Circulo p r) = if r<=0 then False else True
poligono (Rectangulo p1 p2) = if (posx p1 == posx p2 || posy p1 == posy p2) then False else True
poligono (Triangulo p1 p2 p3) = (a < b + c && b < a + c && c < a + b)
                                  where a = distpontos p1 p2
                                        b = distpontos p2 p3
                                        c = distpontos p1 p3

-- b) Defina a função vertices :: Figura -> [Ponto] que calcula a lista dos vértices de uma figura.

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo p1 p2) = [p1, (Cartesiano (posx p2) (posy p1)), p2, (Cartesiano (posx p1) (posy p2))]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

-- VER MELHOR ESTA ------- PERGUNTAR PROFFFFFFFFFFFFFF

-- c) Complete a seguinte definição cujo objectivo é calcular a área de uma figura:

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distpontos p1 p2
        b = distpontos p2 p3
        c = distpontos p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Rectangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)

-- d) Defina a função perimetro :: Figura -> Double que calcula o perímetro de uma figura.

perimetro2 :: Figura -> Double
perimetro2 (Circulo p r) = 2*pi*r
perimetro2 (Rectangulo p1 p2) = 2*(abs ((posx p2) - (posx p1))) + 2*(abs ((posy p2) - (posy p1)))
perimetro2 (Triangulo p1 p2 p3) = (distpontos p1 p2) + (distpontos p2 p3) + (distpontos p1 p3)

{-
8. Utilizando as funções ord :: Char -> Int e chr :: Int -> Char do módulo Data.Char, defina as seguintes funções:

(a) isLower :: Char -> Bool, que testa se um Char é uma minúscula.
(b) isDigit :: Char -> Bool, que testa se um Char é um dígito.
(c) isAlpha :: Char -> Bool, que testa se um Char é uma letra.
(d) toUpper :: Char -> Char, que converte uma letra para a respectiva maiúscula.
(e) intToDigit :: Int -> Char, que converte um número entre 0 e 9 para o respectivo dígito.
(f) digitToInt :: Char -> Int, que converte um dígito para o respectivo inteiro.

Note que todas estas funções já estão também pré-definidas nesse módulo.
-}

-- a)

myIsLower :: Char -> Bool
myIsLower c = (ord c >= ord 'a' && ord c <= ord 'z')

-- b)

myIsDigit :: Char -> Bool
myIsDigit c = (ord c >= ord '0' && ord c <= ord '9')

-- c)

myIsAlpha :: Char -> Bool
myIsAlpha c = (myIsLower c || myIsUpper c)
  where myIsUpper c = (ord c >= ord 'A' && ord c <= ord 'Z')

-- d)

myToUpper :: Char -> Char
myToUpper c
 | myIsLower c = chr ((ord c) - 32)  -- 32 é o resultado de ( ord 'a' - ord 'A' )
 | otherwise = c

-- e)

myIntToDigit :: Int -> Char
myIntToDigit i
 | i <= 9 = chr (i + 48)             -- 48 é o resultado de ( ord '0' - 0 )
 | otherwise = error (show i ++ " is not a Digit")

-- f)

myDigitToInt :: Char -> Int
myDigitToInt c
 | myIsDigit c = (ord c) - 48
 | otherwise = error (show c ++ " is not a Digit")
