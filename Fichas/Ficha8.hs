module Ficha8 where

import  Data.Char

{-
1. Considere o seguinte tipo de dados para representar frações:
                                                                  data Frac = F Integer Integer

(a) Defina a função normaliza :: Frac -> Frac, que dada uma fração calcula uma fração equivalente, irredutível, e com o denominador positivo.
    Por exemplo, normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5)) deve retornar F (-10) 1.
    Sugere-se que comece por definir primeiro a função mdc :: Integer -> Integer -> Integer ,
  que calcula o máximo divisor comum entre dois números, baseada na seguinte propriedade (atribuida a Euclides):
                                                                  mdc x y == mdc (x+y) y == mdc x (y+x)
-}

data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x y |(x==y) = x
        |(x>y) = mdc (x-y) y
        |(x<y) = mdc x (y-x)

normaliza :: Frac -> Frac
normaliza (F x y)   |(x>=0 && y>0) = F (div x d) (y `div` d)
                    |(x<0 && y<0)  = F ((abs x) `div` d) ((abs y) `div` d)
                    |otherwise     = F (-((abs x) `div` d)) ((abs y) `div` d)
        where d = mdc (abs x) (abs y)

{-
(b) Defina Frac como instância da classe Eq.
-}

instance Eq Frac where
    (F a b) == (F c d) = a * d == c * b

{-
(c) Defina Frac como instância da classe Ord.
-}

instance Ord Frac where
    (F a b) <= (F c d) = a * d <= c * b

{-
(d) Defina Frac como instância da classe Show, de forma a que cada fração seja
apresentada por (numerador/denominador).
-}

instance Show Frac where
    show (F a b) = "(" ++ (show a) ++ "/" ++ (show b) ++ ")"

{-
(e) Defina Frac como instância da classe Num. Relembre que a classe Num tem a
seguinte definição
                          class (Eq a, Show a) => Num a where
                              (+), (*), (-) :: a -> a -> a
                              negate, abs, signum :: a -> a
                              fromInteger :: Integer -> a
-}

instance Num Frac where
    (F a b) + (F c d) = F (a*d + c*b) (b*d)     -- (+)
    x - y = x + negate y                        -- (-)
    (F a b) * (F c d) = F (a * c) (b * d)       -- (*)

    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = F 0 1
                   | (a * b > 0) = F 1 1        -- ((a<0 && b<0) || (a>0 && b>0))
                   | otherwise = F (-1) 1

    fromInteger n = F n 1

{-
(f) Defina uma função que, dada uma fração f e uma lista de frações l,
  selecciona de l os elementos que são maiores do que o dobro de f.
-}

maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro f l = filter ((>) (2 * f)) l

{-
2. Relembre o tipo definido na Ficha 7 para representar expressões inteiras.
   Uma possível generalização desse tipo de dados, será considerar expressões cujas constantes são de um qualquer tipo numérico
   (i.e., da classe Num).
                            data Exp a = Const a
                                        | Simetrico (Exp a)
                                        | Mais (Exp a) (Exp a)
                                        | Menos (Exp a) (Exp a)
                                        | Mult (Exp a) (Exp a)
-}

data Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

{-
(a) Declare Exp a como uma instância de Show.
-}

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

{-
(b) Declare Exp a como uma instância de Eq.
-}

calcula :: (Num a) => Exp a -> a
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

instance (Num a,Eq a) => Eq (Exp a) where
    x == y = calcula x == calcula y


{-
(c) Declare Exp a como instância da classe Num.
-}

instance (Num a, Ord a) => Num (Exp a) where

    (+) = Mais
    (-) = Menos
    (*) = Mult

    -- abs :: Num a => Exp a -> Exp a
    abs e | (calcula e >= 0) = e
          | otherwise = Simetrico e

    -- signum :: Num a => Exp a -> Exp a
    signum e |calcula e == 0 = Const 0
             |calcula e > 0  = Const 1
             |calcula e < 0  = Const (-1)

    -- fromInteger :: Num a => Integer -> Exp a
    fromInteger n = Const (fromInteger n)

{-
3. Relembre o exercício da Ficha 3 sobre contas bancárias, com a seguinte declaração de tipos

                        data Movimento = Credito Float | Debito Float
                        data Data = D Int Int Int
                        data Extracto = Ext Float [(Data, String, Movimento)]
-}

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

{-
(a) Defina Data como instância da classe Ord.
-}

instance Eq Data where
  (D dia1 mes1 ano1) == (D dia2 mes2 ano2) = (ano1==ano2) && (mes1==mes2) && (dia1==dia2)

instance Ord Data where
  (D dia1 mes1 ano1) <= (D dia2 mes2 ano2) = ( (ano1 < ano2) || ((ano1==ano2) && (mes1 < mes2)) || ((ano1==ano2) && (mes1 == mes2) && (dia1<=dia2)) )

{-
  compare (D dia1 mes1 ano1) (D dia2 mes2 ano2)
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT
-}

{-
(b) Defina Data como instância da classe Show.
-}

instance Show Data where
    show (D dia mes ano) = "[" ++ show dia ++ "/" ++ show mes ++ "/" ++ show ano ++ "]"

{-
(c) Defina a função ordena :: Extracto -> Extracto,
que transforma um extracto de modo a que a lista de movimentos apareça ordenada por ordem crescente de data.
-}

{-
ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sort l)
-}

{-
(d) Defina Extracto como instância da classe Show,
 de forma a que a apresentação do extracto seja por ordem de data do movimento com o seguinte, e com o seguinte aspecto:

                            Saldo anterior: 300
                            ---------------------------------------
                            Data       Descricao   Credito   Debito
                            ---------------------------------------
                            2010/4/5   DEPOSITO     2000
                            2010/8/10  COMPRA                 37,5
                            2010/9/1   LEV                    60
                            2011/1/7   JUROS        100
                            2011/1/22  ANUIDADE               8
                            ---------------------------------------
                            Saldo actual: 2294,5
-}

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n; Debito n -> acc - n) x lm

instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,_) -> show dat ++ replicate (11 - length (show dat)) ' ' ++ map toUpper str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo atual: " ++ show (saldo (Ext n l))
