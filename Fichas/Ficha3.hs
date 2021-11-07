module Ficha3 where

import  Data.Char



{-
1. Assumindo que uma hora é representada por um par de inteiros, uma viagem pode ser representada por uma sequência de etapas,
onde cada etapa é representada por um par de horas (partida, chegada):
        data Hora = H Int Int
                  deriving Show
        type Etapa = (Hora,Hora)
        type Viagem = [Etapa]

Por exemplo, se uma viagem for
         [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]
significa que teve três etapas:
• a primeira começou às 9 e meia e terminou às 10 e 25;
• a segunda começou às 11 e 20 e terminou à uma menos um quarto;
• a terceira começou às 1 e meia e terminou às 3 menos um quarto;

Para este problema, vamos trabalhar apenas com viagens que começam e acabam no mesmo dia.
Utilizando as funções sobre horas que definiu na Ficha 1, defina as seguintes funções:
-}

data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- (a) Testar se uma etapa está bem construída (i.e., o tempo de chegada é superior ao de partida e as horas são válidas).

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (h1,h2) = horaValida h1 && horaValida h2 && h2 `horaPosterior`h1

{- (b) Testa se uma viagem está bem construída
(i.e., se para cada etapa, o tempo de chegada é superior ao de partida,
e se a etapa seguinte começa depois da etapa anterior ter terminado).
-}

viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [(h1,m1)] = etapaBemConstruida (h1,m1)
viagemBemConstruida ((h1,m1):(h2,m2):t) = etapaBemConstruida (h1,m1) && etapaBemConstruida (h2,m2) && viagemBemConstruida ((h2,m2):t)

-- (c) Calcular a hora de partida e de chegada de uma dada viagem.

partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada [(h1,h2)] = (h1,h2)
partidaEChegada ((h1,h2):(h3,h4):t) = partidaEChegada ((h1,h4):t)

-- (d) Dada uma viagem válida, calcular o tempo total de viagem efectiva.

tempoDeViagem :: Viagem -> Hora
tempoDeViagem [(h1,h2)] = difHora h1 h2
tempoDeViagem ((h1,h2):et) = acresHora (difHora h1 h2) (horaPMin(tempoDeViagem et))

-- (e) Calcular o tempo total de espera.

tempoDeEspera :: Viagem -> Hora
tempoDeEspera [(h1,h2)] = H 0 0
tempoDeEspera ((h1,h2):(h3,h4):et) = acresHora (difHora h2 h3) (horaPMin (tempoDeEspera ((h3,h4):et)))

-- (f) Calcular o tempo total da viagem (a soma dos tempos de espera e de viagem efectiva).

tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = acresHora (tempoDeViagem v) (horaPMin (tempoDeEspera v))

{-
2. Considere as seguinte definição de um tipo para representar linhas poligonais.
        type Poligonal = [Ponto]
O tipo Ponto é idêntico ao definido na Ficha 1.
Nas resolução das alíneas seguintes pode utilizar funções definidas nessa ficha.
-}

type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)


-- a) Defina a função para calcular o comprimento de uma linha poligonal

comprimento :: Poligonal  -> Double
comprimento p = case p of [] -> 0
                          [_] -> 0
                          (a:b:t) -> dist a b + comprimento (b:t)

-- b) Defina uma função para testar se uma dada linha poligonal é ou não fechada.

linhaFechada :: Poligonal -> Bool
linhaFechada p = length p >= 3 && head p == last p

-- c) Defina a função triangula :: Poligonal -> [Figura] que,
-- dada uma linha poligonal fechada e convexa, calcule uma lista de triângulos
-- cuja soma das áreas seja igual à àrea delimitada pela linha poligonal.
-- O tipo Figura é idêntico ao definido na Ficha 1.

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:ps) = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

-- d) Defina uma função para calcular a área delimitada por uma linha poligonal fechada e convexa.

areaPol :: Poligonal -> Double
areaPol p = sum [area t | t <- triangula p]

-- e) Defina a função mover :: Poligonal -> Ponto -> Poligonal que,
-- dada uma linha poligonal e um ponto, dá como resultado uma linha poligonal
-- idêntica à primeira mas tendo como ponto inicial o ponto dado.

mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p : pol

-- f) Defina a função zoom :: Double -> Poligonal -> Poligonal que,
-- dada um factor de escala e uma linha poligonal, dê como resultado uma linha poligonal
-- semelhante e com o mesmo ponto inicial mas em que o comprimento de cada segmento de recta é multiplicado pelo factor dado.

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1,p2] = [p1,multP z p2]
zoom z (p1:p2:pol) = p1 : zoom z (p2' : pol)
    where p2' = multP z p2
zoom _ p = p

multP :: Double -> Ponto -> Ponto
multP z p = Cartesiano (z * posx p) (z * posy p)

{-
3) Para armazenar uma agenda de contactos telefónicos e de correio electrónico definiram-se os seguintes tipos de dados.
   Não existem nomes repetidos na agenda e para cada nome existe uma lista de contactos.

      data Contacto = Casa Integer
                    | Trab Integer
                    | Tlm Integer
                    | Email String
              deriving Show

      type Nome = String
      type Agenda = [(Nome, [Contacto])]
-}

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a) Defina a função acrescEmail :: Nome -> String -> Agenda -> Agenda que,
-- dado um nome, um email e uma agenda, acrescenta essa informação à agenda.

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n, [Email e])]
acrescEmail n e ((n2,l):xs) | (n==n2) = (n2,l:(Email e)) : xs
                           | otherwise = (n2,l) : acrescEmail n e xs

-- b) Defina a função verEmails :: Nome -> Agenda -> Maybe [String] que, dado um nome e uma agenda,
-- retorna a lista dos emails associados a esse nome.
-- Se esse nome não existir na agenda a função deve retornar Nothing.

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):xs) | (n==x) = Just (daListaStr l)
                       | otherwise = verEmails n xs

daListaStr :: [Contacto] -> [String]
daListaStr [] = []
daListaStr (h:t) = case h of Email s -> s: daListaStr t
                             _ -> daListaStr t

-- c) Defina a função consTelefs :: [Contacto] -> [Integer] que,
-- dada uma lista de contactos, retorna a lista de todos os números de telefone dessa lista (tanto telefones fixos como telemóveis).

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of Casa s -> s: consTelefs t
                             Trab s -> s: consTelefs t
                             Tlm s -> s: consTelefs t
                             _ -> consTelefs t

-- d) Defina a função casa :: Nome -> Agenda -> Maybe Integer que,
-- dado um nome e uma agenda, retorna o número de telefone de casa (caso exista).

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa n ((x,l): t) | (n==x) = numCasa l
                  | otherwise = casa n t


numCasa :: [Contacto] -> Maybe Integer
numCasa [] = Nothing
numCasa (h:t) = case h of Casa s -> Just s
                          _ -> numCasa t


{-
4) Pretende-se guardar informação sobre os aniversários das pessoas numa tabela
   que associa o nome de cada pessoa à sua data de nascimento. Para isso, declarou-se a seguinte estrutura de dados:
                type Dia = Int
                type Mes = Int
                type Ano = Int
                type Nome = String

                data Data = D Dia Mes Ano
                    deriving Show

                type TabDN = [(Nome,Data)]
-}

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

-- a) Defina a função procura :: Nome -> TabDN -> Maybe Data
-- que indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome [(n,d):t] = if nome == n
  then Just d
  else procura nome t

-- b) Defina a função idade :: Data -> Nome -> TabDN -> Maybe Int
-- que calcula a idade de uma pessoa numa dada data.

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade dataa@(D dx mx ax) nome ((n,D d m a):ts) = if nome == n then if mx > m || mx == m && dx > d
  then Just (ax - a)
  else Just ((ax - a) - 1)
  else idade dataa nome ts

-- c)  Defina a função anterior :: Data -> Data -> Bool que testa se uma data é anterior a outra data.

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d2 m2 a2) = ( a<a2 || (a==a2 && (m<m2 || ((m==m2) && d<d2))) )

-- d) Defina a função ordena :: TabDN -> TabDN
-- que ordena uma tabela de datas de nascimento por ordem crescente das datas de nascimento.

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insere (n,d) (ordena ts)
    where insere (n,d) [] = [(n,d)]
          insere (n,d) ((nh,dh):t) | anterior dh d = (nh,dh):insere (n,d) t
                                   | otherwise = (n,d):(nh,dh):t

-- e) e) Defina a função porIdade:: Data -> TabDN -> [(Nome,Int)]
-- que apresenta o nome e a idade das pessoas, numa dada data, por ordem crescente da idade das pessoas.

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) tabela = (n,idade) : porIdade (D d m a) ts
    where ((n,D dx mx ax):ts) = ordena tabela
          idade = if m > mx || mx == m && d > dx then a - ax else (a - ax) - 1

{-
5) Considere o seguinte tipo de dados que descreve a informação de um extracto bancário. Cada valor deste tipo indica o saldo inicial e uma lista de movimentos. Cada movimento é representado por um triplo que indica a data da operação, a sua descrição e a quantia movimentada (em que os valores são sempre números positivos).
        data Movimento = Credito Float | Debito Float
          deriving Show

        data Data = D Int Int Int
          deriving Show

        data Extracto = Ext Float [(Data, String, Movimento)]
          deriving Show
-}

data Movimento = Credito Float | Debito Float
  deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
  deriving Show

-- a) Construa a função extValor :: Extracto -> Float -> [Movimento]
-- que produz uma lista de todos os movimentos (créditos ou débitos) superiores a um determinado valor.

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext s l) n = daMov l n

daMov :: [(Data, String, Movimento)] -> Float -> [Movimento]
daMov [] _ = []
daMov ((_,_, Credito x): t) n = if (x>n)
  then (Credito x) : daMov t n
  else daMov t n
daMov ((_,_, Debito x): t) n = if (x>n)
  then (Debito x) : daMov t n
  else daMov t n

-- b) Defina a função filtro :: Extracto -> [String] -> [(Data,Movimento)]
-- que retorna informação relativa apenas aos movimentos cuja descrição esteja incluída na lista fornecida no segundo parâmetro.

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext s l) n = filtroAux l n

filtroAux :: [(Data, String, Movimento)] -> [String] -> [(Data,Movimento)]
filtroAux [] _ = []
filtroAux ((d,s,m): t) x = if (s `elem` x)
  then (d,m): filtroAux t x
  else filtroAux t x


-- c) Defina a função creDeb :: Extracto -> (Float,Float)
-- que retorna o total de créditos e de débitos de um extracto no primeiro e segundo elementos de um par, respectivamente.

creDeb :: Extracto -> (Float,Float)
creDeb (Ext s l) = daPar l

daPar l :: [(Data, String, Movimento)] -> (Float,Float)
daPar [] = (0,0)
daPar ((_,_, Credito x):t) = (x+c, d)
daPar ((_,_, Debito x):t) = (c, x+d)
          where (c,d) = daPar t

-- COM ACUMULADOR SERIA DESTA FORMA:

creDeb2 :: Extracto -> (Float, Float)
creDeb2 (Ext s l) = creDebAC l (0,0)

creDebAC :: [(Data, String, Movimento)] -> (Float,Float) -> (Float, Float)
creDebAC [] (c,d) = (c,d)
creDebAC ((_,_, Credito x): t) (c,d) = creDebAC (c+x,d)
creDebAC ((_,_, Debito x): t) (c,d) = creDebAC (c,d+x)


-- d) Defina a função saldo :: Extracto -> Float
-- que devolve o saldo final que resulta da execução de todos os movimentos no extracto sobre o saldo inicial.

saldo :: Extracto -> Float
saldo (Ext s l) = let (c,d) = creDeb (Ext s l)
                  in s + c - d

-- OU

saldo2 :: Extracto -> Float
saldo2 e@(Ext s l) = let (c,d) = creDeb e
                  in s + c - d
