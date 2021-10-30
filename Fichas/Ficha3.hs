module Ficha1 where

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

-- (a) Testar se uma etapa está bem construída (i.e., o tempo de chegada é superior ao de partida e as horas são válidas).

{- (b) Testa se uma viagem está bem construída
(i.e., se para cada etapa, o tempo de chegada é superior ao de partida,
e se a etapa seguinte começa depois da etapa anterior ter terminado).
-}

-- (c) Calcular a hora de partida e de chegada de uma dada viagem.

-- (d) Dada uma viagem válida, calcular o tempo total de viagem efectiva.

-- (e) Calcular o tempo total de espera.

-- (f) Calcular o tempo total da viagem (a soma dos tempos de espera e de viagem efectiva).

{-
2. Considere as seguinte definição de um tipo para representar linhas poligonais.
        type Poligonal = [Ponto]
O tipo Ponto é idêntico ao definido na Ficha 1.
Nas resolução das alíneas seguintes pode utilizar funções definidas nessa ficha.
-}
