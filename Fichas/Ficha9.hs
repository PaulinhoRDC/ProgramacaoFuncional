module Ficha9 where

import System.Random
import System.IO
import System.IO.Error


{-
1. A classe Random da biblioteca System.Random agrupa os tipos para os quais é possível gerar valores aleatórios.
 Algumas das fuções declaradas nesta classe são:

• randomIO :: Random a => IO a que gera um valor aleatório do tipo a;
• randomRIO :: Random a => (a,a) -> IO a que gera um valor aleatório do tipo a dentro de uma determinada gama de valores.

Usando estas funções implemente os seguintes programas:

(a) bingo :: IO () que sorteia os números para o jogo do bingo.
 Sempre que uma tecla é pressionada é apresentado um número aleatório entre 1 e 90.
 Obviamente, não podem ser apresentados números repetidos e o programa termina depois de gerados os 90 números diferentes.
-}

-- usando o splitAt, que faz: splitAt 3 [5,6,7,8,9,13] -> ([5,6,7],[8,9,13])

bingo :: IO ()
bingo = do putStrLn "======= BINGO ======="
           tiraNum 90 [1..90]                -- 90 serve para sabermos quantos numeros ainda estão na lista

tiraNum :: Int -> [Int] -> IO ()
tiraNum 0 _ = putStrLn "FIM"                                    -- Para a lista vazia
tiraNum n l = do putStr "Prima ENTER para tirar um número!"
                 wait <- getChar
                 i <- randomRIO (0, n-1)
                 let (l1,x:l2) = splitAt i l
                 print x
                 tiraNum (n-1) (l1++l2)   -- (l1++l2) é a lista sem o elemento que foi retirado


{-
(b) mastermind :: IO () que implementa uma variante do jogo de descodificação de padrões Mastermind.
 O programa deve começar por gerar uma sequência secreta de 4 dígitos aleatórios que o jogador vai tentar descodificar.
 Sempre que o jogador introduz uma sequência de 4 dígitos,
o programa responde com o número de dígitos com o valor correcto na posição correcta
e com o número de dígitos com o valor correcto na posição errada.
 O jogo termina quando o jogador acertar na sequência de dígitos secreta.
-}

{-    EM TESTE

mastermind :: IO ()
mastermind = do (n1,n2,n3,n4) <- getKey
                doGuess (n1,n2,n3,n4)
                return ()


getKey :: IO (Int,Int,Int,Int)
getKey = do a <- randomRIO (0,9)
            b <- randomRIO (0,9)
            c <- randomRIO (0,9)
            d <- randomRIO (0,9)
            return (a,b,c,d)

getGuess :: IO (Int,Int,Int,Int)
getGuess = do x <- getLine
              if length x /= 4 || (not (all isDigit x))
              then getGuess
              else return (let (a:b:c:d:resto) = x in (read [a],read [b],read [c],read [d]))

doGuess :: (Int,Int,Int,Int) -> IO ()
doGuess (n1,n2,n3,n4) = do
    let listaNums = [n1,n2,n3,n4]
    (g1,g2,g3,g4) <- getGuess
    let numsC = 0 + (if n1 == g1 then 1 else 0) + (if n2 == g2 then 1 else 0) + (if n3 == g3 then 1 else 0) + (if n4 == g4 then 1 else 0)
    let numsS = 0 + (if n1 /= g1 && g1 `elem` (listaNums \\ [g2,g3,g4]) then 1 else 0) +
                    (if n2 /= g2 && g2 `elem` (listaNums \\ [g3,g4]) then 1 else 0) +
                    (if n3 /= g3 && g3 `elem` (listaNums \\ [g4]) then 1 else 0) +
                    (if n4 /= g4 && g4 `elem` (listaNums \\ []) then 1 else 0)
    if numsC == 4 then print "Ganhaste, parabens!" else print $ "Valores corretos: " ++ show numsC ++ "   Valores no sitio errado: " ++ show numsS
    if numsC == 4 then return () else doGuess (n1,n2,n3,n4)

-}


{-
2. Uma aposta do EuroMilhões corresponde à escolha de 5 Números e 2 Estrelas.
Os Números são inteiros entre 1 e 50. As Estrelas são inteiros entre 1 e 9.
Para modelar uma aposta destas definiu-se o seguinte tipo de dados:
                                                                        data Aposta = Ap [Int] (Int,Int)

(a) Defina a função valida :: Aposta -> Bool que testa se uma dada aposta é válida
(i.e. tem os 5 números e 2 estrelas, dentro dos valores aceites e não tem repeticões).
-}

data Aposta = Ap [Int] (Int,Int)
  deriving Show

valida :: Aposta -> Bool
valida (Ap l (a,b)) = (1<= a && a<=9) && (1<=b && b<=9)  && (a/=b) && (validaNums l && length l == 5)

validaNums :: [Int] -> Bool
validaNums (x:xs) = (1<=x && x<=50) && not (elem x xs) && validaNums xs
validaNums [] = True

{-
(b) Defina a função comuns :: Aposta -> Aposta -> (Int,Int) que dada uma aposta e uma chave,
calcula quantos números e quantas estrelas existem em comum nas duas apostas.
-}

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap ns (a,b)) (Ap xs (c,d)) = (numComs' ns xs, numComs' [a,b] [c,d])

numComs :: [Int] -> [Int] -> Int
numComs (x:xs) l = if x `elem` l
                   then 1 + numComs xs l
                   else numComs xs l
numComs [] _ = 0

-- Ou em ordem superior : (FOLDR) !!!

numComs' l1 l2 = foldr (\x r-> if x `elem` l2 then 1+r else r) 0 l1

{-
(c) Use a função da alínea anterior para:

i. Definir Aposta como instância da classe Eq.

ii. Definir a função premio :: Aposta -> Aposta -> Maybe Int ,que dada uma aposta e a chave do concurso,
indica qual o prémio que a aposta tem.
Os prémios do EuroMilhões são:

                                Números Estrelas Prémio   Números Estrelas Prémio
                                  5       2        1        3         2       7
                                  5       1        2        2         2       8
                                  5       0        3        3         1       9
                                  4       2        4        3         0      10
                                  4       1        5        1         2      11
                                  4       0        6        2         1      12
                                                            2         0      13
-}

-- (i)

instance Eq Aposta where
  ap1 == ap2 = comuns ap1 ap2 == (5,2)

-- (ii)

premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case (comuns ap ch) of
                  (5,2) -> Just 1
                  (5,1) -> Just 2
                  (5,0) -> Just 3
                  -- (...)
                  (2,0) -> Just 13
                  _     -> Nothing

{-
(d) Para permitir que um apostador possa jogar de forma interactiva:

  i. Defina a função leAposta :: IO Aposta que lê do teclado uma aposta. Esta função deve garantir que a aposta produzida é válida.

  ii. Defina a função joga :: Aposta -> IO () que recebe a chave do concurso, lê uma aposta do teclado e imprime o prémio no ecrã.
-}

-- (i)

leAposta :: IO Aposta
leAposta = do putStr "Lista de números (1..50): "
              l <- getLine
              putStr "Par de estrelas (1..9): "
              s <- getLine
              let ap = Ap (read l) (read s)    -- read :: Read a => String -> a
              if valida ap
              then return ap                                                                -- return :: a -> IO a
              else do putStrLn "Inválida"      -- >> significa faz a ação e depois a outra
                      leAposta                 -- mesmo que, putStrLn "Inválida" >> leAposta

-- (ii)

joga :: Aposta -> IO ()
joga ch = do ap <- leAposta
             case (premio ap ch) of
               Just x -> putStrLn ("Tem o prémio: " ++ (show x))
               Nothing -> putStrLn "Não tem prémio"



{-
(e) Defina a função geraChave :: IO Aposta, que gera uma chave válida de forma aleatória.
-}

geraChave :: IO Aposta
geraChave = do num <- geraNums 5 []
               est <- geraEstrelas
               return (Ap num est)

geraNums :: Int -> [Int] -> IO [Int]
geraNums n l
    | n==0 = return l
    | n>0  = do x <- randomRIO (1,50)
                if (elem x l)
                  then geraNums n l
                  else geraNums (n-1) (x:l)

geraEstrelas :: IO (Int,Int)
geraEstrelas = do a <- randomRIO (1,9)
                  b <- randomRIO (1,9)
                  if (a==b)
                    then geraEstrelas
                    else return (a,b)

{-
(f) Pretende-se agora que o programa main permita jogar várias vezes e dê a possiblidade de simular um novo concurso (gerando uma nova chave).
Complete o programa definindo a função ciclo :: Aposta -> IO ().

main :: IO ()
   main = do ch <- geraChave
            ciclo ch

menu :: IO String
menu = do { putStrLn menutxt
             ; putStr "Opcao: "
             ; c <- getLine
             ; return c
             }
      where menutxt = unlines ["",
                              "Apostar ........... 1",
                              "Gerar nova chave .. 2",
                              "",
                              "Sair .............. 0"]
-}

main :: IO ()
main = do ch <- geraChave
          ciclo ch

menu :: IO String
menu = do { putStrLn menutxt
             ; putStr "Opcao: "
             ; c <- getLine
             ; return c
             }
      where menutxt = unlines ["",
                              "Apostar ........... 1",
                              "Gerar nova chave .. 2",
                              "",
                              "Sair .............. 0"]

ciclo :: Aposta -> IO ()
ciclo ch = do op <- menu
              case op of
                ('1': _) -> do joga ch
                               ciclo ch
                ('2': _) -> do x <- geraChave
                               ciclo x
                ('0': _) -> return ()
