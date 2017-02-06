module Main where
import Data.Char
import Data.List (transpose)

import System.IO  (stdin, stdout, hSetEcho, hSetBuffering
                  ,BufferMode(..))
-- | LIGHTBOT

-- | A - ANDAR
-- | L - LUZ
-- | E - ESQUERDA
-- | D - DIREITA
-- | S - SALTAR

-- | EXEMPLO DE NIVEL:
-- | LETRA GRANDE DEFINE O SITIO DA LAMPADA
-- | CMONADOS EM LETRAS MAUISCULA
-- | PRIMEIRA LINHA DEFINE O TAMANHO DO TABULEIRO
{-

   |  2 0| b b C-> 2 2
   |  1 0| B a a
   |  0 0| a a A
   |     --------
        
   |     N A (North)
   |      S V (South)
   |       E -> (Este)
   |        O <- (Oeste)
-}

{-
EXEMPLO TERMINAL
aacdD
aacaa
bbCaa
0 1 S (0->x, 1->y, S->Sul)
SEASLEAADSAL
-}

{-
ATE DIA 30 DE NOVEMBRO:



SVN tem de ter os seguintes ficheiros:

 - Readme.txt
 - docs/documentaçao gerada pelo haddock
 - src/ -> codigo haskell
 - tex/ -> codigo fonte do relatorio 
 - tests/ -> tabuleiro de tests
-}

-- SVN -> PLATAFORMA PARA TRABALHO EM GRUPO:

-- svn checkout

-- svn checkout svn://svnpelaeduroam.alunos.di.uminho.pt/li1g089 --username=li1g089a2

-- svn co svn://svnpelaeduroam.alunos.di.uminho.pt/li1g089 --username=li1g089
----------------------------------------------------------------------------------------
-- COMANDOS PARA LER O FICHEIRO DE TEXTO NO TERMINAL:
-- let linhas = lines texto
----------------------------------------------------------------------------------------
-- LATEX:
-- tipos de documentos: article, book, letter, beamer


-- | 1 Validação de tabuleiro
-- |  verificar se o tabuleiro só contém letras 
-- |  verificar so o numero de colunas é igual em todas as linhas
--   
-- | 2 validar coordenadas
-- |  verificar se os dois primeiros elementos das coordenadas são nÚmeros inteiros
-- |  verificar se o terceiro elemento é uma letras e se corresponde ao conjunto ["N","S","E","O"]
-- |  verificar se a primeiro e segundo elementos de coordenadas sÃo maiores que zero
-- |  verificar se o primeiro elemento é menor igual ao numero de colunas
-- |  verificar se o segundo elemento é menor igual ao numero de linhas

-- | 3 validar comandos
-- |  verificar se o comando é valido, isto é verificar se é letras e se é contido no conjunto  ['A' 'S' 'E' 'D' 'L']

-- | 4 Função da tarefa:
-- |  define - se variável altura inicial que corresponde ao numero de linhas do tabuleiro
-- |  segunda variável tab vai corresponder ao tabuleiro propriamente dito ou seja tabuleiro completo menos as coordenadas e menos os comandos
-- |  vamos definir vTab que vai verificar se a linha é valida se é passa para verificar a linha seguinte, caso não passar no teste de validação retorna um erro on mostra o numero da linhas
-- |  completando a validação do tabuleiro passamos a testar as coordenadas usando a função externa descrita no ponto 1
-- |  a seguir testamos os comandos usando a função descrita no ponto 2
-- |  ao passar pelos testes todos o resultado retorna a linha que contém o erro ou no caso de passar nos testes retorna ["OK"]
------------------------------------------------------------------------------------------------------------------------------------------------------
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t


-- | funçao principal (tarefa)
main = do inp <- getContents
          putStr (outStr(tarefa(lines inp)))


-- | verifica se o tabuleiro cumpre os parametros estabelecidos no enunciado com ajuda de funçoes auxiliares
tarefa :: [String] -> [String]
tarefa [] = ["1"]
tarefa t = let alturaInicial = length t 
               tab = getBoard (t)
               (vTab, l) = validaTab (tab)
           in if vTab then [show l]
              else let altura = length tab
                   in if alturaInicial < altura + 1 then [show (altura+1)] 
                      else let lCoor  =  show(getNumColunas t) ++ " " ++ show(getNumLinhas t) ++ " " ++ t !! altura -- agrupa numero de linhas, numero de colunos e a string das coordenadas
                               vCoor  = validaCoor lCoor
                           in if vCoor then [show (altura+1)]
                              else let lComand  = t !! (altura + 1)
                                       vComand  = validaComandos lComand
                                   in if vComand then [show (altura+2)]
                                      else if (alturaInicial - (length tab)) > 2 then [show (length tab + 3)]
                                           else ["OK"]



-- | valida linhas com coordenadas
-- | retorna true em caso de erro
validaCoor :: String -> Bool
validaCoor s = let coords = words s
               in if length coords == 5
                                       && ehNumero (coords !! 2) 
                                       && ehNumero (coords !! 3)
                                       && (read (coords !! 2)::Int) >= 0 
                                       && (read (coords !! 2)::Int) <=  (read(coords !! 0)::Int) - 1
                                       && (read (coords !! 3)::Int) >= 0 
                                       && (read (coords !! 3)::Int) <=  (read(coords !! 1)::Int) - 1
                                       && elem (last coords) ["N","S","E","O"]
                  then (False)
                  else (True)




ehNumero :: String -> Bool
ehNumero [] = True
ehNumero (h:t) | h >= '0' && h <= '9' = ehNumero t
               | otherwise = False 

-- | verifica se é espaço
ehEsp :: Char -> Bool
ehEsp s = s ==' '


-- | verifica se é um comando valido
validaComandos :: String -> Bool
validaComandos s = if ehComandoValido s then False
                                        else True

ehComandoValido :: String -> Bool
ehComandoValido [] = True
ehComandoValido (h:t) | h == 'A' || h == 'S' || h == 'E' || h == 'D' || h == 'L'  = ehComandoValido t
                      | otherwise = False 


-- | tirar os parenteses da combinaçao de numeros (combinaTudo)
semParentese :: [String] -> [String]
semParentese [] = []
semParentese (x:xs) | ehParentese x = semParentese xs
                    | otherwise = x:semParentese xs

-- | verifica se contém parenteses 
ehParentese :: String -> Bool
ehParentese [] = True
ehParentese (h:t) | h == '(' && h == ')' = ehParentese t
                  | otherwise = False 


-- | Recebe tabuleiro e retorna verdadeiro se nao houver erros, em caso de erro, a linha em que o erro ocorreu
validaTab :: [String] -> (Bool, Int)
validaTab [] = (True, 1)
validaTab a = let l = allSameSize a 
              in if l == 0 then (False, 0) 
                 else (True, l)
                            
                            
-- | allSameSize de uma lista de strings
-- | retorna 0 se todas as strings tiverem mesmo tamanho
-- | retorna > 0 com nr de linhas que difere
allSameSize :: [String] -> Int
allSameSize [] = 0 
allSameSize a = let l = length (head a)
                in validaLargura a l 1

-- | validaLargura recebe: lista de strings
-- | largura que strings devem ter, posiçao de linhas a validar
-- | retorna 0 se todas iguais, caso contrario, indice da linha que é diferente

validaLargura :: [String] -> Int -> Int -> Int
validaLargura [] _ _ = 0 
validaLargura (h:t) larg lin | length h == larg  = validaLargura t larg  (lin+1)
                             | otherwise         = lin 


-- | obtém tabuleiro 
getBoard :: [String] -> [String]
getBoard [] = []
getBoard (h:t) | ehLetra h = h:getBoard t
               | otherwise = []

-- | verifica se a string contém só letras 
ehLetra :: String -> Bool
ehLetra [] = True
ehLetra (h:t) | h >= 'a' && h <= 'z' || h>= 'A' && h<= 'Z' = ehLetra t
              | otherwise = False 

-- | indica o numero d linhas
getNumLinhas :: [String] -> Int
getNumLinhas [] = 0
getNumLinhas (h:t) = length (getBoard (h:t))

-- | indica de colunas
getNumColunas :: [String] -> Int
getNumColunas [] = 0
getNumColunas a = length (head a)


-- | cria uma lista de strings e divide onde tem a virgula em varias strings se o numero for impar e numa string dividida com com virgulas caso numero seja impar
splitstringx :: String -> Int -> [String]
splitstringx [] _ = [[]]
splitstringx (c:cs) x = let (s:ss) = splitstringx cs x
					in if (c==',') && (even x) then []:(s:ss) 
					   else if (c=='\"') then (splitstringx cs (x+1))
							    else (c:s):ss




