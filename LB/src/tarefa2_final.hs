module Main where

import Data.Char
type Tab = [String]
type Posicao = (Int, Int, String)
type Programa = String
type Dim = (Int, Int)

data Orientacao =  Norte
                 | Sul
                 | Este
                 | Oeste
                 deriving (Show, Eq, Ord)


-- svn checkout

-- svn checkout svn://svnpelaeduroam.alunos.di.uminho.pt t/li1g089 --username=li1lg089

-- svn co svn://svnpelaeduroam.alunos.di.uminho.pt/li1g089 --username=li1lg089

-- pdflatex relatorio.tex

-- let linhas = lines texto


-- | 1 Calculo da próxima posição
-- |  recebe um tabuleiro
-- |  lê a linha das coordenadas
-- |  lê a linha dos comandos 
-- |  recebe o primeiro comando da lista e implementa na função da próxima posição
-- |  se o comando for válido, retorna nova posição análoga
-- |  caso comando não seja valido retorna "ERRO"
-- |  comandos possíveis:
-- |  A -> Avançar (recebendo coordenadas e e orientação, o robô avança para a próxima posição caso o robô encontra-se no mesmo nível)
-- |  S -> Saltar (recebendo coordenadas e e orientação, o robô salta para a posição seguinte caso o robô encontra-se no nível superior o inferior)
-- |  E -> Esquerda (altera orientação no sentido contra relógio ex: se estiver na Orientação Norte (N) roda para Oeste (O))
-- |  D -> Direita (altera orientação no sentido de relógio ex: se estiver na Orientação Norte (N) roda para Este (E))
-- |  L -> Lâmpada (caso existe lâmpada na posição representado como letras maíuscula retorna essa posição, caso contrario retorna "ERRO")


{-
input:

aacdD
aacaa
bbCaa
0 1 S
S

output correspondente:

0 0 S
-}  
---------------------------------------------------------------------------------------------------------------------------------------------------


outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr(outStr(tarefa(lines inp)))

-- | verifica se o tabuleiro cumpre os parametros estabelecidos no enunciado com ajuda de funçoes auxiliares
tarefa :: [String] -> [String]
tarefa [] = ["ERRO"]
tarefa t = let tab = getBoard (t)
               altura = length tab
               comandos = t !! (altura + 1)
               nTotal = length comandos
               colunas = show(getNumColunas t)
               linhas  = show(getNumLinhas t)
               nLampadas = length(getLampadas(unwords tab))
               comandosSeparados = separaCharInStr nTotal comandos
               comandosStr = unwords comandosSeparados -- 
               --comandoCoordenadas = show (colunas) ++ " " ++ show (linhas) ++ " " ++ comandosStr -- ++ " " ++ "A" ++ " " ++ show linhas
               comando = comandosTransform comandosStr     
               lCoor   = t !! altura 
               newCoor = nextStep(transforma lCoor)comando tab -- dada uma string de coordenadas e uma lista de comandos, o bot vai para o sito certo no tabuleiro (falta aplicar repetição para cada comando) ***funciona:  converte(moveBot(transforma(lCoor)) comando)
               --vComand = caminhoValido comando
               vCoor   = caminhoValido t
               in if vCoor then ["ERRO"] 
                           -- else if nLampadas < 1 then ["ERRO"]
                           else if newCoor == lCoor && luzDesligada (transforma lCoor) comando t  then ["ERRO"]
                           else [newCoor] -- length (getLampadas (unwords tab))<1 ||


-- | funçao responsovel por alterar a oreinraão
rodaEsquerda :: String -> String
rodaEsquerda s | s == "N" = "O"
               | s == "O" = "S"
               | s == "S" = "E"
               | s == "E" = "N" 

rodaDireita :: String -> String
rodaDireita s  | s == "N" = "E"
               | s == "E" = "S"
               | s == "S" = "O"
               | s == "O" = "N" 


{-
moveBotStr :: Posicao -> String -> Tab -> String 
moveBotStr (x,y,o) comando  t  = 
  case comando of
    "A" -> if      o == "N" then converte(x, y-1, o) 
           else if o == "E" then converte(x+1, y, o) 
           else if o == "S" then converte(x, y+1, o) 
           else if o == "O" then converte(x-1,y,  o)
           else "ERRO"

    "S" -> if      o == "N" then converte(x, y-1, o) 
           else if o == "E" then converte(x+1, y, o) 
           else if o == "S" then converte(x, y+1, o) 
           else if o == "O" then converte(x-1,y,  o)
           else "ERRO"

    "E" -> converte(x, y, rodaEsquerda o)

    "D" -> converte(x, y, rodaDireita o)
-}


-- | proximo passo do robo, caso seja valido retorna novas coordenadas, caso contrario retorna as mesmas
nextStep :: Posicao -> String -> Tab -> String
nextStep (x,y,o) comando  t | comando == "A" && o == "N" && (oldCharYNorte (x,y,o)t == newCharYNorte(x,y,o)t
                                                         || abs(oldCharYNorte (x,y,o)t-newCharYNorte(x,y,o)t)  == 32)  = converte(x,y+1,o)

                            | comando == "A" && o == "E" && (oldCharXEste (x,y,o)t  == newCharXEste(x,y,o)t    
                                                         || abs(oldCharXEste (x,y,o)t-newCharXEste(x,y,o)t)    == 32)  = converte(x+1,y,o)

                            | comando == "A" && o == "S" && (oldCharYSul (x,y,o)t   == newCharYSul(x,y,o)t     
                                                         || abs(oldCharYSul (x,y,o)t-newCharYSul(x,y,o)t)      == 32)  = converte(x,y-1,o) 

                            | comando == "A" && o == "O" && (oldCharXOeste (x,y,o)t == newCharXOeste(x,y,o)t   
                                                         || abs(oldCharXOeste (x,y,o)t-newCharXOeste(x,y,o)t)  == 32)  = converte(x-1,y,o)

                            | comando == "S" && o == "N" && (abs(oldCharYNorte (x,y,o)t-newCharYNorte(x,y,o)t) == 1
                                                         || abs(oldCharYNorte (x,y,o)t-newCharYNorte(x,y,o)t)  == 33
                                                         || abs(oldCharYNorte (x,y,o)t-newCharYNorte(x,y,o)t)  == 31)  = converte(x,y+1,o) 

                            | comando == "S" && o == "E" && (abs(oldCharXEste (x,y,o)t-newCharXEste(x,y,o)t)   == 1    
                                                         || abs(oldCharXEste (x,y,o)t-newCharXEste(x,y,o)t)    == 33
                                                         || abs(oldCharXEste (x,y,o)t-newCharXEste(x,y,o)t)    == 31)  = converte(x+1,y,o)

                            | comando == "S" && o == "S" && (abs(oldCharYSul (x,y,o)t-newCharYSul(x,y,o)t)     == 1     
                                                         || abs(oldCharYSul (x,y,o)t-newCharYSul(x,y,o)t)      == 33
                                                         || abs(oldCharYSul (x,y,o)t-newCharYSul(x,y,o)t)      == 31)  = converte(x,y-1,o) 

                            | comando == "S" && o == "O" && (abs(oldCharXOeste (x,y,o)t-newCharXOeste(x,y,o)t) == 1   
                                                         || abs(oldCharXOeste (x,y,o)t-newCharXOeste(x,y,o)t)  == 33
                                                         || abs(oldCharXOeste (x,y,o)t-newCharXOeste(x,y,o)t)  == 31)  = converte(x-1,y,o)

                            | comando == "E"             = converte(x,y,rodaEsquerda o)

                            | comando == "D"             = converte(x,y,rodaDireita o)

                            | otherwise                  = converte(x,y,o) 


-- | diferença em inteiros entre nova posiçao e a posiçao anterior 

oldCharYNorte :: Posicao -> Tab -> Int
oldCharYNorte (x,y,o) t = ord((pos(x,y,o) t))

newCharYNorte :: Posicao -> Tab -> Int
newCharYNorte (x,y,o) t = if y/=((getNumLinhas (getBoard t))-1) then ord((pos(x,y+1,o) t)) else ord((pos(x,y,o) t))

oldCharYSul :: Posicao -> Tab -> Int
oldCharYSul (x,y,o) t = ord((pos(x,y,o) t))

newCharYSul :: Posicao -> Tab -> Int
newCharYSul (x,y,o) t =  ord((pos(x,abs(y-1),o) t))

oldCharXEste :: Posicao -> Tab -> Int
oldCharXEste (x,y,o) t = ord(pos(x,y,o) t)

newCharXEste :: Posicao -> Tab -> Int
newCharXEste (x,y,o) t = if x/=((getNumColunas t)-1) then  ord(pos(x+1,y,o) t) else ord(pos(x,y,o) t)

oldCharXOeste :: Posicao -> Tab -> Int
oldCharXOeste (x,y,o) t = ord(pos(x,y,o) t)

newCharXOeste :: Posicao -> Tab -> Int
newCharXOeste (x,y,o) t = ord(pos(abs(x-1),y,o) t)


luzDesligada :: Posicao -> String -> Tab -> Bool
luzDesligada (x,y,o) coords t = isLower (getPos(x, y, o) t)

 

-- | encotra a posiçao das as coordendas 
pos :: Posicao -> Tab -> Char
pos (c1,c2,c3) l = let nTotal=(length (getBoard l))-1
                       y=l!!((nTotal)-(c2))
                       x=y!!(c1)
                   in x


-- | encontra a letras das as coordenadas  --  pos = (y * nColunas) + x
getPos :: Posicao -> Tab -> Char
getPos (x,y,o) tab = getLetra (((if y==0 then 0 else y-1) * ((getNumColunas tab))+x)) (semSep(simplifica((unwords (getBoard tab)))))

-- | auxiliar de getPos
getLetra :: Int -> String -> Char
getLetra num letr = letr !! num

getPosInversa :: Posicao -> Tab -> Char
getPosInversa (x,y,o) tab = getLetra (indexPosNormal (x,y,o) tab) (reverse(semSep(simplifica((unwords (getBoard tab)))))) 

numElementos :: Tab -> Int
numElementos t = length (semSep(unwords(getBoard t)))

indexPosNormal :: Posicao -> Tab -> Int
indexPosNormal (x,y,o) tab = (if y<=0 then 0 else y-1) * (getNumColunas tab) + x


                                                                              
--teste posição matriz
{-
a b c   0,0 x=0 y=0 = 0 = v | 0,1 x=0 y=1 = 3 a | 0,2 x=0 y=2 6 a |   
a c d   0,0 x=0 y=0 = 0 = v | 1,0 x=1 y=1 = 1 c | 2,0 x=2 y=2 2 e | 
v c e
-}


-- | verifica se o caminho é valido
caminhoValido :: Tab -> Bool
caminhoValido t = let tabuleiro = getBoard (t)
                      alturaT = length tabuleiro
                      lComandos = t !! (alturaT + 1)
                      lCoords   = t !! alturaT
                      nTotalC = length lComandos
                      lComandosSeparados = separaCharInStr nTotalC lComandos
                      comandoStr = cmdStr (unwords lComandosSeparados)
                      newCoords = nextStep (transforma lCoords) (comandoStr) (tabuleiro)

                   in if read (cx newCoords) < 0   
                         || read (cy newCoords) < 0
                         || read (cx newCoords) > (getNumColunas tabuleiro) - 1
                         || read (cy newCoords) > (getNumLinhas tabuleiro) - 1
                      then True
                      else False

-- | coordenada x
cx :: String -> String  
cx s = let coords = words s
      in (coords !! 0) 
-- | coordenada y
cy :: String -> String  
cy s = let coords = words s
      in (coords !! 1)



-- | retorna o primeiro elemento da string
cmdStr :: String -> String
cmdStr s = let crds = words s
           in (crds !! 0)

-- | verifica se é sequencia 
ehSeq :: (Enum a, Eq a) => [a] -> Bool
ehSeq [] = True
ehSeq (x:[]) = True
ehSeq (x:y:zs) | y == succ x = ehSeq $ y:zs
ehSeq _ = False


-- | converte do tipo Posiçao de volta para string e tira tudo que tem em excesso ex: virgulas parentes etc...
converte :: Posicao -> String
converte (x,y,o) = simplifica(show (x,y,o))


-- | tira as virgulas parentes e as aspas duma string
simplifica :: String -> String
simplifica s = semVirg (semParent s)

-- | transforma uma string do tipo certo ex: "1 2 S" para tipo Posicao (1,2,'S')
transforma :: String -> (Int,Int, String)
transforma s = let coords = words s
            in (read(coords !! 0)::Int, read(coords !! 1)::Int, (last coords))

-- | tira parentes e as aspas
semParent :: String -> String
semParent [] = []
semParent (x:xs) | x == '(' || x == ')' || x == '"' || x == '/' = s  
                 | otherwise = x:s
       where s = semParent xs

-- | substitui virgula pelo espaço
semVirg :: String -> String
semVirg s = substVirg s
substVirg [] = []
substVirg (x:xs) = 
     if x == ',' 
     then ' ' : substVirg xs 
     else x : substVirg xs


-- | sem espaços
semSep :: String -> String
semSep [] = []
semSep (x:xs) | x == ' ' = s  
              | otherwise = x:s
       where s = semSep xs


-- | verifica se é lampada
ehLampada :: String -> Bool
ehLampada [] = True
ehLampada (h:t) | h>= 'A' && h<= 'Z' = ehLampada t
                | otherwise = False 

             

-- | tira o primeiro elemento da lista
comandosTransform :: String -> String  
comandosTransform s = let coords = words s
            in (coords !! 0)
            

-- | separa os characteres numa string com espaço 
separaCharInStr :: Int -> [a] -> [[a]]
separaCharInStr n [] = []
separaCharInStr n xs = take n $ [ y | y <- cada xs ] : separaCharInStr n (tail xs)
            where
            cada [] = []
            cada l@(y:ys) = y : cada (drop n l)
                       
                    
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

-- | retorna uma lista de lampadas
getLampadas :: String -> String
getLampadas [] = []
getLampadas (h:t) | isUpper h = h:getLampadas t
                  | otherwise = getLampadas t

-- | indica o numero d linhas
getNumLinhas :: [String] -> Int
getNumLinhas [] = 0
getNumLinhas (h:t) = (length (getBoard (h:t)))

-- | indica de colunas
getNumColunas :: [String] -> Int
getNumColunas [] = 0
getNumColunas a = length (head a)








