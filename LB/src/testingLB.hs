--import System.IO  (stdin, stdout, hSetEcho, hSetBuffering
 --                 ,BufferMode(..))

type Tab = [String]
type Posicao = (Int, Int, Char)
type Programa = String
type Dim = (Int, Int)



type Coordenadas = (Int, Int, Char) 


data Coords =  A
             | S
             | E
             | D
             | L
             deriving (Show, Eq, Ord)

data Orientacao =  Norte
                 | Sul
                 | Este
                 | Oeste
                 deriving (Show, Eq, Ord)

rodaEsq :: Orientacao -> Orientacao
rodaEsq Norte = Oeste
rodaEsq Oeste = Sul
rodaEsq Sul = Este
rodaEsq Este = Norte

rodaDir :: Orientacao -> Orientacao
rodaDir Norte = Este
rodaDir Este = Sul
rodaDir Sul = Oeste
rodaDir Oeste = Norte

rodaEsquerda :: Char -> Char
rodaEsquerda s | s == 'N' = 'O'
               | s == 'O' = 'S'
               | s == 'S' = 'E'
               | s == 'E' = 'N' 

rodaDireita :: Char -> Char
rodaDireita s | s == 'N' = 'E'
              | s == 'E' = 'S'
              | s == 'S' = 'O'
              | s == 'O' = 'N' 


-- move o lightBot num tabuleiro
{-move :: Posicao -> Coords -> Posicao
move (x,y,o) input = 
  case input of
    A -> (x+1, y, o)
    S -> (x, y+1, o)
    E -> (x, y, rodaEsquerda o)
    D -> (x, y, rodaDireita o)
-}

-- conta quantos carateres iguais tem na lista
conta :: Char -> String -> Int
conta _ [] = 0
conta l (h:t) | l==h = 1 + conta l t
              | otherwise = conta l t 


-- move o lightBot num tabuleiro
moveBot :: Posicao -> Char -> Posicao
moveBot (x,y,o) comando = 
  case comando of
    'A' -> (x+1, y, o)
    'S' -> (x, y+1, o)
    'E' -> (x, y, rodaEsquerda o)
    'D' -> (x, y, rodaDireita o)

-- converte do tipo Posiçao de volta para string e tira tudo que tem em excesso ex: virgulas parentes etc...
converte :: Posicao -> String
converte (x,y,o) = simplifica(sq(show (x,y,o)))

-- tira as virgulas parentes e as aspas duma string
simplifica :: String -> String
simplifica s = semVirg (semParent s)

-- transforma uma string do tipo certo ex: "1 2 S" para tipo Posicao (1,2,'S')
transforma :: String -> (Int,Int, Char)  
transforma s = let coords = words s
            in (read(coords !! 0), read(coords !! 1), (head (last coords)))

-- tira parentes e as aspas
semParent :: String -> String
semParent [] = []
semParent (x:xs) | x == '(' || x == ')' || x == '"' = s  
              | otherwise = x:s
       where s = semParent xs

-- substitui virgula pelo espaço
semVirg :: String -> String
semVirg s = substVirg s
substVirg [] = []
substVirg (x:xs) = 
     if x == ',' 
     then ' ' : substVirg xs 
     else x : substVirg xs

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s






--teste
getCommands :: String -> [String]
getCommands s = words s

-- verifica se é lampada
ehLampada :: String -> Bool
ehLampada [] = True
ehLampada (h:t) | h>= 'A' && h<= 'Z' = ehLampada t
                | otherwise = False 

-- por defeito a lampada está desligada
lamapLigada :: Bool -> Bool
lamapLigada l = False

-- ligar a lamapada
ligarLamp :: Bool -> Bool
ligarLamp l = True


--proximaP :: Posicao -> Tab -> Int -> Int-> Char -> String
--proximaP (x,y,o) t nl nc c | c == 'A'|| c == 'S' && o =='E' && y == nc-1 = "ERRO"

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr(outStr(tarefa(lines inp)))

-- verifica se o tabuleiro cumpre os parametros estabelecidos no enunciado com ajuda de funçoes auxiliares
tarefa :: [String] -> [String]
tarefa [] = ["ERRO"]
tarefa t = let tab = getBoard (t)
               altura = length tab
               comandos = t !! (altura + 1)
               --comando = comandosTransform comandos
               nA = conta 'A' comandos
               nS = conta 'S' comandos
               nE = conta 'E' comandos
               nD = conta 'D' comandos
               nTotal = length comandos
               comandosSeparados = separaCharInStr nTotal comandos
               comandosStr = unwords comandosSeparados
               comando = comandosTransform comandosStr
               lCoor = t !! altura
               newCoor = converte(moveBot(transforma(lCoor)) (head comandosStr)) -- dada uma string de coordenadas e uma lista de comandos, o bot vai para o sito certo no tabuleiro (falta aplicar repetição para cada comando)
           in  [newCoor]


-- testes
tarefa1 :: [String] -> [String]
tarefa1 [] = ["ERRO"]
tarefa1 t = let tab = getBoard (t)
                altura = length tab
                comandos = t !! (altura + 1)
                nA = conta 'A' comandos
                nS = conta 'S' comandos
                nE = conta 'E' comandos
                nD = conta 'D' comandos
                nTotal = length comandos
                comandosS = comandosTransform comandos
                lCoor = t !! altura
                comandosSep = separaCharInStr nTotal comandos 
                vCoor = (moveAllInOne lCoor ("A S A"))
             in [vCoor]
-- testes
tarefa2 :: [String] -> Int
tarefa2 [] = 1
tarefa2 t = let tab = getBoard (t)
                altura = length tab
                comandos = t !! (altura + 1)
                lCoor = t !! altura
                vCoor = converte(moveBot(transforma (lCoor)) (head comandos))
             in conta 'A' comandos

-- tira o primeiro elemento da lista
comandosTransform :: String -> Char  
comandosTransform s = let coords = words s
            in head (coords !! 0)


-- teste
numCStr :: Int -> String  
numCStr s = show s
            

-- separa os characteres numa string com espaço 
separaCharInStr :: Int -> [a] -> [[a]]
separaCharInStr n [] = []
separaCharInStr n xs = take n $ [ y | y <- cada xs ] : separaCharInStr n (tail xs)
            where
            cada [] = []
            cada l@(y:ys) = y : cada (drop n l)




moveAllInOne :: String -> [Char] -> String
moveAllInOne _ [] = []
moveAllInOne coords (comando:comandos)  = converte(moveBot(transforma(coords)) comando)

--moveAllInOne coords (command:commands) = converte(moveBot(transforma (coords))(command:moveAllInOne coords commands))


--mAIO :: String -> String -> String
--mAIO coords [] = coords
--mAIO coords (command:commands) = moveAllInOne coords command

--priMai s | s/=[] = isupper (head s)

--moveAllInOne :: String -> String -> String
--moveAllInOne coords (c1:comandos) = converte(moveBot(transforma (coords))c1):moveAllInOne coords (head comandos)

tSR :: String -> String
tSR (h:t) = show h


soma :: String -> String
soma x = x ++ "***teste"


--converte(moveBot(transforma (lCoor))comandos)



--               
-- converte(move(read(lCoor!!0)),(read(lCoor !! 1)), (show(lCoor !! 2))A)
-- "("++show(lCoor !! 0) ++ "," ++ show(lCoor !! 2) ++ "," ++ show(lCoor !! 4) ++ ")"
        
--teste s = map (filter (/='"')) (words s)              
          
              

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
                    




getBoard :: [String] -> [String]
getBoard [] = []
getBoard (h:t) | ehLetra h = h:getBoard t
               | otherwise = []

-- verifica se a string contém só letras 
ehLetra :: String -> Bool
ehLetra [] = True
ehLetra (h:t) | h >= 'a' && h <= 'z' || h>= 'A' && h<= 'Z' = ehLetra t
              | otherwise = False 


-- indica o numero d linhas
getNumLinhas :: [String] -> Int
getNumLinhas [] = 0
getNumLinhas (h:t) = length (getBoard (h:t))

-- indica de colunas
getNumColunas :: [String] -> Int
getNumColunas [] = 0
getNumColunas a = length (head a)

 


{-
proximaP :: Posicao -> Tab -> Int -> Int-> Char -> String
proximaP (x,y,o) t nl nc c | c == 'A' || c == 'S' && o =='E' && y == nc-1 = "ERRO"
                           | c == 'A' && o == 'E' = (show x) ++ " " ++ (show(y+1)) ++ "" ++ [o]
                           | (c == 'A'|| c == 'S' ) && o =='O' && y == 0 = "ERRO"
                           | c == 'A' && o == 'O' = (show x) ++ " " ++ (show(y-1)) ++ " " ++ [o]
                           | c == 'A' && o == 'N' = (show (x+1)) ++ " " ++ ( show y) ++ " "  ++ [o]
                           | (c =='A' || c == 'S')  && o == 'S' && x == 0 = "ERRO"
                           | c == 'A' && o == 'S' = (show ( x-1 )) ++ " " ++ (show y) ++ "" ++ [o]
                           | c == 'S' && o == 'O' &&( (seguintes (t) ( x,y ) (x,(y-1))) = "ERRO"
  --t1= [" aaBa"." abBC". " bcde"]
-}








 --"<"++ obterLinhaErrada ++">" 

-- svn checkout

-- svn checkout svn://svnpelaeduroam.alunos.di.uminho.pt t/li1g089 --username=li1lg089

-- svn co svn://svnpelaeduroam.alunos.di.uminho.pt/li1g089 --username=li1lg089

-- pdflatex relatorio.tex

--let linhas = lines texto


{-
tenPseudorandomNumbers :: Int -> [Int]
tenPseudorandomNumbers seed = take 10 . randomRs (0, 9) . mkStdGen $ seed

-}





