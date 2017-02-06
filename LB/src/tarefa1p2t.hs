module Main where
import Data.Char
-- | funçao principal (tarefa)
main = do inp <- getContents
          putStr (unlines(tarefa(lines inp)))

type Position = (Int, Int, Char)
type Posicao = (Int, Int, String)
type Tab = [String]

-- | Funçao principal
tarefa :: [String] -> [String]
tarefa x = let revBoard = reverse x --reverse --
               coord    = getCoord (head revBoard)
               board    = tail revBoard
               boardRev = getBoard (reverse board)
               lights   = (computeLights board)    -- se o (x) for maior que a posiçao da primeira lampada entao faz se reverse, caso contrario sem reverse, mesma coisa para (y)
           in  [computeProgram lights boardRev coord] -- [computeProgram lights board coord]


--  if fst1 (coord) > fst(head(computeLights board)) then reverse (computeLights board) else (computeLights board)

--  if second (coord) > snd(head((computeLights board))) then reverse((computeLights board)) else (qsort(computeLights board))
--  (computeLights board) 

{-
tarefa :: [String] -> [String]
tarefa x = let revBoard = reverse x --reverse
               coord    = getCoord (head revBoard)
               board    = tail revBoard
               boardRev = getBoard (reverse board)
               lights   = (computeLights board) -- se o (x) for maior que a posiçao da primeira lampada entao faz se reverse, caso contrario sem reverse, mesma coisa para (y)
           in  [computeProgram lights boardRev coord] -- [computeProgram lights board coord]
-}


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y|y <- xs, y<x] ++ [x] ++ qsort[y|y <- xs, y>=x]


first (x,_,_) = x 
second (_,x,_) = x
third (_,_,x) = x


-- | funçao responsavel por retornar coordenadas das lampadas da matriz
computeLights :: [String] -> [(Int,Int)]
computeLights b = computeLineLights b 0

-- | retorna as coordenadas de lampadas de uma linha 
computeLineLights :: [String] -> Int -> [(Int,Int)]
computeLineLights [] _ = []
computeLineLights (l:ls) linha = computeEachLight l linha 0 ++ computeLineLights ls (linha + 1)

-- | calcula as lampadas individuais  
computeEachLight :: String -> Int -> Int -> [(Int,Int)]
computeEachLight [] _ _ = []
computeEachLight (c:cs) lin col | isUpper (c) = (col, lin):(computeEachLight cs lin (col + 1))
                                | otherwise = computeEachLight cs lin (col + 1)

-- |  funçao responsavel pelo cáclculo de um possivel caminho numa dada matriz, retorna instruçoes para chegar e acender todas as lampadas
computeProgram :: [(Int,Int)] -> [String] -> Position -> String
computeProgram [] _ _ = []
computeProgram ((lx, ly):lz) b (x,y,o) | lx == x && ly == y  = 'L':(computeProgram lz b (x, y, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'N' && (validaSaltoy (getBoard b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly > y  && o == 'E' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly > y  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       
                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'S' && (validaSaltoy (getBoard b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly < y  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly < y  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

                                       | lx > x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'E' && (validaSaltox (getBoard b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))

                                       | lx < x  && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'O' && (validaSaltox (getBoard b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx < x  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

                                         
                                       -- Ir a volta pelo Nordeste
                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 && (mesmoNively (getBoard b) (x+1,y,'N'))
                                                 = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E'))
                                                 && (validaSaltoy (getBoard b) (x+1,y,'N'))
                                                 = 'D':'S':'E':'S':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 && (validaSaltoy (getBoard b) (x+1,y,'N'))
                                                 = 'D':'A':'E':'S':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,'E'))
                                                 && (mesmoNively (getBoard b) (x+1,y,'N'))
                                                 = 'D':'S':'E':'A':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                       ---------------------------------------------------------------------------
                                       
                                       -- Ir a volta pelo Noroeste 
                                       | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (mesmoNively (getBoard b) (x,y,'O'))
                                                 && (mesmoNively (getBoard b) (x-1,y,'N'))
                                                 = 'E':'A':'D':'A':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                       | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'O'))
                                                 && (validaSaltoy (getBoard b) (x-1,y,'N'))
                                                 = 'E':'S':'D':'S':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                       | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (mesmoNively (getBoard b) (x,y,'O'))
                                                 && (validaSaltoy (getBoard b) (x-1,y,'N'))
                                                 = 'E':'A':'D':'S':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))
                                       
                                       | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'O'))
                                                 && (mesmoNively (getBoard b) (x-1,y,'N'))
                                                 = 'E':'S':'D':'A':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                       -------------------------------------------------------------------------

                                       -- Ir a volta pelo Sudeste
                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 && (mesmoNively (getBoard b) (x+1,y,'S'))
                                                 = 'E':'A':'D':'A':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       | ly < y && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E'))
                                                 && (validaSaltoy (getBoard b) (x+1,y,'S'))
                                                 = 'E':'S':'D':'S':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       | ly < y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 && (validaSaltoy (getBoard b) (x-1,y,'S'))
                                                 = 'E':'A':'D':'S':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       | ly < y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,'E'))
                                                 && (mesmoNively (getBoard b) (x-1,y,'S'))
                                                 = 'E':'S':'D':'A':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       ---------------------------------------------------------------------------

                                       -- Ir a volta pelo Sudoeste
                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (mesmoNively (getBoard b) (x,y,'O'))
                                                 && (mesmoNively (getBoard b) (x+1,y,'S'))
                                                 = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       | ly < y  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'O'))
                                                 && (validaSaltoy (getBoard b) (x+1,y,'S'))
                                                 = 'D':'S':'E':'S':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       | ly < y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (mesmoNively (getBoard b) (x,y,'O'))
                                                 && (validaSaltoy (getBoard b) (x-1,y,'S'))
                                                 = 'D':'A':'E':'S':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       | ly < y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNively (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'E')) == False
                                                 && (validaSaltoy (getBoard b) (x,y,'O'))
                                                 && (mesmoNively (getBoard b) (x-1,y,'S'))
                                                 = 'D':'S':'E':'A':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))
                                        
                                        ------------------------------------------------------------------------

                                       -- Falta: Este Norte, Este Sul, Oeste Norte, Oeste Sul

                                       -- Ir a volta pelo Este Norte 
                                       | lx > x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N'))
                                                 && (mesmoNivelx (getBoard b) (x,y+1,'E'))
                                                 = 'E':'A':'D':'A':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                        | lx > x && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,'N'))
                                                 && (validaSaltox (getBoard b) (x,y+1,'E'))
                                                 = 'E':'S':'D':'S':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                        | lx > x && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N'))
                                                 && (validaSaltox (getBoard b) (x,y+1,'E'))
                                                 = 'E':'A':'D':'S':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                        | lx > x && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,'N'))
                                                 && (mesmoNivelx (getBoard b) (x,y+1,'E'))
                                                 = 'E':'S':'D':'A':(computeProgram ((lx,ly):lz) b (x+1,y+1, o))

                                       ---------------------------------------------------------------------------
                                       
                                       -- Ir a volta pelo Este Sul 
                                       | lx > x && o == 'E' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (mesmoNivelx (getBoard b) (x,y,'S'))
                                                 && (mesmoNivelx (getBoard b) (x-1,y,'S'))
                                                 = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       | lx > x && o == 'E' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'S'))
                                                 && (validaSaltox (getBoard b) (x-1,y,'S'))
                                                 = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       | lx > x && o == 'E' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (mesmoNivelx (getBoard b) (x,y,'S'))
                                                 && (validaSaltox (getBoard b) (x-1,y,'S'))
                                                 = 'D':'A':'E':'S':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))
                                       
                                       | lx > x && o == 'E' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'S'))
                                                 && (mesmoNivelx (getBoard b) (x-1,y,'S'))
                                                 = 'D':'S':'E':'A':(computeProgram ((lx,ly):lz) b (x+1,y-1, o))

                                       -------------------------------------------------------------------------

                                       -- Ir a volta pelo Oeste Norte 
                                       | lx < x  && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N'))
                                                 && (mesmoNivelx (getBoard b) (x,y+1,'O'))
                                                 = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                        | lx < x && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,'N'))
                                                 && (validaSaltox (getBoard b) (x,y+1,'O'))
                                                 = 'D':'S':'E':'S':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                        | lx < x && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N'))
                                                 && (validaSaltox (getBoard b) (x,y+1,'O'))
                                                 = 'D':'A':'E':'S':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                        | lx < x && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,'N'))
                                                 && (mesmoNivelx (getBoard b) (x,y+1,'O'))
                                                 = 'D':'S':'E':'A':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                       ---------------------------------------------------------------------------

                                        -- Ir a volta pelo Oeste Sul 
                                       | lx < x && o == 'O' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (mesmoNivelx (getBoard b) (x,y,'S'))
                                                 && (mesmoNivelx (getBoard b) (x,y-1,'O'))
                                                 = 'E':'A':'D':'A':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       | lx < x  && o == 'O' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'S'))
                                                 && (validaSaltox (getBoard b) (x,y-1,'O'))
                                                 = 'E':'S':'D':'S':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       | lx < x  && o == 'O' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (mesmoNivelx (getBoard b) (x,y,'S'))
                                                 && (validaSaltox (getBoard b) (x,y-1,'O'))
                                                 = 'E':'A':'D':'S':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))
                                       
                                       | lx < x && o == 'O' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False 
                                                 && (mesmoNivelx (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'N')) == False
                                                 && (validaSaltox (getBoard b) (x,y,'S'))
                                                 && (mesmoNivelx (getBoard b) (x,y-1,'O'))
                                                 = 'E':'S':'D':'A':(computeProgram ((lx,ly):lz) b (x-1,y-1, o))

                                       -------------------------------------------------------------------------
                                       
                                        | otherwise = 'D':'A':'E':'A':(computeProgram ((lx,ly):lz) b (x-1,y+1, o))

                                       
                                    -- | otherwise = 'E':'A':'D':'A':'A':(computeProgram ((lx,ly):lz) b (x+2,y+1, o))

{-
                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) 
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 = 'D':'A':'E':'A':'A':(computeProgram ((lx,ly):lz) b (x-1,y+2, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) 
                                                 && (mesmoNively (getBoard b) (x,y,'E'))
                                                 && (mesmoNively (getBoard b) (x+1,y,'N'))
                                                 = 'D':'A':'E':'A':'A':(computeProgram ((lx,ly):lz) b (x-1,y+2, o))

                                       | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) 
                                                 && (validaSaltoy (getBoard b) (x,y,'E'))
                                                 = 'D':'S':'E':'A':'A':(computeProgram ((lx,ly):lz) b (x-1,y+2, o))

                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltoy (getBoard b) (x,y,o)) == False  
                                                 = 'E':'A':'D':'A':'A':(computeProgram ((lx,ly):lz) b (x+1,y+2, o))

                                       | lx > x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) == False 
                                                 && (validaSaltox (getBoard b) (x,y,o)) == False  
                                                 = 'E':'A':'D':'A':'A':(computeProgram ((lx,ly):lz) b (x+2,y+1, o))
                                       
                                       | otherwise = 'D':'A':'E':'A':'A':(computeProgram ((lx,ly):lz) b (x-2,y+1, o))


-}

computeProgramNO :: [(Int,Int)] -> [String] -> Position -> String
computeProgramNO [] _ _ = []
computeProgramNO ((lx, ly):lz) b (x,y,o) | lx == x && ly == y  = 'L':(computeProgramNO lz b (x, y, o))
                                        
                                        -- | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o))  = 'A':(computeProgramNO ((lx,ly):lz) b (x,y+1, o))
                                         
                                          -- Ir a volta pelo Noroeste 
                                         | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                   && (mesmoNivelx (getBoard b) (x,y,'O'))
                                                   && (mesmoNively (getBoard b) (x-1,y,'N'))
                                                   = 'E':'A':'D':'A':(computeProgramNO ((lx,ly):lz) b (x-1,y+1, o))
                                                 
                                         | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                   && (validaSaltox (getBoard b) (x,y,'O'))
                                                   && (validaSaltoy (getBoard b) (x-1,y,'N'))
                                                   = 'E':'S':'D':'S':(computeProgramNO ((lx,ly):lz) b (x-1,y+1, o))

                                         | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                   && (mesmoNivelx (getBoard b) (x,y,'O'))
                                                   && (validaSaltoy (getBoard b) (x-1,y,'N'))
                                                   = 'E':'A':'D':'S':(computeProgramNO ((lx,ly):lz) b (x-1,y+1, o))

                                         | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltox (getBoard b) (x,y,'O'))
                                                   && (mesmoNively (getBoard b) (x-1,y,'N'))
                                                   = 'E':'S':'D':'A':(computeProgramNO ((lx,ly):lz) b (x-1,y+1, o))

                                      

computeProgramNE :: [(Int,Int)] -> [String] -> Position -> String
computeProgramNE [] _ _ = []
computeProgramNE ((lx, ly):lz) b (x,y,o) | lx == x && ly == y  = 'L':(computeProgramNE lz b (x, y, o))

                                         

                                          -- Ir a volta pelo Nordeste
                                         | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                  -- && (mesmoNivelx (getBoard b) (x,y,'O')) == False
                                                  -- && (validaSaltox (getBoard b) (x,y,'O')) == False 
                                                   && (mesmoNivelx (getBoard b) (x,y,'E'))
                                                   && (mesmoNively (getBoard b) (x+1,y,'N'))
                                                   = 'D':'A':'E':'A':(computeProgramNE ((lx,ly):lz) b (x+1,y+1, o))
                                                   
                                          | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                  -- && (mesmoNivelx (getBoard b) (x,y,'O')) == False
                                                  -- && (validaSaltox (getBoard b) (x,y,'O')) == False 
                                                   && (validaSaltox (getBoard b) (x,y,'E'))
                                                   && (validaSaltoy (getBoard b) (x+1,y,'N'))
                                                   = 'D':'S':'E':'S':(computeProgramNE ((lx,ly):lz) b (x+1,y+1, o))

                                          | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                  -- && (mesmoNivelx (getBoard b) (x,y,'O')) == False
                                                  -- && (validaSaltox (getBoard b) (x,y,'O')) == False 
                                                   && (mesmoNivelx (getBoard b) (x,y,'E'))
                                                   && (validaSaltoy (getBoard b) (x+1,y,'N'))
                                                   = 'D':'A':'E':'S':(computeProgramNE ((lx,ly):lz) b (x+1,y+1, o)) 
                                                   
                                          | ly > y && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) == False 
                                                   && (validaSaltoy (getBoard b) (x,y,o)) == False
                                                  -- && (mesmoNivelx (getBoard b) (x,y,'O')) == False
                                                  -- && (validaSaltox (getBoard b) (x,y,'O')) == False 
                                                   && (validaSaltox (getBoard b) (x,y,'E'))
                                                   && (mesmoNively (getBoard b) (x+1,y,'N'))
                                                   = 'D':'S':'E':'A':(computeProgramNE ((lx,ly):lz) b (x+1,y+1, o))  
{-
                                          
-}                                   
                                       {-
                                       | lx < x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o))  = 'A':(computeProgramI ((lx,ly):lz) b (x+1,y, o))
                                       | lx < x  && o == 'E' && (validaSaltox (getBoard b) (x,y,o)) = 'S':(computeProgramI ((lx,ly):lz) b (x+1,y, o))
                                       | lx < x  && o == 'N' = 'D':(computeProgramI ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'O' = 'D':(computeProgramI ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'S' = 'E':(computeProgramI ((lx,ly):lz) b (x,y,rotateLeft o))

                                       | lx > x  && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) = 'A':(computeProgramI ((lx,ly):lz) b (x-1,y, o))
                                       | lx > x  && o == 'O' && (validaSaltox (getBoard b) (x,y,o)) = 'S':(computeProgramI ((lx,ly):lz) b (x-1,y, o))
                                       | lx > x  && o == 'S' = 'D':(computeProgramI ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'N' = 'E':(computeProgramI ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx > x  && o == 'E' = 'D':(computeProgramI ((lx,ly):lz) b (x,y,rotateRight o))
                                       -}

-- corrigido, mas dá 11 pts
{-
  | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'N' = 'S':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly > y  && o == 'E' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly > y  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       
                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'S' = 'S':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly < y  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly < y  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

                                       | lx > x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'E' = 'S':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       
                                       | lx < x  && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'O' = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx < x  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

-}

-- com um error mas dá 16 pts

{-

 | ly > y  && o == 'N' && (mesmoNively (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'N' = 'S':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly > y  && o == 'E' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly > y  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       
                                       | ly < y  && o == 'S' && (mesmoNively (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'S' = 'S':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly < y  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly < y  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))

                                       | lx > x  && o == 'E' && (mesmoNivelx (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'E' = 'S':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx > x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       
                                       | lx < x  && o == 'O' && (mesmoNivelx (getBoard b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'O' = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx < x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

-}


validaSaltox :: Tab -> Position -> Bool
validaSaltox l c = let ytotal=(length l)-1
                       (c1,c2,c3) = c
                       x=y!!(c1) 
                       y=l!!((ytotal)-(c2))
                       x1=y!!(c1+direcaoX c3)
                   in
                     if ((direcaoX (c3)) /=0) && (abs((ord (x1))-(ord (x)))==1 || abs((ord (x1))-(ord (x)))==31 || abs((ord (x1))-(ord (x)))==33) then True 
                                                                                                                                                  else False
                    where 
                        direcaoX :: Char -> Int
                        direcaoX cmd
                                  |cmd =='O' = -1
                                  |cmd =='E' = 1
                                  |otherwise = 0 



validaSaltoy :: Tab -> Position -> Bool
validaSaltoy l c = let ytotal=(length l)-1
                       (c1,c2,c3) = c
                       y=l!!((ytotal)-(c2))
                       x=y!!(c1)
                       y1=l!!(ytotal-(c2 + direcaoY c3))
                       x1=y1!!(c1)
                    in
                      if ((direcaoY (c3)) /=0) && (abs((ord (x1))-(ord (x)))==1 || abs((ord (x1))-(ord (x)))==31 || abs((ord (x1))-(ord (x)))==33) then True 
                                                                                                                                                 else False
                    where 
                        direcaoY :: Char -> Int
                        direcaoY cmd 
                                  |cmd=='N' = 1
                                  |cmd=='S' = -1
                                  |otherwise = 0

mesmoNivelx :: Tab -> (Int,Int,Char) -> Bool
mesmoNivelx l c = let ytotal=(length l)-1
                      (c1,c2,c3) = c
                      x=y!!(c1)
                      y=l!!((ytotal)-(c2))
                      x1=y!!(c1+direcaoX c3)
                   in
                     if ((direcaoX (c3)) /=0) && (((ord (x1)) == (ord (x))) || (abs((ord (x1))-(ord (x)))==32) || (abs((ord (x1))-(ord (x)))==0)) then True
                                                                                                                else False
                   where
                        direcaoX :: Char -> Int
                        direcaoX cmd
                                  |cmd =='O' = -1
                                  |cmd =='E' = 1
                                  |otherwise = 0


mesmoNively :: Tab -> (Int,Int,Char) -> Bool
mesmoNively l c = let ytotal=(length l)-1
                      (c1,c2,c3) = c
                      y=l!!((ytotal)-(c2))
                      x=y!!(c1)
                      y1=l!!(ytotal-(c2 + direcaoY c3))
                      x1=y1!!(c1)
                   in
                    if ((direcaoY (c3)) /=0) && (((ord (x1)) == (ord (x))) || (abs((ord (x1))-(ord (x)))==32) || (abs((ord (x1))-(ord (x)))==0)) then True 
                                                                                                               else False
                   where 
                        direcaoY :: Char -> Int
                        direcaoY cmd 
                                  |cmd=='N' = 1
                                  |cmd=='S' = -1
                                  |otherwise = 0









                                                                    

{-

 | ly > y  && o /= 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotate o))
                                       | ly > y              = 'A':(computeProgram ((lx,ly):lz) b (x,y+1,o))
                                       | ly < y  && o /= 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotate o))   
                                       | ly < y              = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                      
                                       | lx > x  && o /= 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotate o))
                                       | lx > x              = 'A':(computeProgram ((lx,ly):lz) b (x+1,y,o))
                                       | lx < x  && o /= 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotate o))   
                                       | lx < x              = 'A':(computeProgram ((lx,ly):lz) b (x-1,y,o))
               
-}

rotateRight :: Char -> Char
rotateRight 'N' = 'E'
rotateRight 'E' = 'S'
rotateRight 'S' = 'O'
rotateRight 'O' = 'N' 

rotateLeft :: Char -> Char
rotateLeft 'N' = 'O'
rotateLeft 'O' = 'S'
rotateLeft 'S' = 'E'
rotateLeft 'E' = 'N' 
       

getCoord :: String -> Position
getCoord x = let l = words x
                 a = read (l !! 0) :: Int
                 b = read (l !! 1) :: Int
                 o = head (l !! 2)
              in (a, b, o)

getBoard :: [String] -> [String]
getBoard [] = []
getBoard (h:t) | ehLetra h = h:getBoard t
               | otherwise = []

ehLetra :: String -> Bool
ehLetra [] = True
ehLetra (h:t) | h >= 'a' && h <= 'z' || h>= 'A' && h<= 'Z' = ehLetra t
              | otherwise = False 

pos :: (Int,Int,String) -> [String] -> Char
pos (cx,cy,o) l = let nTotal=(length (getBoard l))-1
                      y=l!!((nTotal)-(cy))
                      x=y!!(cx)
                  in x