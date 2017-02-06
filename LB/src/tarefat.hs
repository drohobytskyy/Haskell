import Data.Char
-- | V2.0
-- | funçao principal (tarefa)
main = do inp <- getContents
          putStr (unlines(tarefa(lines inp)))

main1 f = do s <- readFile f
             putStr (unlines(tarefa (lines s)))


type Position = (Int, Int, Char)
type Posicao = (Int, Int, String)
type Tab = [String]
type Lilamps = (Int, Int)

-- | Funçao principal
tarefa :: [String] -> [String]
tarefa x = let revBoard = reverse x --reverse
               coord    = getCoord (head revBoard)
               board    = tail revBoard
               boardRev = reverse board
               lights   = isort (computeLights board)   -- se o (x) for maior que a posiçao da primeira lampada entao faz se reverse, caso contrario sem reverse, mesma coisa para (y)
               dLights  = distMin lights board coord
           in  [computeProgram lights board coord] -- [computeProgram lights board coord]


--  if fst1 (coord) > fst(head(computeLights board)) then reverse (computeLights board) else (computeLights board

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


isort :: [Lilamps] -> [Lilamps]
isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Lilamps -> [Lilamps] -> [Lilamps]
insert (x,y) [] = [(x,y)]
insert (x,y) ((x1,y1):xs) = if x < x1 then ((x,y):(x1,y1):xs)
                                      else ((x1,y1):(insert (x,y) xs))

first (x,_,_) = x 
second (_,x,_) = x
third (_,_,x) = x



{- | Pega na coordenada actual, calcula a rota mais curta para a lampada seguinte;

-}

distMin :: [Lilamps] -> Tab -> Position -> [(Lilamps)]
distMin [] t cr@(x,y,o) = []
distMin [(a,b)] t cr@(x,y,o) = [(a,b)]
distMin ((a,b):(c,d):bs) t cr@(x,y,o) = if length (computeProgram ([(a,b)]) t cr) < length (computeProgram ([(c,d)]) t cr) then (a,b):(c,d):bs
                                                                                                                           else (c,d):distMin ((a,b):bs) t cr

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


toString :: (Int,Int,Char) -> String
toString (c1,c2,c3) = [' ']++[intToDigit c1]++[' ']++[intToDigit c2]++[' ']++[c3]

-- |  funçao responsavel pelo cáclculo de um possivel caminho numa dada matriz, retorna instruçoes para chegar e acender todas as lampadas
computeProgram :: [(Int,Int)] -> [String] -> Position -> String
computeProgram [] _ _ = []
computeProgram ((lx, ly):lz) b (x,y,o) | lx == x && ly == y  = 'L':(computeProgram lz b (x, y, o))

                                       
                                       | ly > y  && o == 'N' && (mesmoNively (b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'N' && (validaSaltoy (b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly > y  && o == 'E' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly > y  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

                                 --      | ly > y  && o == 'N' && (mesmoNively (b) (x,y,o)) == False && (validaSaltoy (b) (x,y,o)) == False = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                   --    | ly > y  && o == 'O' && (mesmoNively (b) (x,y,o)) && (validaSaltoy (b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       
                                     --  | ly > y  && o == 'O' && (mesmoNively (b) (x,y,o)) && (validaSaltoy (b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                            
                                       | ly < y  && o == 'S' && (mesmoNively (b)(x,y,o))   = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'S' && (validaSaltoy (b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly < y  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly < y  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))


                                       | lx > x  && o == 'E' && (mesmoNivelx (b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'E' && (validaSaltox (b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))

                                       --| lx > x  && o == 'E' && (mesmoNively (b) (x,y,o)) == False && (validaSaltoy (b) (x,y,o)) == False = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       
                                       | lx < x  && o == 'O' && (mesmoNivelx (b) (x,y,o))  = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'O' && (validaSaltox (b) (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx < x  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       --| otherwise = 

                                       
                                      -- | lx < x  && o == 'O' && (mesmoNively (b) (x,y,o)) == False && (validaSaltoy (b) (x,y,o)) == False = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))

                                       
{-

                                       | ly > y  && o == 'N' && (mesmoNively (b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'N' && (validaSaltoy (b) (x,y,o))= 'S':(computeProgram ((lx,ly):lz) b (x,y+1, o))
                                       | ly > y  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly > y  && o == 'E' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly > y  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       
                                       | ly < y  && o == 'S' && (mesmoNively (b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'S' && (validaSaltoy (b) (x,y,o))= 'S':(computeProgram ((lx,ly):lz) b (x,y-1,o))
                                       | ly < y  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | ly < y  && o == 'O' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | ly < y  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))

                                       | lx > x  && o == 'E' && (mesmoNivelx (b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'E' && (validaSaltox (b) (x,y,o))= 'P':(computeProgram ((lx,ly):lz) b (x+1,y, o))
                                       | lx > x  && o == 'N' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'O' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx > x  && o == 'S' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       
                                       | lx < x  && o == 'O' && (mesmoNivelx (b) (x,y,o)) = 'A':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'O' && (validaSaltox b (x,y,o)) = 'S':(computeProgram ((lx,ly):lz) b (x-1,y, o))
                                       | lx < x  && o == 'S' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))
                                       | lx < x  && o == 'N' = 'E':(computeProgram ((lx,ly):lz) b (x,y,rotateLeft o))
                                       | lx < x  && o == 'E' = 'D':(computeProgram ((lx,ly):lz) b (x,y,rotateRight o))

-}

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
                     if ((direcaoX (c3)) /=0) && (((ord (x1)) == (ord (x))) || (abs((ord (x1))-(ord (x)))==32)) then True
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
                    if ((direcaoY (c3)) /=0) && (((ord (x1)) == (ord (x))) || (abs((ord (x1))-(ord (x)))==32)) then True 
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