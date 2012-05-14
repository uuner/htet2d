module TetrisGraphics (visualize)
where
{-TODO
 - help on the right side
 - visualize top scores
 -}
import Data.Array

import Tetris2D

fieldStr :: (Num t) => GameStatus -> [[(t, [Char])]]
fieldStr gs = map (map fun) z
   where
   fun (a, b)
     | a /= Nothing = (color a, "X")
     | b == 0 = (0, " ")
     | True = (color (Just f), "O")
   arr = getArr gs
   (x, y) = getXY gs
   fig@(Figure f d) = getFigure gs
   z = zipWith zip mArr fArr
   mArr = map elems (elems arr)
   fArr = (replicate (max (y - 1) 0) (replicate fieldWidth 0)) ++
     map (\a -> (replicate (x - 1) 0) ++ 
                 (drop (1-x) a) ++ 
		 (replicate (max (fieldWidth - fromIntegral x - w + 1) 0) 0)) 
	 (drop (1-y) $ shape fig) ++
     replicate (max (fieldHeight - y - h + 1) 0) (replicate fieldWidth 0)
   w = length $ head $ shape fig
   h = length $ shape fig

colorize gs = map (concatMap coloredCell) (fieldStr gs)

visualize gs oldgs = do 
  if isNotStarted oldgs 
    then 
    putStr $! outstr
    else
    putStr $! changestr
  where 
  outstr = "\27[2J\27[H" ++ 
           concatMap (++"\n") (border strs) ++ 
	   moveCursor 16 3 ++ "Next:" ++ 
	   drawNext 16 4 ++ 
	   moveCursor 16 9 ++ "Score:" ++
	   drawScores 17 10 ++
	   moveCursor 16 12 ++ "Level:" ++
	   drawLevel 18 13 ++
	   (moveCursor 1 (3+fieldHeight))
  strs = colorize gs
  horiz st = "+"++(replicate fieldWidth '-')++"+"
  border st = (horiz st): (map (\a -> "|"++ a ++ "|") st ++ [horiz st])
  next@(Figure nf dir) = getNext gs
  upTo4 n xs = if length xs < 4 then xs ++ replicate (4 - length xs) n else xs
  empt n = replicate n []
  changestr = concat (zipWith (\(a,b,c) (a', b', c') -> 
                   if c /= c' 
       		   then ((moveCursor (1+a) (1+b))++coloredCell c)
       		   else "")
                      (g $ fieldStr gs) (g $ fieldStr oldgs))
              ++ (if getScores gs /= getScores oldgs then drawScores 17 10 else "")
              ++ (if getLevel gs /= getLevel oldgs then drawLevel 18 13 else "")
              ++ (if getNext gs /= getNext oldgs then drawNext 16 4 else "")
	      ++ (if isOver gs then moveCursor 2 10 ++ "GAME OVER!" else "")
	      ++ drawPause
              ++ (moveCursor 1 (3+fieldHeight))
  moveCursor x y = "\27["++show y++';':(show x)++"f"
  drawNext x y = concat [moveCursor x (y+n) ++ nextFig!!n | n <- [0..(length nextFig - 1)]]
  nextFig = map (concatMap (\a -> if a == 0 then " " else coloredCell (color (Just nf), "X"))) $
       	   upTo4 [0,0,0,0] (map (upTo4 0) 
       	        (shape next))
  drawPause
    | isPaused gs && not (isPaused oldgs) = moveCursor 16 11 ++ "PAUSED"
    | isPaused oldgs && not (isPaused gs) = moveCursor 16 11 ++ "     "
    | True = ""
    
  drawScores x y = moveCursor x y ++ (show $ getScores gs)
  drawLevel x y = moveCursor x y ++ (show $ getLevel gs)
  g a = concat $ map (\m -> [(n+1,fst (m!!n), snd (m!!n)) | 
                              n <-[0..length m - 1]])
                     [map (\m -> (n+1,m))(a!!n) | 
       	           n <-[0..length a - 1]]

color x = case x of
  Just FigI -> 36
  Just FigJ -> 34
  Just FigL -> 30
  Just FigO -> 33
  Just FigS -> 32
  Just FigZ -> 31
  Just FigT -> 35
  _ -> 0

coloredCell (n, l) 
  | n == 0 = l
  | True = f n
  where f n = "\27[1;"++show (n+10) ++ ";" ++ show n ++ "m" ++ l ++ "\27[m"
