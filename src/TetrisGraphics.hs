module TetrisGraphics --(visualize)
where
import Data.Map (Map, empty, notMember, fromList, union, member, delete, (!))
import Tetris2D

fieldStr :: GameStatus -> [[(Color, Char)]]
fieldStr gs@Game{getDebris = debris, getCurrentFigure = fig, getCurrentPosition = pos} = [[
    if member (Coord x y) figures then (color (figures ! Coord x y), 'X') else (0, ' ') 
    | x <- [0..fieldWidth-1]] | y <- [0..fieldHeight - 1]]
    where figures = debris `union` figureAsDebris fig pos

fieldStrs :: GameStatus -> [String]
fieldStrs gs@Game{getDebris = debris, getCurrentFigure = fig, getCurrentPosition = pos} =
    [concatMap (\x -> if member (Coord x y) allDebris then coloredCell (color (allDebris ! Coord x y), 'X') else " ") [0..fieldWidth-1] | y <- [0..fieldHeight - 1]]
    where allDebris = debris `union` figureAsDebris fig pos

visualize gs oldgs = if isNotStarted oldgs then putStr $! outstr else putStr $! changestr
  where 
  outstr = "\27[2J\27[H" ++ 
      unlines (addBorder $ fieldStrs gs) ++ 
      moveCursor 16 3 ++ "Next:" ++ drawNext gs ++ 
      moveCursor 16 9 ++ "Score:" ++ drawScores gs ++
      moveCursor 16 12 ++ "Level:" ++ drawLevel gs ++
      moveCursor 1 (3 + fieldHeight)
  wholeColoredField = map (concatMap coloredCell) (fieldStr gs)
  changestr = redrawFigureDiffs 
      ++ (if getScores gs /= getScores oldgs then drawScores gs else "")
      ++ (if getLevel gs /= getLevel oldgs then drawLevel gs else "")
      ++ (if getNext gs /= getNext oldgs then drawNext gs else "")
      ++ (if isOver gs then moveCursor 2 10 ++ "GAME OVER!" else "")
      ++ drawPause gs oldgs
      ++ moveCursor 1 (3+fieldHeight)
  redrawFigureDiffs = concat [moveCursor (1 + a) (1 + b) ++ coloredCell c
      | ((a,b,c),(a', b', c')) <- embedCoord (fieldStr gs) `zip` embedCoord (fieldStr oldgs), c /= c']
  embedCoord a = concatMap (\m -> [(n + 1, fst (m !! n), snd (m !! n)) | n <- [0..length m - 1]])
                  [map (\m -> (n + 1, m))(a !! n) | n <- [0..length a - 1]]

moveCursor x y = "\27["++ show y ++ ';' : show x ++"f"
drawPause gs oldgs
  | isPaused gs && not (isPaused oldgs) = moveCursor 16 11 ++ "PAUSED"
  | isPaused oldgs && not (isPaused gs) = moveCursor 16 11 ++ "      "
  | otherwise = "" 
drawScores gs = moveCursor 17 10 ++ show (getScores gs)
drawLevel gs = moveCursor 18 13 ++ show (getLevel gs)
drawNext gs = concat [moveCursor 16 (4+n) ++ nextFig!!n | n <- [0..(length nextFig - 1)]]
  where
  upTo4 n xs = if length xs < 4 then xs ++ replicate (4 - length xs) n else xs
  nextFig = map (concatMap (\a -> if a then coloredCell (color nextFigure, 'X') else " ")) $
              upTo4 [False, False, False, False] (map (upTo4 False) (rotatedShape nextFigure nextOrient))  
  next@(nextFigure, nextOrient) = getNext gs

addBorder st = horizLine : (map (\a -> "|"++ a ++ "|") st ++ [horizLine])
horizLine    = "+" ++ replicate fieldWidth '-' ++ "+"

type Color = Int
color FigI = 36
color FigJ = 34
color FigL = 30
color FigO = 33
color FigS = 32
color FigZ = 31
color FigT = 35

coloredCell :: (Color, Char) -> String
coloredCell (n, l) 
  | n == 0 = [l]
  | otherwise = "\27[1;" ++ show (n + 10) ++ ";" ++ show n ++ "m" ++ l : "\27[m"
