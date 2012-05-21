module Tetris2D (fieldWidth, fieldHeight,
                 FigureType (..),
                 figNum, Figure (..), Score, State, GameStatus(..), emptyField,
                 isOver, isNotStarted, isPaused, shape, getLevel,
                 startGame, finishGame, pauseGame, can, moveLeft, moveRight,
                 moveDown, dropDown, rotateLeft, rotateRight, concrete
                 )
where
{- TODO
 - second space freeze
 -}
import Data.Array
import Data.Maybe(isNothing)

fieldWidth  = 10 :: Int
fieldHeight = 20 :: Int

type Direction = Int

data FigureType  = FigI | FigJ | FigL | FigO | FigS | FigZ | FigT deriving (Eq)
figNum = 7 :: Int
data Figure = Figure FigureType Direction deriving (Eq)
type Score = Integer
data State = NotStarted | InProgress | Paused | Finished deriving (Eq)

instance Show Figure
  where
  show fig = concatMap (++ "\n") $
               map (map (\n -> if n == 0 then ' ' else 'X'))
               (shape fig)

data GameStatus = Status {
          getArr :: Array Int (Array Int (Maybe FigureType)), -- array of fallen figures
          getFigure :: Figure, -- current figure
          getXY :: (Int, Int), -- its position
          getNext :: Figure, -- next figure
          getScores :: Score, -- score
          getState :: State, -- is finished
          getLines :: Int -- eaten lines
          } deriving (Eq)

isOver status = getState status == Finished
isNotStarted status = getState status == NotStarted
isPaused status = getState status == Paused
getLevel status = min 10 (div (getLines status) 12)

rotL :: Direction -> Direction
rotL x = mod (x + 1) 4

rotR :: Direction -> Direction
rotR x = mod (x - 1) 4

emptyField :: Int -> Int -> GameStatus
emptyField r1 r2 = Status { getArr = listArray (1,fieldHeight)
                                               (repeat $ listArray (1,fieldWidth) (repeat Nothing))
                          , getFigure = firstFig
                          , getXY = startPosition firstFig
                          , getNext = nextFig
                          , getScores = 0
                          , getState = NotStarted
                          , getLines = 0
                          }
  where
  firstFig = randomFig r1
  nextFig  = randomFig r2

startPosition :: Figure -> (Int, Int)
startPosition fig = (fieldWidth `div` 2 - 1 + voidleft, 2 - h + voiddown)
  where
  sh = shape fig
  h = length sh
  voiddown = length $ takeWhile (notElem 1) $ reverse sh
  voidleft = minimum $ map (length . takeWhile (==0)) sh

randomFig :: Int -> Figure
randomFig r = Figure f (r `mod` 4)
  where
  f = figs !! (r `div` 4)
  figs = [FigI, FigJ, FigL, FigO, FigS, FigZ, FigT]

positions :: Figure -> (Int, Int) -> [(Int, Int)]
positions fig (x,y) =
    concat [map (\a -> (x + snd a, y + snd xs)) $ filter (\a -> fst a == 1) (zip (fst xs) [0..])
                 | xs <- zip fs [0 ..]] where fs = shape fig

finishGame status@Status { getState = state } = status { getState = Finished }
startGame  status@Status { getState = state } = status { getState = InProgress }
pauseGame  status@Status { getState = state } = status { getState = Paused }

concrete :: GameStatus -> Int -> GameStatus
concrete (Status arr fig@(Figure f d) (x, y) next s curr l) rnum
  | can newstat && y > 0 = newstat
  | otherwise = Status { getArr = fst na
                       , getFigure = fig
                       , getXY = (x, y)
                       , getNext = next
                       , getScores = scor $ snd na
                       , getState = Finished
                       , getLines = l + snd na
                       }
  where
  newstat = Status { getArr = fst na
                   , getFigure = next
                   , getXY = startPosition next
                   , getNext = randomFig rnum
                   , getScores = scor (snd na)
                   , getState = InProgress
                   , getLines = l + snd na
                   }
  na = removeLines frozen s
  frozen = arr // [ (y + n, (arr!(y + n)) //
                        [(x + m, Just f) | m <- [0 .. fw - 1],
                                            x + m > 0,
                                            x + m <= fieldWidth,
                                            fs!!n!!m == 1])
                            | n <- [(max 0 (1 - y)) .. fl - 1],
                              y + n > 0,
                              y + n <= fieldHeight ]
  fs = shape fig
  fl = length fs
  fw = length $ head fs
  removeLines ar s = (listArray (1, fieldHeight) $
      replicate d (listArray (1, fieldWidth) (repeat Nothing)) ++ xs,
      d)
    where
    xs = filter (elem Nothing . elems) (elems ar)
    d  = fieldHeight - length xs
  scor n
    | n == 1 = s + 40
    | n == 2 = s + 100
    | n == 3 = s + 300
    | n == 4 = s + 1200
    | otherwise = s + 0

can :: GameStatus -> Bool
can (Status arr fig (x, y) _ _ _ _) =
    all (\(x,y) ->x >= 1 &&
                  x <= fieldWidth &&
		          y <= fieldHeight &&
		         (y < 1 || isNothing (arr!y!x))) pos
  where pos = positions fig (x,y)

moveLeft  status@Status { getXY = (x, y) } = status { getXY = (x-1, y) }
moveRight status@Status { getXY = (x, y) } = status { getXY = (x+1, y) }
moveDown  status@Status { getXY = (x, y) } = status { getXY = (x, y+1) }
rotateLeft  status@Status { getFigure = fig@(Figure f d) } = status { getFigure = Figure f (rotL d) }
rotateRight status@Status { getFigure = fig@(Figure f d) } = status { getFigure = Figure f (rotR d) }

dropDown gs = if can moved then dropDown moved else gs
    where moved = moveDown gs

shape :: (Num a) => Figure -> [[a]]
shape (Figure f d)
  | f == FigI && d == 0 =
      [[0,0,1,0],
       [0,0,1,0],
       [0,0,1,0],
       [0,0,1,0]]
  | f == FigJ && d == 0 =
      [[0,1,1],
       [0,1,0],
       [0,1,0]]
  | f == FigL && d == 0 =
      [[1,1,0],
       [0,1,0],
       [0,1,0]]
  | f == FigS && d == 0 =
      [[0,0,0],
       [0,1,1],
       [1,1,0]]
  | f == FigZ && d == 0 =
      [[0,0,0],
       [1,1,0],
       [0,1,1]]
  | f == FigT && d == 0 =
      [[0,1,0],
       [1,1,1],
       [0,0,0]]
  | f == FigO =
      [[1,1],
       [1,1]]
  | d == 1 = reverse [map (!! n) x | let x = shape (Figure f 0), n <-[0..length (head x) - 1]]
  | d == 2 = let x = shape (Figure f 0) in reverse (map reverse x)
  | d == 3 = [reverse $ map (!! n) x | let x = shape (Figure f 0), n <-[0..length (head x) - 1]]

