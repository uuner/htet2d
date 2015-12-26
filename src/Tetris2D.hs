{-# LANGUAGE TupleSections #-}
module Tetris2D (Coord(..), Position(..), fieldHeight, fieldWidth, Figure(..), figureAsDebris, GameStatus(..), getLevel, isNotStarted, isOver, isPaused, rotatedShape, emptyField, startGame, nextStep, Orientation(..), pauseGame, moveLeft, moveRight, moveDown, turnRight, dropDown, rotateRight, isValid, State(..))
where

import Data.Map (Map, empty, mapKeys, notMember, fromList, union, member, delete, (!))
import Data.List (sortBy)

fieldWidth  = 10 :: Int
fieldHeight = 20 :: Int

data Orientation = N | E | S | W deriving (Eq, Enum, Bounded, Show)
data Figure  = FigI | FigJ | FigL | FigO | FigS | FigZ | FigT deriving (Eq, Enum, Bounded, Show)

shape :: Figure -> [[Bool]]
shape fig = map (map (==1)) $ case fig of
    FigI -> [[0,0,1,0],
             [0,0,1,0],
             [0,0,1,0],
             [0,0,1,0]]
    FigJ -> [[0,1,1],
             [0,1,0],
             [0,1,0]]
    FigL -> [[1,1,0],
             [0,1,0],
             [0,1,0]]
    FigS -> [[0,0,0],
             [0,1,1],
             [1,1,0]]
    FigZ -> [[0,0,0],
             [1,1,0],
             [0,1,1]]
    FigT -> [[0,1,0],
             [1,1,1],
             [0,0,0]]
    FigO -> [[1,1],
             [1,1]]

rotatedShape :: Figure -> Orientation -> [[Bool]]
rotatedShape fig N = shape fig
rotatedShape fig E = reverse [map (!! n) x | let x = shape fig, n <-[0..length (head x) - 1]]
rotatedShape fig S = let x = shape fig in reverse (map reverse x)
rotatedShape fig W = [reverse $ map (!! n) x | let x = shape fig, n <-[0..length (head x) - 1]]

rotateRight o = if o == maxBound then minBound else succ o
rotateLeft o = if o == minBound then maxBound else pred o

data Coord = Coord {
    getX :: Int,
    getY :: Int
    } deriving (Eq, Ord, Show)  

data Position = Position {
    getCoord :: Coord, 
    getOrientation :: Orientation
    } deriving (Eq, Show)

type Score = Integer
data State = NotStarted | InProgress | Paused | Finished deriving (Eq, Show)
type Debris = Map Coord Figure

data GameStatus = Game {
    getDebris :: Debris,              -- array of fallen figures
    getCurrentFigure :: Figure,       -- current figure
    getCurrentPosition :: Position,   -- position of the current figure
    getNext :: (Figure, Orientation), -- next figure
    getScores :: Score,               -- score
    getGameState :: State,            -- is finished
    getLines :: Int -- eaten lines - we need them to change level
    } deriving (Eq, Show)
getLevel status = min 10 (div (getLines status) 12)

emptyField :: Figure -> Orientation -> Figure -> Orientation -> GameStatus
emptyField randomFigure1 randomOrient1 randomFigure2 randomOrient2 = Game {
    getDebris = empty,
    getCurrentFigure = randomFigure1, 
    getCurrentPosition = initFigurePosition randomFigure1 randomOrient1,
    getNext = (randomFigure2, randomOrient2),
    getScores = 0,
    getGameState = NotStarted,
    getLines = 0
  }

finishGame status = status { getGameState = Finished }
startGame  status = status { getGameState = InProgress }
pauseGame  status = status { getGameState = Paused }

initFigurePosition :: Figure -> Orientation -> Position
initFigurePosition fig o = Position {
    getCoord = Coord {
      getX = fieldWidth `div` 2 - 1 + voidleft,
      getY = 2 - length sh + voidbelow
    },
    getOrientation = o
  }
  where
  sh = rotatedShape fig o
  voidbelow = length $ takeWhile (not . or) $ reverse sh
  voidleft = minimum $ map (length . takeWhile not) sh

getOccupiedBlocks :: Figure -> Position -> [Coord]
getOccupiedBlocks fig pos =
   map toCoord $ truthOnly $ withNumbers $ rotatedShape fig $ getOrientation pos
  where 
  withNumbers = concat . zipWithY . zipWithX
  zipWithX = map (zip [getX (getCoord pos)..])
  zipWithY = zipWith (\a -> map (a,)) [getY (getCoord pos)..]
  truthOnly = filter (snd . snd)
  toCoord (yy, (xx, _)) = Coord xx yy

isValid :: GameStatus -> Bool
isValid status =
    all (\coord@(Coord x y) -> x >= 0 && x < fieldWidth && y < fieldHeight
        && notMember coord (getDebris status)
    ) $ getOccupiedBlocks (getCurrentFigure status) (getCurrentPosition status)

moveIfValid gs moved = if isValid moved then moved else gs
moveLeft gs@Game {getCurrentPosition = p@Position {getCoord = c@Coord {getX = x}}} =
    moveIfValid gs gs { getCurrentPosition = p {getCoord = c{getX = x - 1}}}

moveRight gs@Game {getCurrentPosition = p@Position {getCoord = c@Coord {getX = x}}} =
    moveIfValid gs gs { getCurrentPosition = p {getCoord = c{getX = x + 1}}}

moveDownUnverified gs@Game {getCurrentPosition = p@Position {getCoord = c@Coord {getY = y}}} =
    gs { getCurrentPosition = p {getCoord = c{getY = y + 1}}}

moveDown gs@Game {getCurrentPosition = p@Position {getCoord = c@Coord {getY = y}}} =
    let moved = moveDownUnverified gs in if isValid moved then moved else gs

turnLeft gs@Game { getCurrentPosition = p@Position {getOrientation = o }} =
    moveIfValid gs gs { getCurrentPosition = p {getOrientation = rotateLeft o}}

turnRight gs@Game { getCurrentPosition = p@Position {getOrientation = o }} =
    moveIfValid gs gs { getCurrentPosition = p {getOrientation = rotateRight o}}

dropDown gs = if isValid moved then dropDown moved else gs
  where moved = moveDownUnverified gs

scoreIncrement lines =
    case lines of
      0 -> 0
      1 -> 40
      2 -> 100
      3 -> 300
      _ -> 1200

concrete gs randomNext = if isValid newState then newState else newState {getGameState = Finished}
    where 
    newState = gs { 
        getDebris = newDebris,
        getCurrentFigure = oldNextFigure, 
        getCurrentPosition = initFigurePosition oldNextFigure oldNextOrientation,
        getNext = randomNext,
        getScores = getScores gs + scoreIncrement (length extraRows),
        getLines = getLines gs + length extraRows 
    }
    debrisWithExtraRows = getDebris gs `union` figureAsDebris (getCurrentFigure gs) (getCurrentPosition gs)
    extraRows = [row | y <- [0..fieldHeight - 1], let row = [Coord x y | x <- [0..fieldWidth - 1]], all (`member` debrisWithExtraRows) row]
    (oldNextFigure, oldNextOrientation) = getNext gs
    extraRowNumbers = sortBy (flip compare) (map (getY . head) extraRows)
    newDebris = mapKeys (\c@Coord {getY = y} -> c {getY = y + length (takeWhile (>=y) extraRowNumbers)}  ) $
      foldl (flip delete) debrisWithExtraRows (concat extraRows) 

nextStep gs = let next = moveDownUnverified gs 
              in if isValid next then Right next else Left $ concrete gs

isOver status = getGameState status == Finished
isNotStarted status = getGameState status == NotStarted
isPaused status = getGameState status == Paused

figureAsDebris fig pos = fromList $ zip (getOccupiedBlocks fig pos) (repeat fig)

