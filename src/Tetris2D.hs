{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Tetris2D (Coord(..), Position(..), fieldHeight, fieldWidth, Figure(..), figureAsDebris, GameStatus(..), getLevel, isNotStarted, isOver, isPaused, rotatedShape, emptyField, startGame, nextStep, Orientation(..), pauseGame, moveLeft, moveRight, moveDown, turnRight, dropDown, rotateRight, isValid, State(..))
where

import Control.Lens
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

data Coord = Coord {_x :: Int, _y :: Int } deriving (Eq, Ord, Show)
makeLenses ''Coord

data Position = Position {_coord :: Coord, _orientation :: Orientation} deriving (Eq, Show)
makeLenses ''Position

type Score = Integer
data State = NotStarted | InProgress | Paused | Finished deriving (Eq, Show)
type Debris = Map Coord Figure

data GameStatus = Game {
    _debris :: Debris,              -- array of fallen figures
    _currentFigure :: Figure,       -- current figure
    _currentPosition :: Position,   -- position of the current figure
    _next :: (Figure, Orientation), -- next figure
    _scores :: Score,               -- score
    _gameState :: State,            -- is finished
    _lines :: Int                   -- eaten lines - we need them to change level
    } deriving (Eq, Show)
getLevel status = min 10 (div (_lines status) 12)

makeLenses ''GameStatus

emptyField :: Figure -> Orientation -> Figure -> Orientation -> GameStatus
emptyField randomFigure1 randomOrient1 randomFigure2 randomOrient2 = Game {
    _debris = empty,
    _currentFigure = randomFigure1,
    _currentPosition = initFigurePosition (randomFigure1, randomOrient1),
    _next = (randomFigure2, randomOrient2),
    _scores = 0,
    _gameState = NotStarted,
    _lines = 0
  }

finishGame = gameState .~ Finished
startGame  = gameState .~ InProgress
pauseGame  = gameState .~ Paused

initFigurePosition :: (Figure, Orientation) -> Position
initFigurePosition (fig, o) = Position {
    _coord = Coord {
      _x = fieldWidth `div` 2 - 1 + voidleft,
      _y = 2 - length sh + voidbelow
    },
    _orientation = o
  }
  where
  sh = rotatedShape fig o
  voidbelow = length $ takeWhile (not . or) $ reverse sh
  voidleft = minimum $ map (length . takeWhile not) sh

getOccupiedBlocks :: Figure -> Position -> [Coord]
getOccupiedBlocks fig pos =
   map toCoord $ truthOnly $ withNumbers $ rotatedShape fig $ pos ^. orientation
  where
  withNumbers = concat . zipWithY . zipWithX
  zipWithX = map (zip [pos ^. coord ^. x ..])
  zipWithY = zipWith (\a -> map (a,)) [pos ^. coord ^. y ..]
  truthOnly = filter (snd . snd)
  toCoord (yy, (xx, _)) = Coord xx yy

isValid :: GameStatus -> Bool
isValid status =
    all (\coord@(Coord x y) -> x >= 0 && x < fieldWidth && y < fieldHeight
        && notMember coord (status ^. debris)
    ) $ getOccupiedBlocks (status ^. currentFigure) (status ^. currentPosition)

moveIfValid movement gs = let moved = movement gs in if isValid moved then moved else gs

turnLeft = moveIfValid $ currentPosition.orientation %~ rotateLeft
turnRight = moveIfValid $ currentPosition.orientation %~ rotateRight
moveLeft = moveIfValid $ currentPosition.coord.x -~ 1
moveRight = moveIfValid $ currentPosition.coord.x +~ 1
moveDownUnverified = currentPosition.coord.y +~ 1
moveDown = moveIfValid moveDownUnverified

validator change good bad st = if isValid (change st) then good st else bad st

dropDown gs = if isValid moved then dropDown moved else gs
  where moved = moveDownUnverified gs

scoreIncrement lines =
    case lines of
      0 -> 0
      1 -> 40
      2 -> 100
      3 -> 300
      _ -> 1200

concrete gs randomNext = if isValid newState then newState else finishGame newState
    where
    newState =
        ( (debris .~ newDebris)
        . (currentFigure .~ (fst $ gs ^. next))
        . (currentPosition .~ (initFigurePosition (gs ^. next)))
        . (next .~ randomNext)
        . (scores +~ scoreIncrement (length extraRows))
        . (Tetris2D.lines +~ length extraRows)) gs
    debrisWithExtraRows = _debris gs `union` figureAsDebris (_currentFigure gs) (_currentPosition gs)
    extraRows = [row | y <- [0..fieldHeight - 1], let row = [Coord x y | x <- [0..fieldWidth - 1]], all (`member` debrisWithExtraRows) row]
    extraRowNumbers = sortBy (flip compare) (map (_y . head) extraRows)
    newDebris = mapKeys (\c@Coord {_y = y} -> c {_y = y + length (takeWhile (>=y) extraRowNumbers)}  ) $
      foldl (flip delete) debrisWithExtraRows (concat extraRows)

nextStep gs = let next = moveDownUnverified gs
              in if isValid next then Right next else Left $ concrete gs

isOver status = _gameState status == Finished
isNotStarted status = _gameState status == NotStarted
isPaused status = _gameState status == Paused

figureAsDebris fig pos = fromList $ zip (getOccupiedBlocks fig pos) (repeat fig)

