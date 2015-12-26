module Main
where

import Control.Monad(void)
import System.IO
import System.Random
import System.Time

import Tetris2D
import TetrisGraphics

main = do oldEcho <- hGetEcho stdin
          hSetEcho stdin False
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          randomFigure1 <- pickRandomFigure
          randomOrient1 <- pickRandomOrientation
          randomFigure2 <- pickRandomFigure
          randomOrient2 <- pickRandomOrientation
          let newField = emptyField randomFigure1 randomOrient1 randomFigure2 randomOrient2
          processio (startGame newField) newField =<< getClockTime
          hSetEcho stdin oldEcho

getMSeconds :: ClockTime -> Integer
getMSeconds (TOD sec psec) = sec * 1000 + div psec (10 ^ 9)

toWait :: ClockTime -> Integer -> ClockTime -> Int
toWait old mseclvl new = fromIntegral (max 0 (mseclvl - (getMSeconds new - getMSeconds old)))

pickRandomOrientation :: IO Orientation
pickRandomOrientation = fmap toEnum $ getStdRandom $ randomR 
    (fromEnum (minBound :: Orientation), fromEnum (maxBound :: Orientation))

pickRandomFigure :: IO Figure
pickRandomFigure = fmap toEnum $ getStdRandom $ randomR 
    (fromEnum (minBound :: Figure), fromEnum (maxBound :: Figure))

processio :: GameStatus -> GameStatus -> ClockTime -> IO ()
processio gs oldgs lastdown
  | isOver gs = void $ visualize gs oldgs
  | isPaused gs = do visualize gs oldgs
                     key <- getChar
                     case key of
                        'p' -> processio (startGame gs) gs =<< getClockTime
                        'q' -> return ()
                        _   -> processio gs oldgs lastdown
  | otherwise = do visualize gs oldgs
                   time <- getClockTime
                   mKey <- getKey $ toWait lastdown (lvlDelay (getLevel gs))
                   case mKey of
                     Nothing  -> do randomFigure <- pickRandomFigure
                                    randomOrient <- pickRandomOrientation
                                    processio (case nextStep gs of
                                        Right g -> g
                                        Left g -> g (randomFigure, randomOrient)) gs =<< getClockTime
                     Just "q" -> return ()
                     Just key -> processio (keyAction gs key) gs lastdown

getKey :: (ClockTime -> Int) -> IO (Maybe String)
getKey toWaitFunc = getKey' toWaitFunc 0

getKey' toWaitFunc escIndex = do
    time <- getClockTime
    hasInput <- hWaitForInput stdin $! toWaitFunc time
    if hasInput then do
         key <- getChar
         case key of 
            '\27' -> getKey' toWaitFunc 1
            '['   -> if escIndex == 1 then getKey' toWaitFunc 2 else return (Just "[")
            x     -> if escIndex == 2 then return (Just ['\27', '[', x]) else return (Just [x])
        else return Nothing

keyAction :: GameStatus -> String -> GameStatus
keyAction gs key
  | key == "p" = pauseGame gs
  | key == "a" || key == "\27[D" = moveLeft gs
  | key == "d" || key == "\27[C" = moveRight gs
  | key == "s" || key == "\27[B" = moveDown gs
  | key == "w" || key == "\27[A" = turnRight gs
  | key == " " = dropDown gs
  | otherwise = gs

lvlDelay n = ([1000, 900 .. 200] ++ [150] ++ repeat 100) !! n
