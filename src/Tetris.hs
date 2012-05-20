import Control.Monad (void)
import Data.Array
import System.IO
import System.Random
import System.Time

import Tetris2D
import TetrisGraphics

main = do oldEcho <- hGetEcho stdin
          hSetEcho stdin False
          hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          rand1 <- pickRandom
          rand2 <- pickRandom
          let newField = emptyField rand1 rand2
          processio (startGame newField) newField =<< getClockTime
          hSetEcho stdin oldEcho

getMSeconds :: ClockTime -> Integer
getMSeconds (TOD sec psec) = sec * 1000 + div psec (10 ^ 9)

toWait :: ClockTime -> ClockTime -> Integer -> Int
toWait new old mseclvl = fromIntegral (max 0 (mseclvl - (getMSeconds new - getMSeconds old)))

pickRandom :: IO Int
pickRandom = getStdRandom (randomR (0, 4 * figNum - 1))

processio :: GameStatus -> GameStatus -> ClockTime -> IO ()
processio gs oldgs lastdown
  | isOver gs = void $ visualize gs oldgs
  | isPaused gs = do visualize gs oldgs
                     key <- getKey
                     case key of
                        "p" -> processio (startGame gs) gs =<< getClockTime
                        "q" -> return ()
                        _   -> processio gs oldgs lastdown
  | otherwise = do visualize gs oldgs
                   time <- getClockTime
                   hasInput <- hWaitForInput stdin $! toWait time lastdown (lvlDelay (getLevel gs))
                   if not hasInput
                   then do rand <- pickRandom
                           processio (timeStep gs rand) gs =<< getClockTime
                   else do key <- getKey
                           case key of
                              "q" -> return ()
                              _   -> processio (keyAction gs key) gs lastdown

getKey = do key <- getChar
            if key == '\27'
            then do key1 <- getChar
                    if key1 == '['
                    then do key2 <- getChar
                            return [key, key1, key2]
                    else
                        return [key, key1]
            else
                return [key]

timeStep :: GameStatus -> Int -> GameStatus
timeStep gs rnum
  | can $ moveDown gs = moveDown gs
  | otherwise = concrete gs rnum

keyAction :: GameStatus -> String -> GameStatus
keyAction gs key
  | key == "p" = pauseGame gs
  | key == "a" || key == "\27[D" = tryDo (moveLeft gs) gs
  | key == "d" || key == "\27[C" = tryDo (moveRight gs) gs
  | key == "s" || key == "\27[B" = tryDo (moveDown gs) gs
  | key == "w" || key == "\27[A" = tryDo (rotateRight gs) gs
  | key == " " = dropDown gs
  | otherwise = gs
  where tryDo moved original = if can moved then moved else original

lvlDelay n = ([1000, 900 .. 200] ++ [150] ++ repeat 100) !! n
