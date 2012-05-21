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
          rand1 <- pickRandom
          rand2 <- pickRandom
          let newField = emptyField rand1 rand2
          processio (startGame newField) newField =<< getClockTime
          hSetEcho stdin oldEcho

getMSeconds :: ClockTime -> Integer
getMSeconds (TOD sec psec) = sec * 1000 + div psec (10 ^ 9)

toWait :: ClockTime -> Integer -> ClockTime -> Int
toWait old mseclvl new = fromIntegral (max 0 (mseclvl - (getMSeconds new - getMSeconds old)))

pickRandom :: IO Int
pickRandom = getStdRandom (randomR (0, 4 * figNum - 1))

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
                     Nothing  -> do rand <- pickRandom
                                    processio (timeStep gs rand) gs =<< getClockTime
                     Just "q" -> return ()
                     Just key -> processio (keyAction gs key) gs lastdown

getKey :: (ClockTime -> Int) -> IO (Maybe String)
getKey toWaitFunc = getKey' toWaitFunc 0

getKey' toWaitFunc escIndex = do
    time <- getClockTime
    hasInput <- hWaitForInput stdin $! toWaitFunc time
    if hasInput 
    then do
         key <- getChar
         case key of 
            '\27' -> getKey' toWaitFunc 1
            '['   -> if escIndex == 1 then getKey' toWaitFunc 2 else return (Just "[")
            x     -> if escIndex == 2 then return (Just ['\27', '[', x]) else return (Just [x])
    else return Nothing

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
