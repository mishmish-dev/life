import qualified Control.Concurrent (threadDelay)
import Control.DeepSeq (deepseq)
import qualified System.Environment (getArgs)
import qualified Life

delayTimeMicroseconds :: Int
delayTimeMicroseconds = 100000


clearTerminalLines :: Int -> String
clearTerminalLines n =
  ("\ESC[" ++ show n ++ "A") -- Move cursor up
  ++ "\ESC[J" -- Clear terminal from cursor to the end
  

loop :: Int -> String -> Life.Board -> IO ()
loop genNumber clearTerminalSequence board = do
  let boardStr = show board
      nextDisplayStr =
        (if genNumber > 0 then clearTerminalSequence else "")
        ++ "  Generation: " ++ show genNumber ++ "\n"
        ++ boardStr

  boardStr `deepseq` putStrLn nextDisplayStr
  Control.Concurrent.threadDelay delayTimeMicroseconds
  loop (genNumber+1) clearTerminalSequence (Life.nextGen board)


startSimulation :: Life.Board -> IO ()
startSimulation board = do
  putStrLn $ "  Size: " ++ show (Life.width board) ++ " x " ++ show (Life.height board)
  let clearTerminalSequence = clearTerminalLines (Life.height board + 3)
  loop 0 clearTerminalSequence board


main :: IO ()
main = do
  args <- System.Environment.getArgs
  rawInput <-
    if null args then
      getContents
    else if length args == 1 then
      readFile $ head args
    else
      fail "too many cmdline arguments"

  case Life.parseBoard rawInput of
    Life.Parsed board -> startSimulation board
    Life.UnevenRowLength -> fail "uneven row length in the input"
    Life.BoardTooSmall -> fail $ "input board must not be less than " ++ show Life.minBoardDimension ++ " in one dimension"
    Life.BoardEmpty -> fail "the input is empty"
