import qualified Control.Concurrent (threadDelay)
import qualified Control.DeepSeq (deepseq)
import qualified System.Environment (getArgs)
import qualified Life

delayTimeMicroseconds :: Int
delayTimeMicroseconds = 100000


clearTermLines :: Int -> String
clearTermLines n =
  ("\ESC[" ++ show n ++ "A") -- Move cursor up
  ++ "\ESC[J" -- Clear terminal from cursor to the end
  

loop :: Int -> String -> Life.Board -> IO ()
loop genNumber clearTerm board = do
  let boardStr = show board
      nextDisplayStr =
        clearTerm ++ "  Generation: " ++ show genNumber
        ++ "\n" ++ boardStr

  boardStr `Control.DeepSeq.deepseq` putStrLn nextDisplayStr
  Control.Concurrent.threadDelay delayTimeMicroseconds
  loop (genNumber+1) (clearTermLines (Life.height board + 3)) (Life.nextGen board)


startSimulation :: Life.Board -> IO ()
startSimulation board = do
  putStrLn $ "  Size: " ++ show (Life.width board) ++ " x " ++ show (Life.height board)
  loop 0 "" board


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
