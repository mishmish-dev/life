import Control.Concurrent
import Data.Maybe (fromJust)
import Lib

tickTime :: Int
tickTime = 100000

main :: IO ()
main = do
  rawInput <- getContents
  let board = fromJust $ parseBoard rawInput
  loop 0 board

loop :: Int -> Board -> IO ()
loop genNumber board = do
  putStr "-- Generation "
  print genNumber
  print board
  threadDelay tickTime
  putStr ("\ESC[" ++ show ((height board) + 1) ++ "A") -- Move cursor up
  putStr "\ESC[J" -- Clear terminal from cursor to the end
  loop (genNumber+1) (nextGen board)
