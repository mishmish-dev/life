module Lib (Board, height, parseBoard, nextGen) where
import Data.List (intercalate, unfoldr)

data Cell = Dead | Alive
    deriving Eq

data Board = Board { width :: Int, cells :: [Cell]}

height :: Board -> Int
height Board{width=w, cells=cs} = (length cs) `quot` w

maybeSplitAt :: Int -> [a] -> Maybe ([a], [a])
maybeSplitAt n xs =
    if null xs then Nothing
    else Just (splitAt n xs)

chunk :: Int -> [a] -> [[a]]
chunk n = unfoldr (maybeSplitAt n)

insertBetweenChunks :: Int -> [a] -> [a] -> [a]
insertBetweenChunks n xs ys = intercalate xs (chunk n ys)

fmt :: Cell -> Char
fmt Dead = '.'
fmt Alive = '#'

instance Show Cell where
    show c = [fmt c]

instance Show Board where
    show Board{width=w, cells=s} = insertBetweenChunks w "\n" (map fmt s)

parseRow :: String -> [Cell]
parseRow ('.' : xs) = Dead : parseRow xs
parseRow ('#' : xs) = Alive : parseRow xs
parseRow (_ : xs) = parseRow xs
parseRow [] = []

parseBoard :: String -> Maybe Board
parseBoard s =
    let grid = filter (not . null) (map parseRow (lines s)) in
        case grid of
            (row : rows) -> let width = length row in
                if all ((== width) . length) rows
                    then Just Board{width=width, cells=concat grid}
                    else Nothing
            _ -> Nothing

aliveNborsCount :: Board -> Int -> Int
aliveNborsCount Board{width=w, cells=cells} idx =
    let nborsIndices = [idx-1-w, idx-w, idx+1-w, idx-1, idx+1, idx-1+w, idx+w, idx+1+w] 
        nborsIndicesWrapped = map (`mod` (length cells)) nborsIndices
        nbors = map (cells !!) nborsIndicesWrapped
    in
        length $ filter (== Alive) nbors

nextGen :: Board -> Board
nextGen board@Board{width=width, cells=cells} =
    let nextCellState :: Board -> Int -> Cell
        nextCellState board idx =
            case aliveNborsCount board idx of
                2 -> cells !! idx
                3 -> Alive
                _ -> Dead
        in
            Board{width=width, cells=map (nextCellState board) [0..(length cells)-1]}
