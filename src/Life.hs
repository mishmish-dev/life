module Life
    ( Board, width, height, parseBoard, nextGen, minBoardDimension
    , ParseBoardResult(Parsed, UnevenRowLength, BoardTooSmall, BoardEmpty) )
where

import Data.List (intercalate, intersperse, unfoldr)


data Cell = Dead | Alive
    deriving Eq


data Board = Board { width :: Int, cells :: [Cell] }


fmtCell :: Cell -> Char
fmtCell Dead = ' '
fmtCell Alive = '#'


height :: Board -> Int
height Board{width=w, cells=cs} = length cs `quot` w


instance Show Cell where
    show c = [fmtCell c]


instance Show Board where
    show Board{width=width, cells=cells} =
        let 
            maybeSplitAt :: Int -> [a] -> Maybe ([a], [a])
            maybeSplitAt n xs =
                if null xs then Nothing
                else Just (splitAt n xs)

            rows = unfoldr (maybeSplitAt width) $ map fmtCell cells
            wideRows = map (intersperse ' ') rows
            wideRowsWithBorder = map (\r -> "│ " ++ r ++ " │") wideRows

            topEdge = "┌" ++ replicate (2*width+1) '─' ++ "┐"
            bottomEdge = "└" ++ replicate (2*width+1) '─' ++ "┘"
        in
            topEdge ++ "\n" ++ intercalate "\n" wideRowsWithBorder ++ "\n" ++ bottomEdge


data ParseBoardResult =
    Parsed Board
    | UnevenRowLength
    | BoardTooSmall
    | BoardEmpty


minBoardDimension :: Int
minBoardDimension = 3


parseBoard :: String -> ParseBoardResult
parseBoard rawInput =
    let
        parseRow :: String -> [Cell]
        parseRow ('.' : xs) = Dead : parseRow xs
        parseRow ('#' : xs) = Alive : parseRow xs
        parseRow (_ : xs) = parseRow xs
        parseRow [] = []   

        grid :: [[Cell]]
        grid = filter (not . null) (map parseRow (lines rawInput))
    in
        case grid of
            (row : rows) ->
                let width = length row
                    candidate = Board{width=width, cells=concat grid}
                    rowsAreEvenLength = all ((== width) . length) rows
                in
                    if not rowsAreEvenLength then 
                        UnevenRowLength
                    else if width < minBoardDimension || length grid < minBoardDimension then 
                        BoardTooSmall
                    else
                        Parsed candidate
            _ -> BoardEmpty


aliveNborsCount :: Board -> Int -> Int
aliveNborsCount Board{width=width, cells=cells} idx =
    let nborsIndices =
            [ idx-1-width, idx-width, idx+1-width
            , idx-1, idx+1
            , idx-1+width, idx+width, idx+1+width ]
        nborsIndicesWrapped = map (`mod` length cells) nborsIndices
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
