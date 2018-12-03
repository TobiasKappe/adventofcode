import System.IO (isEOF)
import Text.Regex.Posix
import qualified Data.Map

readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine line =
    let numbersWrapped = (line =~ "[0-9]+") :: [[String]]
        numbersAsStrings = map head numbersWrapped
        numbers = map read numbersAsStrings :: [Int]
        [id, x, y, width, height] = numbers
    in (id, x, y, width, height)



cells (id, x, y, width, height) =
    [(i, j) | i <- [x..(x+width-1)], j <- [y..(y+height-1)]]


sweep [] claimed = claimed

sweep (cell : cells) claimed =
    let sweepRest = sweep cells claimed
        sweepNew = Data.Map.insertWith (+) cell 1 sweepRest
    in sweepNew


claimed [] = Data.Map.empty

claimed (box : boxes) =
    let claimedByOthers = claimed boxes
        claimedByThis = cells box
        claimedTotal = sweep claimedByThis claimedByOthers
    in claimedTotal


main = do
    linesAsStrings <- readLines
    let boxes = map parseLine linesAsStrings
        claimedCount = claimed boxes
        overClaimed = Data.Map.filter (> 1) claimedCount
    print $ length $ overClaimed
