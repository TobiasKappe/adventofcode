import Data.Maybe (catMaybes)
import System.IO (isEOF)
import Text.Regex.Posix
import qualified Data.Map
import qualified Data.Set


manhattanDistance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)


totalDistanceTo points point =
    let distances = map (manhattanDistance point) points
    in sum distances


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: String -> (Int, Int)

parseLine line =
    let [[x], [y]] = line =~ "[0-9]+"
    in (read x, read y)


main = do
    lines <- readLines
    let points = map parseLine lines
        width = maximum $ map (fst) points
        height = maximum $ map (snd) points
        cells = [(i, j) | i <- [0..width], j <- [0..height]]
        distances = map (totalDistanceTo points) cells
        region = filter (< 10000) distances
    print $ length region

